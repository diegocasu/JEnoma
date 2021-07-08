package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;

import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;


class Worker {
    private final String host;
    private final String loggerCoordinatorHost;
    private WorkerLogger workerLogger;
    private OtpNode javaNode;
    private OtpMbox mailBox;

    public Worker(String host, String loggerCoordinatorHost) {
        this.host = host;
        this.loggerCoordinatorHost = loggerCoordinatorHost;
    }

    private boolean startJavaNode() {
        try {
            javaNode = new OtpNode(ClusterUtils.compose(ClusterUtils.Node.JAVA, host));
        } catch (IOException e) {
            //TODO: add error log
            e.printStackTrace();
            return false;
        }

        javaNode.setCookie(ClusterUtils.Cluster.COOKIE);
        mailBox = javaNode.createMbox(ClusterUtils.Process.WORKER);
        return true;
    }

    private boolean startWorkerLogger() {
        try {
            workerLogger = new WorkerLogger(
                    host,
                    loggerCoordinatorHost,
                    ClusterUtils.compose(ClusterUtils.Node.JAVA, host));
        } catch (IOException e) {
            return false;
        }

        return true;
    }

    private void stopJavaNode() {
        mailBox.close();
        javaNode.close();
    }

    private void sendHeartbeatToErlangNode() {
        mailBox.send(
                ClusterUtils.Process.WORKER,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, host),
                new OtpErlangAtom("heartbeat"));
    }

    private void receiveLoop() throws OtpErlangDecodeException, OtpErlangExit {
        OtpErlangAtom stopAtom = new OtpErlangAtom("stop");

        while (true) {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(stopAtom)) {
                workerLogger.log("Stopping the execution.");
                stopJavaNode();
                return;
            }

            if (msg instanceof OtpErlangBinary msgBinary) {
                GeneticAlgorithm geneticAlgorithm = (GeneticAlgorithm) msgBinary.getObject();
                workerLogger.log("Received workload.");
                workerLogger.log(geneticAlgorithm.getPopulation().toString());
            }

            //TODO: add missing messages.
        }
    }

    public void start() {
        if (!startWorkerLogger())
            return;

        if (!startJavaNode()) {
            workerLogger.log("Java node failed to start.");
            workerLogger.stop();
            return;
        }

        workerLogger.log("Java node started correctly.");
        sendHeartbeatToErlangNode();

        try {
            receiveLoop();
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopJavaNode();
        }
    }

    public static void main(String[] args) {
        String thisHost = args[0];
        String loggerCoordinatorHost = args[1];

        Worker worker = new Worker(thisHost, loggerCoordinatorHost);
        worker.start();
    }
}
