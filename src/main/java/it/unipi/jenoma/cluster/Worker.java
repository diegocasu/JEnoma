package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import java.io.IOException;


class Worker {
    private OtpNode javaNode;
    private OtpMbox mailBox;
    private String host;

    public Worker(String host) {
        this.host = host;
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

    private void mainLoop() throws OtpErlangDecodeException, OtpErlangExit {
        OtpErlangAtom stopAtom = new OtpErlangAtom("stop");

        while (true) {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(stopAtom)) {
                stopJavaNode();
                return;
            }
            //TODO: add missing messages.
        }
    }

    public void start() {
        if (!startJavaNode())
            return;

        sendHeartbeatToErlangNode();

        try {
            mainLoop();
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
            stopJavaNode();
        }
    }

    public static void main(String[] args) {
        String erlangNodeName = args[0];
        String host = erlangNodeName.split("@")[1];

        Worker worker = new Worker(host);
        worker.start();
    }
}
