package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.utils.Configuration;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


public class Coordinator {
    private final GeneticAlgorithm geneticAlgorithm;
    private OtpNode javaNode;
    private OtpMbox mailBox;


    private boolean startEPMD() {
        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(ClusterUtils.getShellCommandStartEPMD());

        try {
            Process process = processBuilder.start();
            process.waitFor();
        } catch (IOException | InterruptedException e) {
            //TODO: add error log
            e.printStackTrace();
            return false;
        }

        return true;
    }

    private boolean startJavaNode() {
        try {
            javaNode = new OtpNode(ClusterUtils.compose(ClusterUtils.Node.JAVA, ClusterUtils.Host.COORDINATOR));
        } catch (IOException e) {
            //TODO: add error log
            e.printStackTrace();
            return false;
        }

        javaNode.setCookie(ClusterUtils.Cluster.COOKIE);
        mailBox = javaNode.createMbox(ClusterUtils.Process.COORDINATOR);
        return true;
    }

    private boolean startErlangNode() {
        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(ClusterUtils.getShellCommandStartErlangCoordinator());

        try {
            processBuilder.start();
        } catch (IOException e) {
            //TODO: add error log
            e.printStackTrace();
            return false;
        }

        // Wait for a heartbeat sent by the spawned Erlang process.
        try {
            OtpErlangAtom msg = (OtpErlangAtom) mailBox.receive(10000);

            if (msg == null || !msg.equals(new OtpErlangAtom("heartbeat"))) {
                // TODO: add error log.
                return false;
            }
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
            return false;
        }

        return true;
    }

    private void closeJavaNode() {
        mailBox.close();
        javaNode.close();
    }

    private void initializeCluster() {
        OtpErlangString initClusterCmd = new OtpErlangString(
                String.join(" ", ClusterUtils.getShellCommandClusterInit()));

        List<OtpErlangObject> workers = new ArrayList<>();
        for (String worker : geneticAlgorithm.getConfiguration().getWorkers())
            workers.add(new OtpErlangAtom(worker));

        OtpErlangTuple msg = new OtpErlangTuple(
                new OtpErlangObject[] {
                        initClusterCmd,
                        new OtpErlangList(workers.toArray(new OtpErlangObject[0]))
        });

        mailBox.send(
                ClusterUtils.Process.COORDINATOR,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, ClusterUtils.Host.COORDINATOR),
                msg);
    }

    public Coordinator(GeneticAlgorithm geneticAlgorithm) {
        this.geneticAlgorithm = geneticAlgorithm;
    }

    public void start() {
        //TODO: check if GeneticAlgorithm has all fields set (not null).

        if (!startEPMD() || !startJavaNode())
            return;

        if (!startErlangNode()) {
            closeJavaNode();
            return;
        }

        if (!ClusterUtils.createClusterInitScript(geneticAlgorithm.getConfiguration())) {
            closeJavaNode();
            //TODO: closeErlangNode()
            return;
        }

        initializeCluster();

        //TODO: add error management with disposal of spawned processes
        //TODO: remember to close the node and the mailbox at the end of the computation
    }

    // TODO: remove test
    public static void main(String[] args) throws IOException {
        Configuration conf = new Configuration("configuration.json");
        Coordinator c = new Coordinator(new GeneticAlgorithm().setConfiguration(conf));
        c.start();
    }
}
