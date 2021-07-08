package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.utils.Configuration;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;


public class Coordinator {
    /*
     * This logger object is the same used by coordinatorLogger, as granted by calling Logger.getLogger()
     * passing the same name as argument, but concurrent writes on a logger object are thread-safe.
     */
    private final Logger localLogger;
    private final String LOGGER_NAME = "it.unipi.jenoma";
    private final String LOGGER_PROPERTIES = "/logging.properties";
    private final GeneticAlgorithm geneticAlgorithm;
    private OtpNode javaNode;
    private OtpMbox mailBox;
    private CoordinatorLogger coordinatorLogger;
    private ExecutorService loggerExecutor;


    private void loadLoggerConfiguration() {
        try {
            LogManager.getLogManager().readConfiguration(Coordinator.class.getResourceAsStream(LOGGER_PROPERTIES));
        } catch (IOException e) {
            localLogger.log(Level.INFO, "Failed to load the logger configuration.");
            localLogger.log(Level.FINE, ExceptionUtils.getStackTrace(e));
        }
    }

    private boolean startEPMD() {
        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(ClusterUtils.getShellCommandStartEPMD());

        try {
            Process process = processBuilder.start();
            process.waitFor();
        } catch (IOException | InterruptedException e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        return true;
    }

    private boolean startJavaNode() {
        try {
            javaNode = new OtpNode(
                    ClusterUtils.compose(
                            ClusterUtils.Node.JAVA,
                            geneticAlgorithm.getConfiguration().getCoordinator()));
        } catch (IOException e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        javaNode.setCookie(ClusterUtils.Cluster.COOKIE);
        mailBox = javaNode.createMbox(ClusterUtils.Process.COORDINATOR);
        return true;
    }

    private boolean startErlangNode() {
        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(ClusterUtils.getShellCommandStartErlangCoordinator(geneticAlgorithm.getConfiguration()));

        try {
            processBuilder.start();
        } catch (IOException e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        // Wait for a heartbeat sent by the spawned Erlang process.
        try {
            OtpErlangAtom msg = (OtpErlangAtom) mailBox.receive(geneticAlgorithm.getConfiguration().getTimeoutWorker());

            if (msg == null || !msg.equals(new OtpErlangAtom("heartbeat")))
                return false;
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        return true;
    }

    private boolean startCoordinatorLogger() {
        try {
            coordinatorLogger = new CoordinatorLogger(
                    geneticAlgorithm.getConfiguration().getCoordinator(),
                    LOGGER_NAME);
        } catch (IOException e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        loggerExecutor = Executors.newFixedThreadPool(1);
        loggerExecutor.submit(coordinatorLogger);
        return true;
    }

    private void stopJavaNode() {
        mailBox.close();
        javaNode.close();
    }

    private void stopErlangNode() {
        mailBox.send(
                ClusterUtils.Process.COORDINATOR,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, geneticAlgorithm.getConfiguration().getCoordinator()),
                new OtpErlangAtom("stop"));
    }

    private void stopCoordinatorLogger() {
        mailBox.send(
                ClusterUtils.Process.LOGGER,
                ClusterUtils.compose(ClusterUtils.Node.LOGGER, geneticAlgorithm.getConfiguration().getCoordinator()),
                new OtpErlangAtom("stop"));
        loggerExecutor.shutdown();
    }

    private boolean startCoordinator() {
        loadLoggerConfiguration();
        localLogger.log(Level.INFO, "Initializing the required processes.");

        if (!startEPMD()) {
            localLogger.log(Level.INFO, "EPMD daemon failed to start.");
            return false;
        }
        localLogger.log(Level.INFO, "EPMD daemon started correctly.");

        if (!startJavaNode()) {
            localLogger.log(Level.INFO, "Java node failed to start.");
            return false;
        }
        localLogger.log(Level.INFO, "Java node started correctly.");

        if (!startErlangNode()) {
            localLogger.log(Level.INFO, "Erlang node failed to start.");
            stopJavaNode();
            return false;
        }
        localLogger.log(Level.INFO, "Erlang node started correctly.");

        if (!startCoordinatorLogger()) {
            localLogger.log(Level.INFO, "Logger failed to start.");
            stopErlangNode();
            stopJavaNode();
            return false;
        }
        localLogger.log(Level.INFO, "Logger started correctly.");

        if (!ClusterUtils.createClusterInitScript(geneticAlgorithm.getConfiguration(), localLogger)) {
            localLogger.log(Level.INFO, "Error while creating the cluster initialization script.");
            stopErlangNode();
            stopJavaNode();
            stopCoordinatorLogger();
            return false;
        }

        localLogger.log(Level.INFO, "Cluster initialization script created.");
        return true;
    }

    private void initializeCluster() {
        OtpErlangString initClusterCmd = new OtpErlangString(
                String.join(" ", ClusterUtils.getShellCommandClusterInit()));

        List<OtpErlangObject> workers = new ArrayList<>();
        for (String worker : geneticAlgorithm.getConfiguration().getWorkers())
            workers.add(
                    new OtpErlangTuple(
                            new OtpErlangObject[] {
                                    new OtpErlangAtom(ClusterUtils.Process.WORKER),
                                    new OtpErlangAtom(ClusterUtils.compose(ClusterUtils.Node.ERLANG, worker))
                            }));

        OtpErlangTuple msg = new OtpErlangTuple(
                new OtpErlangObject[] {
                        initClusterCmd,
                        new OtpErlangList(workers.toArray(new OtpErlangObject[0])),
                        new OtpErlangInt(geneticAlgorithm.getConfiguration().getTimeoutWorker())
        });

        mailBox.send(
                ClusterUtils.Process.COORDINATOR,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, geneticAlgorithm.getConfiguration().getCoordinator()),
                msg);
    }

    public Coordinator(GeneticAlgorithm geneticAlgorithm) {
        this.geneticAlgorithm = geneticAlgorithm;
        this.localLogger = Logger.getLogger(LOGGER_NAME);
    }

    public void start() {
        //TODO: check if GeneticAlgorithm has all fields set (not null).

        if (!startCoordinator()) {
            localLogger.log(Level.INFO, "Stopping the initialization.");
            return;
        }

        localLogger.log(Level.INFO, "Initialization performed successfully.");
        localLogger.log(Level.INFO, "Starting the initialization of the cluster.");
        initializeCluster();


        OtpErlangAtom msg;
        while(true) {
            try {
                 msg = (OtpErlangAtom) mailBox.receive();
            } catch (OtpErlangDecodeException | OtpErlangExit e) {
                localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
                return;
            }

            if (msg.equals(new OtpErlangAtom("end_computation"))) {
                stopJavaNode();
                stopCoordinatorLogger();
                return;
            }
        }

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
