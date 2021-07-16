package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import com.opencsv.CSVWriter;
import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.algorithm.Statistics;
import it.unipi.jenoma.population.Population;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;


/**
 * The Java coordinator of the cluster of machines. The coordinator starts the initialization of the cluster
 * and, together with the Erlang coordinator, manages the lifecycle of the genetic algorithm's execution,
 * ensuring that the population is distributed to workers, the termination condition is correctly checked
 * and the final results are collected.
 */
public class Coordinator {
    /*
     * This logger object is the same used by coordinatorLogger, as granted by calling Logger.getLogger()
     * passing the same name as argument. Concurrent writes on a logger object are thread-safe.
     */
    private final Logger localLogger;
    private final String LOGGER_NAME = "it.unipi.jenoma";
    private final String LOGGER_PROPERTIES = "/logging.properties";
    private final GeneticAlgorithm geneticAlgorithm;
    private OtpNode javaNode;
    private OtpMbox mailBox;
    private CoordinatorLogger coordinatorLogger;
    private ExecutorService loggerExecutor;
    private List<Statistics[]> statisticsList;
    private Statistics coordinatorLastStatistic;

    /**
     * Checks if at least one worker was provided in the configuration file.
     * @return  true if at least one worker was provided in the configuration file, false otherwise.
     */
    private boolean noWorkersRegistered() {
        return geneticAlgorithm.getConfiguration().getWorkers().size() == 0;
    }

    /**
     * Loads the configuration of the logger stored in <code>resources/LOGGER_PROPERTIES</code>.
     * A failure in the loading of the properties does not prevent the execution of the algorithm.
     */
    private void loadLoggerConfiguration() {
        try {
            LogManager.getLogManager().readConfiguration(Coordinator.class.getResourceAsStream(LOGGER_PROPERTIES));
        } catch (IOException e) {
            localLogger.log(Level.INFO, "Failed to load the logger configuration.");
            localLogger.log(Level.FINE, ExceptionUtils.getStackTrace(e));
        }
    }

    /**
     * Sends a given message to the Erlang node.
     * @param msg  the message to send to the Erlang node.
     */
    private void sendToErlangNode(OtpErlangObject msg) {
        mailBox.send(
                ClusterUtils.Process.COORDINATOR,
                ClusterUtils.compose(
                        ClusterUtils.Node.ERLANG,
                        geneticAlgorithm.getConfiguration().getCoordinator()),
                msg);
    }

    /**
     * Starts the EPMD daemon in the current machine.
     * @return  true if the EPMD daemon is started correctly, false otherwise.
     */
    private boolean startEPMD() {
        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(ClusterUtils.getProcessCommandStartEPMD());

        try {
            Process process = processBuilder.start();
            process.waitFor();
        } catch (IOException | InterruptedException e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        return true;
    }

    /**
     * Starts the Java <code>OtpNode</code> in the current machine.
     * @return  true if the Java <code>OtpNode</code> is started correctly, false otherwise.
     */
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

    /**
     * Starts the Erlang node in the current machine.
     * @return  true if the Erlang node is started correctly, false otherwise.
     */
    private boolean startErlangNode() {
        ProcessBuilder processBuilder = new ProcessBuilder();
        processBuilder.command(ClusterUtils.getProcessCommandCommandStartErlangCoordinator(geneticAlgorithm.getConfiguration()));

        try {
            processBuilder.start();
        } catch (IOException e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        // Wait for a heartbeat sent by the spawned Erlang process.
        try {
            OtpErlangAtom msg = (OtpErlangAtom) mailBox.receive(geneticAlgorithm.getConfiguration().getTimeoutSetupCluster());
            if (msg == null || !msg.equals(ClusterUtils.Atom.HEARTBEAT))
                return false;
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        return true;
    }

    /**
     * Starts the coordinator logger node in the current machine.
     * The logger is executed in a separate thread, whose instantiation
     * is managed by an ExecutorService.
     * @return  true if the coordinator logger node is started correctly, false otherwise.
     */
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

    /**
     * Stops the Java <code>OtpNode</code> and deallocates its resources.
     */
    private void stopJavaNode() {
        mailBox.close();
        javaNode.close();
    }

    /**
     * Sends a stop message to the Erlang node.
     * @param phase  the phase of the lifecycle of the Erlang node.
     */
    private void stopErlangNode(OtpErlangAtom phase) {
        sendToErlangNode(new OtpErlangTuple(new OtpErlangObject[]{ phase, ClusterUtils.Atom.STOP }));
    }

    /**
     * Sends a stop message to the coordinator logger node.
     */
    private void stopCoordinatorLogger() {
        mailBox.send(
                ClusterUtils.Process.LOGGER,
                ClusterUtils.compose(ClusterUtils.Node.LOGGER, geneticAlgorithm.getConfiguration().getCoordinator()),
                ClusterUtils.Atom.STOP);
        loggerExecutor.shutdown();
    }

    /**
     * Stops the Java <code>OtpNode</code>, the Erlang node and the coordinator logger node.
     * Utility method to avoid missing some closing steps.
     * @param phase  the phase of the lifecycle of the Erlang node.
     */
    private void stopAllNodes(OtpErlangAtom phase) {
        stopErlangNode(phase);
        stopCoordinatorLogger();
        stopJavaNode();
    }

    /**
     * Starts the Java <code>OtpNode</code>, the Erlang node and the
     * coordinator logger node in the coordinator machine.
     * @return  true if all the nodes are initialized correctly, false otherwise.
     */
    private boolean startCoordinator() {
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
            stopErlangNode(ClusterUtils.Atom.INIT_PHASE);
            stopJavaNode();
            return false;
        }
        localLogger.log(Level.INFO, "Logger started correctly.");

        return true;
    }

    /**
     * Sends the cluster initialization information to the Erlang node,
     * so that the cluster setup can effectively start.
     */
    private void sendClusterInitializationInfo() {
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

        sendToErlangNode(
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.CLUSTER_SETUP_PHASE,
                                initClusterCmd,
                                new OtpErlangList(workers.toArray(new OtpErlangObject[0])),
                                new OtpErlangInt(geneticAlgorithm.getConfiguration().getTimeoutSetupCluster()),
                                new OtpErlangInt(geneticAlgorithm.getConfiguration().getTimeoutWorker()),
                                new OtpErlangInt(geneticAlgorithm.getElitism().getNumberOfIndividuals())
                        }));
    }

    /**
     * Waits until the cluster setup is terminated. The setup termination (or
     * an eventual timeout) is notified by the Erlang node with a message.
     * @return  true if the cluster setup succeeds, false otherwise.
     */
    private boolean waitForClusterReady() {
        OtpErlangAtom msg;

        try {
            msg = (OtpErlangAtom) mailBox.receive();
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        return msg.equals(ClusterUtils.Atom.CLUSTER_READY);
    }

    /**
     * Sends the list of population chunks obtained from the initial population to the Erlang node.
     * @return  true if the population can be correctly divided into chunks, false otherwise
     *          (empty population or less than two individuals per chunk).
     */
    private boolean sendWorkloads() {
        Population population = geneticAlgorithm.getPopulation();
        List<String> workers = geneticAlgorithm.getConfiguration().getWorkers();
        int chunkSize = population.getSize() / workers.size();

        if (population.getSize() == 0) {
            localLogger.log(Level.INFO, "Empty population.");
            return false;
        }

        if (chunkSize < 2) {
            localLogger.log(
                    Level.INFO,
                    "The actual setup assigns less than two individuals per worker. "
                    + "Please decrease the number of workers or increase the population size.");
            return false;
        }

        OtpErlangObject[] workloads = new OtpErlangObject[workers.size()];
        for (int i = 0; i < workers.size(); i++) {
            int endChunkIndex = (i == workers.size() - 1) ? population.getSize() : (i + 1)*chunkSize;

            GeneticAlgorithm workload = new GeneticAlgorithm(
                    geneticAlgorithm,
                    new Population(new ArrayList<>(population.getIndividuals(i*chunkSize, endChunkIndex))));

            workloads[i] = new OtpErlangBinary(workload);
        }

        sendToErlangNode(
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.CLUSTER_SETUP_PHASE,
                                new OtpErlangList(workloads)
                        }));
        return true;
    }

    /**
     * Sends a termination result for the current iteration of the algorithm to the Erlang node.
     * @param terminationResult  the termination result of the iteration.
     */
    private void sendTerminationResult(OtpErlangAtom terminationResult) {
        sendToErlangNode(
                new OtpErlangTuple(
                        new OtpErlangObject[]{
                                ClusterUtils.Atom.GENERATION_END_PHASE,
                                terminationResult
                        }));
    }

    /**
     * Waits until the genetic algorithms terminates or fails, evaluating the termination conditions
     * sent by the workers at each generation to determine if the algorithm must end, and
     * eventually merging the final results to obtain the final population.
     * @return  the final population if the algorithm terminates correctly, an empty population otherwise.
     * @throws OtpErlangDecodeException  if an error occurs while decoding an Erlang message.
     * @throws OtpErlangExit             if the communication channel with the Erlang node fails.
     */
    private Population waitForTermination() throws OtpErlangDecodeException, OtpErlangExit {
        Population finalPopulation = new Population(new ArrayList<>());
        ClusterLogger terminationConditionLogger = log -> localLogger.log(Level.INFO, log);
        OtpErlangObject msg;

        while (true) {
            msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.COMPUTATION_FAILED)) {
                localLogger.log(Level.INFO, "Timeout: at least one worker did not reply.");
                localLogger.log(Level.INFO, "The computation failed. Shutting down.");
                return finalPopulation;
            }

            if (msg instanceof OtpErlangTuple msgTuple && msgTuple.elementAt(0).equals(ClusterUtils.Atom.TERMINATION_CONDITIONS)) {
                List partialConditions = new ArrayList<>();

                for (OtpErlangObject element : (OtpErlangList) msgTuple.elementAt(1))
                    partialConditions.add(((OtpErlangBinary) element).getObject());

                if (geneticAlgorithm.getTerminationCondition().end(partialConditions, terminationConditionLogger)) {
                    localLogger.log(Level.INFO, "Termination condition met. Collecting the population.");
                    sendTerminationResult(ClusterUtils.Atom.ALGORITHM_END);
                } else {
                    localLogger.log(Level.INFO, "Termination condition not met. Starting the next iteration.");
                    sendTerminationResult(ClusterUtils.Atom.ALGORITHM_CONTINUE);
                }
                continue;
            }

            if (msg instanceof OtpErlangTuple msgTuple && msgTuple.elementAt(0).equals(ClusterUtils.Atom.FINAL_POPULATION)) {
                for (OtpErlangObject element : (OtpErlangList) msgTuple.elementAt(1)) {
                    Population populationChunk = (Population) ((OtpErlangBinary) element).getObject();
                    finalPopulation.addIndividuals(populationChunk.getIndividuals(0, populationChunk.getSize()));
                }

                OtpErlangList otpErlangStatisticsList = (OtpErlangList) msgTuple.elementAt(2);

                for (OtpErlangObject workerStatisticObject : otpErlangStatisticsList) {
                    OtpErlangList workerStatisticList = (OtpErlangList) (workerStatisticObject);
                    Statistics[] workerStatisticArray = new Statistics[workerStatisticList.elements().length];
                    for (int i = 0 ; i < workerStatisticArray.length; i++){
                        workerStatisticArray[i] = (Statistics)((OtpErlangBinary)workerStatisticList.elementAt(i)).getObject();
                    }
                    this.statisticsList.add(workerStatisticArray);
                }
                return finalPopulation;
            }
        }
    }

    /**
     * Creates a new <code>Coordinator</code>.
     * @param geneticAlgorithm  the genetic algorithm to be executed on the remote cluster.
     */
    public Coordinator(GeneticAlgorithm geneticAlgorithm) {
        this.geneticAlgorithm = geneticAlgorithm;
        this.localLogger = Logger.getLogger(LOGGER_NAME);
        this.statisticsList = new ArrayList<>();
    }

    /**
     * Starts the execution of the genetic algorithm on the remote cluster.
     * The method is blocking: it returns the final population when the algorithm terminates
     * correctly, or an empty population as soon as an error in the initialization of the cluster
     * or in the execution of the stages occurs.
     * @return  the final population if the algorithm terminates correctly, an empty population otherwise.
     */
    public Population start() {
        Population finalPopulation = new Population(new ArrayList<>());
        loadLoggerConfiguration();

        if (noWorkersRegistered()) {
            localLogger.log(Level.INFO, "No workers specified in the configuration file.");
            localLogger.log(Level.INFO, "Stopping the initialization.");
            return finalPopulation;
        }
        coordinatorLastStatistic = new Statistics();
        coordinatorLastStatistic.communicationTime = System.currentTimeMillis();

        if (!startCoordinator()) {
            localLogger.log(Level.INFO, "Stopping the initialization.");
            return finalPopulation;
        }

        if (!ClusterUtils.createClusterInitScript(geneticAlgorithm.getConfiguration(), localLogger)) {
            localLogger.log(Level.INFO, "Error while creating the cluster initialization script.");
            stopAllNodes(ClusterUtils.Atom.INIT_PHASE);
            return finalPopulation;
        }

        localLogger.log(Level.INFO, "Cluster initialization script created.");
        localLogger.log(Level.INFO, "Initialization performed successfully.");
        localLogger.log(Level.INFO, "Starting the initialization of the cluster.");
        sendClusterInitializationInfo();
        boolean clusterReady = waitForClusterReady();

        if (!clusterReady) {
            localLogger.log(Level.INFO, "Timeout: the cluster failed to start.");
            localLogger.log(Level.INFO, "Stopping the initialization.");
            stopAllNodes(ClusterUtils.Atom.CLUSTER_SETUP_PHASE);
            return finalPopulation;
        }

        localLogger.log(Level.INFO, "Cluster initialized successfully.");
        localLogger.log(Level.INFO, "Sending workloads to workers.");
        boolean workloadsSent = sendWorkloads();
        coordinatorLastStatistic.communicationTime = System.currentTimeMillis() - coordinatorLastStatistic.communicationTime;
        if (!workloadsSent) {
            localLogger.log(Level.INFO, "Failed to send the workloads.");
            localLogger.log(Level.INFO, "Stopping the initialization.");
            stopAllNodes(ClusterUtils.Atom.CLUSTER_SETUP_PHASE);
            return finalPopulation;
        }

        localLogger.log(Level.INFO, "Workloads sent successfully.");

        try {
            finalPopulation = waitForTermination();
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
        }

        stopAllNodes(ClusterUtils.Atom.SHUTDOWN_PHASE);
        try {
            saveStatistics();
        } catch (IOException e) {
            localLogger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
        }

        return finalPopulation;

    }

    /**
     * Stops the execution of the genetic algorithm, asks for the deallocation of
     * the cluster resources and stops the coordinator.
     */
    public void stop() {
        // Check if the coordinator is running or not.
        if (javaNode == null)
            return;

        localLogger.log(
                Level.INFO,
                "Call to stop(): aborting the computation and forcing the deallocation of the cluster resources.");

        // Unblock the coordinator if it is waiting on a receive().
        mailBox.send(mailBox.self(), ClusterUtils.Atom.CLUSTER_TIMEOUT);
        mailBox.send(mailBox.self(), ClusterUtils.Atom.COMPUTATION_FAILED);

        stopAllNodes(ClusterUtils.Atom.SHUTDOWN_PHASE);
    }

    public void saveStatistics() throws IOException{
        File file = new File("./statistics.csv");
        FileWriter outputfile = new FileWriter(file);
        CSVWriter writer = new CSVWriter(outputfile);

        // adding header to csv
        String[] header = { "Id","ComputationTime", "CommunicationTime", "fitnesses" };
        writer.writeNext(header);

        for (Statistics[] arr : this.statisticsList) {

            if(arr.length == 0){
                writer.close();
                return;
            }
            double[] workingtTimeArr = new double[arr.length];
            double[] communicationTimeArr = new double[arr.length];
            double[] fitnessArr = new double[arr.length];
            String id = arr[0].workerID;
            for(int j = 0; j < arr.length; j++){
                workingtTimeArr[j] = arr[j].computationTime;
                communicationTimeArr[j] = arr[j].communicationTime;
                fitnessArr[j] = arr[j].fittestIndividual.getFitness();
            }
            String[] newLine = {id,
                                Arrays.toString(workingtTimeArr),
                                Arrays.toString(communicationTimeArr),
                                Arrays.toString(fitnessArr)};
            writer.writeNext(newLine);

        }
        writer.close();

    }
}
