package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import it.unipi.jenoma.algorithm.GeneticAlgorithm;
import it.unipi.jenoma.operator.Crossover;
import it.unipi.jenoma.operator.Elitism;
import it.unipi.jenoma.operator.Evaluation;
import it.unipi.jenoma.operator.Mutation;
import it.unipi.jenoma.operator.TerminationCondition;
import it.unipi.jenoma.population.Individual;
import it.unipi.jenoma.population.Population;
import it.unipi.jenoma.utils.PRNG;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;


/**
 * A Java worker in the cluster of machines, able to execute a genetic algorithm on a portion
 * of the overall population. A worker executes the evaluation, crossover and mutation stages with
 * <code>numberOfThreads = Runtime.getRuntime().availableProcessors()</code> threads,
 * while it executes the selection stage with a single thread.
 * It communicates with the Erlang node on the same machine to perform
 * in a distributed way the elitism, shuffle and termination stages.
 */
class Worker {
    private final String host;
    private final String loggerCoordinatorHost;
    private final ExecutorService executorService;
    private final int numberOfThreads;
    private WorkerLogger workerLogger;
    private OtpNode javaNode;
    private OtpMbox mailBox;


    /**
     * Starts the Java <code>OtpNode</code> in the current machine.
     * @return  true if the Java <code>OtpNode</code> is started correctly, false otherwise.
     */
    private boolean startJavaNode() {
        try {
            javaNode = new OtpNode(ClusterUtils.compose(ClusterUtils.Node.JAVA, host));
        } catch (IOException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            return false;
        }

        javaNode.setCookie(ClusterUtils.Cluster.COOKIE);
        mailBox = javaNode.createMbox(ClusterUtils.Process.WORKER);
        return true;
    }

    /**
     * Starts the worker logger node in the current machine. The worker logger is able
     * to redirect log messages to the coordinator logger running in the coordinator machine.
     * @return  true if the worker logger node is started correctly, false otherwise.
     */
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

    /**
     * Stops the Java <code>OtpNode</code> and deallocates its resources.
     */
    private void stopJavaNode() {
        executorService.shutdown();
        mailBox.close();
        javaNode.close();
    }

    /**
     * Stops the worker logger and deallocates its resources.
     */
    private void stopWorkerLogger() {
        workerLogger.stop();
    }

    /**
     * Stops the Java <code>OtpNode</code> and the worker logger.
     * Utility method to avoid missing some closing steps.
     */
    private void stopAllNodes() {
        stopWorkerLogger();
        stopJavaNode();
    }

    /**
     * Sends a given message to the Erlang node.
     * @param msg  the message to send to the Erlang node.
     */
    private void sendToErlangNode(OtpErlangObject msg) {
        mailBox.send(
                ClusterUtils.Process.WORKER,
                ClusterUtils.compose(ClusterUtils.Node.ERLANG, host),
                msg);
    }

    /**
     * Creates one main PRNG for generating values inside the main thread, and
     * <code>numberOfThreads</code> different PRNGs to do the same inside the working threads.
     * The PRNGs are seeded differently starting from a base seed, as specified in the
     * documentation of <code>PRNG</code>.
     * @param seed  the base seed used to derive the seeds for the PRNGs.
     * @return      a pair holding a main PRNG and the list of <code>numberOfThreads</code>
     *              PRNGs for the working threads.
     */
    private Pair<PRNG, List<PRNG>> createPRNGs(int seed) {
        int hostSeed = Objects.hash(ClusterUtils.compose(ClusterUtils.Node.JAVA, host), seed);

        PRNG mainGenerator = new PRNG(hostSeed);
        List<PRNG> threadGenerators = new ArrayList<>();

        for (int i = 1; i <= numberOfThreads; i++)
            threadGenerators.add(new PRNG(hostSeed + i));

        return new ImmutablePair<>(mainGenerator, threadGenerators);
    }

    /**
     * Executes the evaluation stage with multiple threads, assigning an equal portion of the population
     * to each of them. The threads work on different subsets of the population, so no particular
     * synchronization patterns are required.
     * @param population  the population to be evaluated.
     * @param evaluation  the <code>Evaluation</code> operator to be applied.
     * @return            true if the evaluation stage is completed successfully, false otherwise.
     */
    private boolean evaluate(Population population, Evaluation evaluation) {
        int chunkSize = population.getSize()/numberOfThreads;
        int threadsToSpawn = chunkSize > 0 ? numberOfThreads : 1;
        List<Callable<Void>> evaluationTasks = new ArrayList<>();

        for (int i = 0; i < threadsToSpawn; i++) {
            int startChunkIndex = i*chunkSize;
            int endChunkIndex = (i == threadsToSpawn - 1) ? population.getSize() : (i + 1)*chunkSize;

            evaluationTasks.add(() -> {
                for (Individual individual : population.getIndividuals(startChunkIndex, endChunkIndex)) {
                    individual.setFitness(evaluation.evaluate(individual, workerLogger));
                }
                return null;
            });
        }

        try {
            List<Future<Void>> results = executorService.invokeAll(evaluationTasks);
            for (Future<Void> result : results)
                result.get();
        } catch (InterruptedException | ExecutionException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            return false;
        }

        // Safety check, since a reference to the population is passed to the user-defined evaluation operator.
        return population != null && population.getSize() != 0;
    }

    /**
     * Executes the crossover stage with multiple threads, so that random individuals are recombined
     * until the overall offspring is as large as the given size (in this case, the original population
     * size). Each thread performs a certain number of crossover operations between randomly chosen
     * individual: since a crossover operation is expected to not modify the involved parents,
     * the threads operate on individuals without synchronization patterns; the only synchronization
     * is performed at the end to generate the final offspring.<br>
     * Supposing that each crossover generates two children, the default number of overall crossover
     * operations per execution round is chosen to be <code>offspringSize/2</code>;
     * the number of operations done by a thread is chosen accordingly.
     * However, since the user is free to return one or more than
     * two individuals, the size of the offspring is checked, so that:<br>
     * 1) additional execution rounds are scheduled if <code>offspringSize</code> is not reached;<br>
     * 2) the offspring is shrank if its size exceeds <code>offspringSize</code>.
     * @param matingPool        the population of individuals from which parents are drawn.
     * @param offspringSize     the size of the final offspring.
     * @param crossover         the crossover operator.
     * @param threadGenerators  the list of <code>numberOfThreads</code> PRNGs
     *                          to be assigned to the worker threads.
     * @return                  null if an error occurred while executing the threads,
     *                          the generated offspring otherwise.
     */
    private Population cross(Population matingPool, int offspringSize, Crossover crossover, List<PRNG> threadGenerators) {
        Population offspring = new Population(new ArrayList<>());
        List<Callable<List<Individual>>> crossoverTasks = new ArrayList<>();

        while (true) {
            for (int i = 0; i < numberOfThreads; i++) {
                final PRNG prng = threadGenerators.get(i);

                crossoverTasks.add(() -> {
                    List<Individual> partialOffspring = new ArrayList<>();
                    int maxAttempts = Math.max(1, offspringSize/(2*numberOfThreads) + (offspringSize%(2*numberOfThreads) == 0 ? 0 : 1));
                    int counter = 0;

                    while (counter < maxAttempts) {
                        int firstParent = prng.nextInt(matingPool.getSize());
                        int secondParent = prng.nextInt(matingPool.getSize());

                        if (firstParent != secondParent) {
                            List<Individual> children = crossover.cross(matingPool.getIndividual(firstParent),
                                                                        matingPool.getIndividual(secondParent),
                                                                        prng, workerLogger);
                            partialOffspring.addAll(children);
                            counter++;
                        }
                    }
                    return partialOffspring;
                });
            }

            try {
                for (Future<List<Individual>> result : executorService.invokeAll(crossoverTasks)) {
                    List<Individual> taskResult = result.get();
                    if (taskResult != null && taskResult.size() != 0)
                        offspring.addIndividuals(taskResult);
                }

                if (offspring.getSize() == offspringSize)
                    return offspring;

                if (offspring.getSize() > offspringSize) {
                    offspring.removeIndividuals(offspringSize, offspring.getSize());
                    return offspring;
                }
            } catch (InterruptedException | ExecutionException e) {
                workerLogger.log(ExceptionUtils.getStackTrace(e));
                return null;
            }
        }
    }

    /**
     * Executes the mutation stage with multiple threads, assigning an equal portion of the population
     * to each of them. The threads work on different subsets of the population, so no particular
     * synchronization patterns are required.
     * @param offspring         the population to be mutated.
     * @param mutation          the mutation operator.
     * @param threadGenerators  the list of <code>numberOfThreads</code> PRNGs
     *                          to be assigned to the worker threads.
     * @return                  true if the evaluation stage is completed successfully,
     *                          false otherwise.
     */
    private boolean mutate(Population offspring, Mutation mutation, List<PRNG> threadGenerators) {
        int chunkSize = offspring.getSize()/numberOfThreads;
        int threadsToSpawn = chunkSize > 0 ? numberOfThreads : 1;
        List<Callable<Void>> mutationTasks = new ArrayList<>();

        for (int i = 0; i < threadsToSpawn; i++) {
            int startChunkIndex = i*chunkSize;
            int endChunkIndex = (i == threadsToSpawn - 1) ? offspring.getSize() : (i + 1)*chunkSize;
            final PRNG prng = threadGenerators.get(i);

            mutationTasks.add(() -> {
                for (Individual individual : offspring.getIndividuals(startChunkIndex, endChunkIndex)) {
                    mutation.mutate(individual, prng, workerLogger);
                }
                return null;
            });
        }

        try {
            List<Future<Void>> results = executorService.invokeAll(mutationTasks);
            for (Future<Void> result : results)
                result.get();
        } catch (InterruptedException | ExecutionException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            return false;
        }

        return true;
    }

    /**
     * Sends the best individuals of the previous generation and the worst individuals
     * of the current one to the Erlang node, so that the distributed elitism stage can be executed.
     * @param elite  the best individuals of the previous generation.
     * @param worst  the worst individuals of the current generation.
     */
    private void sendIndividualsForElitism(List<Individual> elite, List<Individual> worst) {
        // Both "elite" and "worst have the same number of elements by construction.
        int numberOfCandidates = elite.size();

        OtpErlangObject[] eliteTuplesList = new OtpErlangObject[numberOfCandidates];
        OtpErlangObject[] worstTuplesList = new OtpErlangObject[numberOfCandidates];

        for (int i = 0; i < numberOfCandidates; i++) {
            eliteTuplesList[i] = new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangBinary(elite.get(i)),
                            new OtpErlangDouble(elite.get(i).getFitness()),
                    });

            worstTuplesList[i] = new OtpErlangTuple(
                    new OtpErlangObject[] {
                            new OtpErlangDouble(worst.get(i).getFitness()),
                            new OtpErlangInt(i)
                    });
        }

        sendToErlangNode(
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.ELITISM_PHASE,
                                new OtpErlangList(eliteTuplesList),
                                new OtpErlangList(worstTuplesList)
                        }));
    }

    /**
     * Receives a subset of the best individuals of the previous generation and uses them
     * to replace a subset of the worst individuals of the current generation. If the current
     * worker is not involved in the substitutions, it simply waits for the elitism stage to end.
     * If it is involved, it receives the indexes of the worst individuals to be substituted along
     * with the substituting individuals.
     * @param offspring  the current generation of individuals to be modified.
     */
    private void receiveIndividualsForElitism(Population offspring) {
        try {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("The elitism stage failed.");
                workerLogger.log("Stopping the execution.");
                stopAllNodes();
                System.exit(0);
            }

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.NOT_INVOLVED_ELITISM)) {
                workerLogger.log("Not involved in the elitism stage.");
                return;
            }

            if (msg instanceof OtpErlangList msgList) {
                workerLogger.log(String.format("Involved in the elitism stage. Received %s individuals.", msgList.arity()));
                for (OtpErlangObject element : msgList) {
                    OtpErlangTuple individualTuple = (OtpErlangTuple) element;
                    Individual newIndividual = (Individual) ((OtpErlangBinary) individualTuple.elementAt(0)).getObject();
                    int indexToReplace = ((OtpErlangLong) individualTuple.elementAt(1)).intValue();

                    offspring.setIndividual(indexToReplace, newIndividual);
                }
            }
        } catch (OtpErlangExit | OtpErlangDecodeException | OtpErlangRangeException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopAllNodes();
            System.exit(0);
        }
    }

    /**
     * Applies the elitism strategy to the current population. The method sends the
     * best individuals of the previous generation and the worst ones of the current generation
     * to the Erlang node, so that the distributed elitism stage can be executed. Then, if involved
     * in the replacement, it receives the individuals to insert into the current generation and
     * the relative instructions about the substitution.<br>
     * If <i>N</i> is the number of individuals specified by the elitism operator, each worker sends
     * its best and worst <i>N</i> individuals to the coordinator, since a single worker
     * could potentially hold all the best and worst individuals at the same time.
     * @param original   the previous generation of individuals, i.e. the one that generated the offspring.
     * @param offspring  the current generation of individuals.
     * @param elitism    the elitism operator.
     */
    private void applyElitism(Population original, Population offspring, Elitism elitism) {
        int numberOfCandidates = Math.min(elitism.getNumberOfIndividuals(), original.getSize());
        original.sortByDescendingFitness();
        offspring.sortByAscendingFitness();

        List<Individual> elite = original.getIndividuals(0, numberOfCandidates);
        List<Individual> worst = offspring.getIndividuals(0, numberOfCandidates);

        sendIndividualsForElitism(elite, worst);
        receiveIndividualsForElitism(offspring);
    }

    /**
     * Checks if the termination condition of the algorithm is satisfied. The worker computes in isolation
     * the <code>map</code> method specified by the termination condition and sends the result
     * to the coordinator. Then, it waits for the coordinator to establish if the algorithm
     * has ended or not.
     * @param offspring           the current generations of individuals.
     * @param generationsElapsed  the number of generations elapsed so far.
     * @param termination         the termination condition operator.
     * @return                    true if the termination condition is satisfied, false otherwise.
     */
    private boolean endAlgorithm(Population offspring, int generationsElapsed, TerminationCondition<?> termination) {
        sendToErlangNode(
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.GENERATION_END_PHASE,
                                new OtpErlangBinary(termination.map(offspring, generationsElapsed, workerLogger))
                        }));

        try {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("The termination condition check failed.");
                workerLogger.log("Stopping the execution.");
                stopAllNodes();
                System.exit(0);
            }

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.ALGORITHM_CONTINUE))
                return false;

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.ALGORITHM_END)) {
                workerLogger.log("Sending the population chunk.");
                sendToErlangNode(
                        new OtpErlangTuple(
                                new OtpErlangObject[] {
                                        ClusterUtils.Atom.RESULT_COLLECTION_PHASE,
                                        new OtpErlangBinary(offspring)
                                }));
                return true;
            }
        } catch (OtpErlangExit | OtpErlangDecodeException e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopAllNodes();
            System.exit(0);
        }

        return true;
    }

    /**
     * Shuffles the population of individuals. The worker sends the local portion of the population
     * to the Erlang node and waits until the shuffled population is received. The shuffle is distributed,
     * so the individuals composing the shuffled population are obtained from the global one, i.e. they
     * are received from different machines of the cluster.
     * @param population  the population to be shuffled.
     * @return            the shuffled population.
     */
    private Population shuffle(Population population) {
        OtpErlangBinary[] individuals = new OtpErlangBinary[population.getSize()];
        for (int i = 0; i < population.getSize(); i++)
            individuals[i] = new OtpErlangBinary(population.getIndividual(i));

        sendToErlangNode(
                new OtpErlangTuple(
                        new OtpErlangObject[] {
                                ClusterUtils.Atom.SHUFFLE_PHASE,
                                new OtpErlangList(individuals)
                        }));

        List<Individual> shuffledIndividuals = new ArrayList<>();

        try {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("The shuffle stage failed.");
                workerLogger.log("Stopping the execution.");
                stopAllNodes();
                System.exit(0);
            }

            if (msg instanceof OtpErlangList msgList) {
                for (OtpErlangObject individual : msgList)
                    shuffledIndividuals.add((Individual) ((OtpErlangBinary) individual).getObject());
            }
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopAllNodes();
            System.exit(0);
        }

        return new Population(shuffledIndividuals);
    }

    /**
     * Executes the genetic algorithm, performing the selection, crossover, mutation, evaluation and
     * elitism stages, and checking the termination condition. If a new iteration must be executed,
     * a shuffling of the overall population is performed.
     * @param geneticAlgorithm  the genetic algorithm to be executed.
     */
    private void execute(GeneticAlgorithm geneticAlgorithm) {
        Pair<PRNG, List<PRNG>> prngs = createPRNGs(geneticAlgorithm.getConfiguration().getSeed());
        PRNG mainGenerator = prngs.getLeft();
        List<PRNG> threadGenerators = prngs.getRight();

        while (true) { // Algorithm loop.
            Population workingPopulation = geneticAlgorithm.getPopulation().clone();

            Population matingPool = geneticAlgorithm.getSelection().select(workingPopulation, mainGenerator, workerLogger);
            if (matingPool == null || matingPool.getSize() < 2) {
                workerLogger.log("The selection stage failed. Less than two individuals selected.");
                return;
            }
            workerLogger.log("The selection stage succeeded.");

            Population offspring = cross(matingPool, geneticAlgorithm.getPopulation().getSize(), geneticAlgorithm.getCrossover(), threadGenerators);
            if (offspring == null) {
                workerLogger.log("The crossover stage failed.");
                return;
            }
            workerLogger.log("The crossover stage succeeded.");

            if (!mutate(offspring, geneticAlgorithm.getMutation(), threadGenerators)) {
                workerLogger.log("The mutation stage failed.");
                return;
            }
            workerLogger.log("The mutation stage succeeded.");

            if (!evaluate(offspring, geneticAlgorithm.getEvaluation())) {
                workerLogger.log("The evaluation stage failed.");
                return;
            }
            workerLogger.log("The evaluation stage succeeded.");

            if (geneticAlgorithm.getElitism().getNumberOfIndividuals() > 0) {
                applyElitism(geneticAlgorithm.getPopulation(), offspring, geneticAlgorithm.getElitism());
                workerLogger.log("The elitism stage succeeded.");
            } else
                workerLogger.log("Elitism stage skipped.");

            geneticAlgorithm.incrementGenerationsElapsed();
            workerLogger.log(String.format("Reached generation %s.", geneticAlgorithm.getGenerationsElapsed()));

            if (endAlgorithm(offspring, geneticAlgorithm.getGenerationsElapsed(), geneticAlgorithm.getTerminationCondition()))
                return;

            Population shuffledPopulation = shuffle(offspring);
            workerLogger.log("The shuffling stage succeeded.");
            geneticAlgorithm.setPopulation(shuffledPopulation);
        }
    }

    /**
     * Waits until a message is received, executing the appropriate actions: if a genetic algorithm
     * (workload) is received, the execution of the stages is started; if a stop command is received,
     * the node is shut down. If the execution of a stage in the genetic algorithm fails, the worker
     * either suspends its operations and waits to be stopped by an external message.
     * @throws OtpErlangDecodeException  if an error occurs while decoding a message.
     * @throws OtpErlangExit             if the Java <code>OtpNode</code> fails.
     */
    private void receiveLoop() throws OtpErlangDecodeException, OtpErlangExit {
        boolean waitForStop = false;

        while (true) {
            OtpErlangObject msg = mailBox.receive();

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                workerLogger.log("Stopping the execution.");
                stopAllNodes();
                return;
            }

            if (msg instanceof OtpErlangBinary msgBinary && !waitForStop) {
                GeneticAlgorithm geneticAlgorithm = (GeneticAlgorithm) msgBinary.getObject();
                workerLogger.log("Received workload.");

                // Start algorithm, evaluating the initial population.
                workerLogger.log(String.format("Starting the execution of the algorithm, exploiting %s threads.", numberOfThreads));
                workerLogger.log("Starting the evaluation of the initial population.");

                if (!evaluate(geneticAlgorithm.getPopulation(), geneticAlgorithm.getEvaluation())) {
                    workerLogger.log("The evaluation stage failed.");
                    workerLogger.log("Waiting for a stop command.");
                    waitForStop = true;
                    continue;
                }

                execute(geneticAlgorithm);
                waitForStop = true;
                workerLogger.log("Waiting for a stop command.");
            }
        }
    }

    public Worker(String host, String loggerCoordinatorHost) {
        this.host = host;
        this.loggerCoordinatorHost = loggerCoordinatorHost;
        this.numberOfThreads = Runtime.getRuntime().availableProcessors();
        this.executorService = Executors.newFixedThreadPool(numberOfThreads);
    }

    /**
     * Starts the worker, initializing the logger and the Java <code>OtpNode</code>, and
     * enters the main receive loop, waiting for the genetic algorithm (workload) to be sent.
     */
    public void start() {
        if (!startWorkerLogger())
            return;

        if (!startJavaNode()) {
            workerLogger.log("Java node failed to start.");
            stopWorkerLogger();
            return;
        }

        workerLogger.log("Java node started correctly.");
        sendToErlangNode(ClusterUtils.Atom.HEARTBEAT);

        try {
            receiveLoop();
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            workerLogger.log(ExceptionUtils.getStackTrace(e));
            stopAllNodes();
        }
    }

    public static void main(String[] args) {
        String thisHost = args[0];
        String loggerCoordinatorHost = args[1];

        Worker worker = new Worker(thisHost, loggerCoordinatorHost);
        worker.start();
    }
}
