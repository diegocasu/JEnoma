package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import it.unipi.jenoma.utils.Configuration;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Class holding constants and utility methods for the management of the cluster.
 */
class ClusterUtils {
    // Constants useful for the management of the cluster.
    public static class Cluster {
        public static final String COOKIE = "jenoma";
        public static final String REMOTE_WORKING_DIRECTORY = ".jenoma";
        public static final String INIT_SCRIPT = "cluster_init";
    }

    /*
     * Constants representing the names of Erlang nodes in the cluster, where
     * a single node is identified by the atom 'nodeName@hostName'. The hostName
     * part is given by configuration (coordinator and workers fields).
     */
    public static class Node {
        public static final String JAVA = "javanode";
        public static final String ERLANG = "erlangnode";
        public static final String LOGGER = "logger";
    }

    // Constants representing the names of Erlang processes on each node.
    public static class Process {
        public static final String COORDINATOR = "coordinator";
        public static final String WORKER = "worker";
        public static final String LOGGER = "logger";
    }

    // Constants representing the directory and files of the project containing Erlang modules.
    public static class Erlang {
        public static final String MODULES_DIRECTORY = "ebin";
        public static final String COORDINATOR_MODULE = "coordinator";
        public static final String WORKER_MODULE = "worker";
    }

    // Constants representing atoms used in the exchange of messages between Erlang processes.
    public static class Atom {
        public static final OtpErlangAtom HEARTBEAT = new OtpErlangAtom("heartbeat");
        public static final OtpErlangAtom STOP = new OtpErlangAtom("stop");
        public static final OtpErlangAtom CLUSTER_READY = new OtpErlangAtom("cluster_ready");
        public static final OtpErlangAtom CLUSTER_TIMEOUT = new OtpErlangAtom("cluster_timeout");
        public static final OtpErlangAtom NOT_INVOLVED_ELITISM = new OtpErlangAtom("not_involved_elitism");
        public static final OtpErlangAtom TERMINATION_CONDITIONS = new OtpErlangAtom("termination_conditions");
        public static final OtpErlangAtom FINAL_POPULATION = new OtpErlangAtom("final_population");
        public static final OtpErlangAtom ALGORITHM_CONTINUE = new OtpErlangAtom("algorithm_continue");
        public static final OtpErlangAtom ALGORITHM_END = new OtpErlangAtom("algorithm_end");
        public static final OtpErlangAtom COMPUTATION_FAILED = new OtpErlangAtom("computation_failed");
        public static final OtpErlangAtom INIT_PHASE = new OtpErlangAtom("init_phase");
        public static final OtpErlangAtom CLUSTER_SETUP_PHASE = new OtpErlangAtom("cluster_setup_phase");
        public static final OtpErlangAtom ELITISM_PHASE = new OtpErlangAtom("elitism_phase");
        public static final OtpErlangAtom GENERATION_END_PHASE = new OtpErlangAtom("generation_end_phase");
        public static final OtpErlangAtom RESULT_COLLECTION_PHASE = new OtpErlangAtom("result_collection_phase");
        public static final OtpErlangAtom SHUTDOWN_PHASE = new OtpErlangAtom("shutdown_phase");
        public static final OtpErlangAtom SHUFFLE = new OtpErlangAtom("shuffle");
        public static final OtpErlangAtom SHUFFLE_FRAGMENT = new OtpErlangAtom("shuffle_fragment");
        public static final OtpErlangAtom SHUFFLE_COMPLETE = new OtpErlangAtom("shuffle_complete");
    }


    /**
     * Returns the prefix to invoke a command in the shell of the current OS.
     * The prefix is in a format suitable to be passed to the <code>command</code> method of
     * <code>ProcessBuilder</code>.
     * @return  a list of strings representing the command <code>"cmd -c"</code> if the OS is Windows,
     *          the command <code>bash</code> otherwise.
     */
    private static List<String> getProcessCommandShellPrefix() {
        List<String> command = new ArrayList<>();

        if (SystemUtils.IS_OS_WINDOWS) {
            Collections.addAll(command,"cmd", "/c");
        } else
            Collections.addAll(command,"bash");

        return command;
    }

    /**
     * Returns the passed command as a background command for the shell of the current OS.
     * The command is modified prepending <code>start /B</code> if the OS is Windows,
     * appending <code>&</code> otherwise.
     * @param command  the command to be modified.
     * @return         the modified command.
     */
    private static String asBackgroundShellCommand(String command) {
        if (SystemUtils.IS_OS_WINDOWS) {
            return "start /B " + command;
        } else
            return command + "&";
    }

    /**
     * Returns the file extension that denotes a script file in the current OS.
     * @return  <code>.bat</code> if the OS is Windows, <code>.sh</code> otherwise.
     */
    private static String getScriptFileExtension() {
        if (SystemUtils.IS_OS_WINDOWS) {
            return ".bat";
        } else
            return ".sh";
    }

    /**
     * Returns the OS specific command to delete a given file.
     * The command is modified prepending <code>del</code> if the OS is Windows,
     * prepending <code>rm</code> otherwise.
     * @param filename  the name of the file to be deleted.
     * @return          the modified command.
     */
    private static String getClusterInitScriptDeleteFile(String filename) {
        if (SystemUtils.IS_OS_WINDOWS) {
            return "del " + filename;
        } else
            return "rm " + filename;
    }

    /**
     * Returns the SSH command to connect to a remote worker machine. The command is in the form
     * <code>ssh -i sshKeyFolder/worker sshUser@worker</code>.
     * @param configuration  the configuration object holding the information needed to complete the command.
     * @param worker         the name of the remote worker machine.
     * @return               the SSH command to connect to a remote worker machine.
     */
    private static String getClusterInitScriptSSH(Configuration configuration, String worker) {
        return String.format(
                "ssh -i %s/%s %s@%s",
                configuration.getSshKeyFolder(),
                worker,
                configuration.getSshUser(),
                worker);
    }

    /**
     * Returns the SSH command to create a working directory in a remote worker machine.
     * The command is in the form <code>ssh -i sshKeyFolder/worker sshUser@worker
     * "mkdir REMOTE_WORKING_DIRECTORY" > output</code>.
     * @param configuration  the configuration object holding the information needed to complete the command.
     * @param worker         the name of the remote worker machine.
     * @return               the SSH command to create a working directory in a remote worker machine.
     */
    private static String getClusterInitScriptMkdir(Configuration configuration, String worker) {
        return String.format(
                getClusterInitScriptSSH(configuration, worker) + " \"mkdir %s\" > output",
                Cluster.REMOTE_WORKING_DIRECTORY);
    }

    /**
     * Returns the SCP command to transfer the JAR file of the project to a remote worker machine.
     * The command is in the form <code>scp -i sshKeyFolder/worker jarPath
     * sshUser@worker:REMOTE_WORKING_DIRECTORY/ > output</code>.
     * @param configuration  the configuration object holding the information needed to complete the command.
     * @param worker         the name of the remote worker machine.
     * @return               the SCP command to transfer the JAR file of the project to a remote worker machine.
     */
    private static String getClusterInitScriptSCP(Configuration configuration, String worker) {
        return String.format(
                "scp -i %s/%s %s %s@%s:%s/ > output",
                configuration.getSshKeyFolder(),
                worker,
                configuration.getJarPath(),
                configuration.getSshUser(),
                worker,
                Cluster.REMOTE_WORKING_DIRECTORY);
    }

    /**
     * Returns the SSH command to extract the <code>MODULES_DIRECTORY</code> folder of the project
     * in the working directory of the remote worker machine. The command is in the format
     * <code>ssh -i sshKeyFolder/worker sshUser@worker  "cd REMOTE_WORKING_DIRECTORY &&
     * jar -xvf *.jar MODULES_DIRECTORY" > output</code>.
     * @param configuration  the configuration object holding the information needed to complete the command.
     * @param worker         the name of the remote worker machine.
     * @return               the SSH command to extract the <code>MODULES_DIRECTORY</code> folder of
     *                       the project in the working directory of the remote worker machine.
     */
    private static String getClusterInitScriptJar(Configuration configuration, String worker) {
        return String.format(
                getClusterInitScriptSSH(configuration, worker) + " \"cd %s && jar -xvf *.jar %s\" > output",
                Cluster.REMOTE_WORKING_DIRECTORY,
                Erlang.MODULES_DIRECTORY);
    }

    /**
     * Returns the SSH command used to start an Erlang worker node in a remote machine.
     * The command is in the format
     * <code>ssh -i sshKeyFolder/worker sshUser@worker "cd REMOTE_WORKING_DIRECTORY &&
     * erl -noshell -sname ERLANG@worker -setcookie COOKIE -pz MODULES_DIRECTORY -s
     * WORKER_MODULE start WORKER COORDINATOR ERLANG@coordinatorHost jarFile WORKER
     * JAVA@worker"</code>
     * @param configuration  the configuration object holding the information needed to complete the command.
     * @param worker         the name of the remote worker machine.
     * @return               the SSH command used to start an Erlang worker node in a remote machine.
     */
    private static String getClusterInitScriptErl(Configuration configuration, String worker) {
        String[] jarPathElements =  configuration.getJarPath().split("[/\\\\]");
        String jarFile = jarPathElements[jarPathElements.length - 1];

        return String.format(
                getClusterInitScriptSSH(configuration, worker) +
                        " \"cd %s && erl -noshell -sname %s@%s -setcookie %s -pz %s " +
                        "-s %s start %s %s %s '%s' %s %s\"",
                Cluster.REMOTE_WORKING_DIRECTORY,
                Node.ERLANG,
                worker,
                Cluster.COOKIE,
                Erlang.MODULES_DIRECTORY,
                Erlang.WORKER_MODULE,
                Process.WORKER,
                Process.COORDINATOR,
                ClusterUtils.compose(Node.ERLANG, configuration.getCoordinator()),
                jarFile,
                Process.WORKER,
                ClusterUtils.compose(Node.JAVA, worker));
    }

    /**
     * Returns the shell command to be executed to start the EPMD daemon in the current machine.
     * The command is <code>epmd -daemon</code>, with an OS specific prefix to be launched in the shell.
     * The command is in a format suitable to be passed to the <code>command</code> method of
     * <code>ProcessBuilder</code>.
     * @return  a list of strings representing the shell command to be executed to start the
     *          EPMD daemon in the current machine.
     */
    public static List<String> getProcessCommandStartEPMD() {
        List<String> command = new ArrayList<>(getProcessCommandShellPrefix());
        Collections.addAll(command, "epmd", "-daemon");
        return command;
    }

    /**
     * Returns the shell command to be executed to start the Erlang coordinator in the current machine.
     * The command is in the format <code>erl -noshell -sname ERLANG@coordinatorHost -setcookie COOKIE
     * -pz MODULES_DIRECTORY -s COORDINATOR_MODULE start COORDINATOR COORDINATOR JAVA@coordinatorHost</code>.
     * The command is in a format suitable to be passed to the <code>command</code> method of
     * <code>ProcessBuilder</code>.
     * @param configuration  the configuration object holding the information needed to complete the command.
     * @return               a list of strings representing the shell command to be executed to start
      *                      the Erlang coordinator in the current machine.
     */
    public static List<String> getProcessCommandCommandStartErlangCoordinator(Configuration configuration) {
        List<String> command = new ArrayList<>(getProcessCommandShellPrefix());
        Collections.addAll(command,
                "erl",
                "-noshell",
                "-sname", ClusterUtils.compose(Node.ERLANG, configuration.getCoordinator()),
                "-setcookie", Cluster.COOKIE,
                "-pz", Erlang.MODULES_DIRECTORY,
                "-s",
                Erlang.COORDINATOR_MODULE,
                "start",
                Process.COORDINATOR,
                Process.COORDINATOR,
                ClusterUtils.compose(Node.JAVA, configuration.getCoordinator()));

        return command;
    }

    /**
     * Creates the script file used to initialize the remote cluster. For each remote worker
     * machine, the script creates a working directory, transfers the project files, extracts the
     * Erlang modules and starts the Erlang worker node. The output of the commands in the script
     * is redirected to an <code>output</code> file to avoid that the commands hang waiting for their
     * output to be consumed; the file is automatically deleted when the script execution ends.
     * @param configuration  the configuration object holding the information needed to create
     *                       the commands in the script.
     * @param logger         a local logger used to report eventual I/O exceptions.
     * @return               true if the script file is created correctly, false if an I/O error occurs.
     */
    public static boolean createClusterInitScript(Configuration configuration, Logger logger) {
        String scriptName = Cluster.INIT_SCRIPT + getScriptFileExtension();

        try {
            // Clean file if it already exists.
            BufferedWriter writer = new BufferedWriter(new FileWriter(scriptName, false));
            writer.close();

            writer = new BufferedWriter(new FileWriter(scriptName, true));

            if (SystemUtils.IS_OS_WINDOWS) {
                writer.append("echo \"%%%%%% If an SSH command is successful, " +
                        "but hangs, press ENTER to continue the execution %%%%%%\"");
                writer.newLine();
            }

            for (String worker : configuration.getWorkers()) {
                writer.append(getClusterInitScriptMkdir(configuration, worker));
                writer.newLine();

                writer.append(getClusterInitScriptSCP(configuration, worker));
                writer.newLine();

                writer.append(getClusterInitScriptJar(configuration, worker));
                writer.newLine();

                writer.append(asBackgroundShellCommand(getClusterInitScriptErl(configuration, worker)));
                writer.newLine();
            }

            writer.append(getClusterInitScriptDeleteFile("output"));
            writer.newLine();

            writer.append("exit");
            writer.newLine();

            writer.close();
        } catch (IOException e) {
            logger.log(Level.SEVERE, ExceptionUtils.getStackTrace(e));
            return false;
        }

        return true;
    }

    /**
     * Returns the OS specific shell command that starts the execution of the cluster initialization script.
     * The command is in the format <code>start /wait INIT_SCRIPT.bat</code> if the OS is Windows,
     * <code>chmod +x INIT_SCRIPT.sh && ./INIT_SCRIPT.sh</code> otherwise.
     * The command must <b>not</b> be passed to a <code>ProcessBuilder</code>:
     * it is intended to be executed by the <code>os:cmd()</code> function provided by Erlang.
     * @return  a list of strings representing the shell command that starts the execution of the
     *          cluster initialization script.
     */
    public static List<String> getShellCommandClusterInit() {
        List<String> command = new ArrayList<>();
        String scriptName = Cluster.INIT_SCRIPT + getScriptFileExtension();

        if (SystemUtils.IS_OS_WINDOWS) {
            Collections.addAll(command,"start", "/wait", scriptName);
        } else
            Collections.addAll(command,"chmod", "+x", scriptName, "&&", "./" + scriptName);

        return command;
    }

    /**
     * Returns the composition of a name and a host, following the Erlang convention for node names.
     * @param name  the name of the Erlang node.
     * @param host  the host of the Erlang node.
     * @return      the string <code>name@host</code>.
     */
    public static String compose(String name, String host) {
        return name + "@" + host;
    }
}
