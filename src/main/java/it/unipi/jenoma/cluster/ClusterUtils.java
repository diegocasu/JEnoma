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


class ClusterUtils {
    public static class Cluster {
        public static final String COOKIE = "jenoma";
        public static final String REMOTE_WORK_DIRECTORY = ".jenoma";
        public static final String INIT_SCRIPT = "cluster_init";
    }

    public static class Node {
        public static final String JAVA = "javanode";
        public static final String ERLANG = "erlangnode";
        public static final String LOGGER = "logger";
    }

    public static class Process {
        public static final String COORDINATOR = "coordinator";
        public static final String WORKER = "worker";
        public static final String LOGGER = "logger";
    }

    public static class Erlang {
        public static final String MODULES_DIRECTORY = "ebin";
        public static final String COORDINATOR_MODULE = "coordinator";
        public static final String WORKER_MODULE = "worker";
    }

    public static class Atom {
        public static final OtpErlangAtom HEARTBEAT = new OtpErlangAtom("heartbeat");
        public static final OtpErlangAtom STOP = new OtpErlangAtom("stop");
        public static final OtpErlangAtom CLUSTER_READY = new OtpErlangAtom("cluster_ready");
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


    private static List<String> getProcessCommandStartShell() {
        List<String> command = new ArrayList<>();

        if (SystemUtils.IS_OS_WINDOWS) {
            Collections.addAll(command,"cmd", "/c");
        } else
            Collections.addAll(command,"bash");

        return command;
    }

    private static String asBackgroundShellCommand(String command) {
        if (SystemUtils.IS_OS_WINDOWS) {
            return "start /B " + command;
        } else
            return command + "&";
    }

    private static String getScriptFileExtension() {
        if (SystemUtils.IS_OS_WINDOWS) {
            return ".bat";
        } else
            return ".sh";
    }

    private static String getClusterInitScriptDeleteFile(String filename) {
        if (SystemUtils.IS_OS_WINDOWS) {
            return "del " + filename;
        } else
            return "rm " + filename;
    }

    private static String getClusterInitScriptSSH(Configuration configuration, String worker) {
        return String.format(
                "ssh -i %s/%s %s@%s",
                configuration.getSshKeyFolder(),
                worker,
                configuration.getSshUser(),
                worker);
    }

    private static String getClusterInitScriptMkdir(Configuration configuration, String worker) {
        return String.format(
                getClusterInitScriptSSH(configuration, worker) + " \"mkdir %s\" > output",
                Cluster.REMOTE_WORK_DIRECTORY);
    }

    private static String getClusterInitScriptSCP(Configuration configuration, String worker) {
        return String.format(
                "scp -i %s/%s %s %s@%s:%s/ > output",
                configuration.getSshKeyFolder(),
                worker,
                configuration.getJarPath(),
                configuration.getSshUser(),
                worker,
                Cluster.REMOTE_WORK_DIRECTORY);
    }

    private static String getClusterInitScriptJar(Configuration configuration, String worker) {
        return String.format(
                getClusterInitScriptSSH(configuration, worker) + " \"cd %s && jar -xvf *.jar %s\" > output",
                Cluster.REMOTE_WORK_DIRECTORY,
                Erlang.MODULES_DIRECTORY);
    }

    private static String getClusterInitScriptErl(Configuration configuration, String worker) {
        String[] jarPathElements =  configuration.getJarPath().split("[/\\\\]");
        String jarFile = jarPathElements[jarPathElements.length - 1];

        return String.format(
                getClusterInitScriptSSH(configuration, worker) +
                        " \"cd %s && erl -noshell -sname %s@%s -setcookie %s -pz %s " +
                        "-s %s start %s %s %s '%s' %s %s\"",
                Cluster.REMOTE_WORK_DIRECTORY,
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

    public static List<String> getProcessCommandStartEPMD() {
        List<String> command = new ArrayList<>(getProcessCommandStartShell());
        Collections.addAll(command, "epmd", "-daemon");
        return command;
    }

    public static List<String> getProcessCommandCommandStartErlangCoordinator(Configuration configuration) {
        List<String> command = new ArrayList<>(getProcessCommandStartShell());
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

    public static List<String> getShellCommandClusterInit() {
        List<String> command = new ArrayList<>();
        String scriptName = Cluster.INIT_SCRIPT + getScriptFileExtension();

        if (SystemUtils.IS_OS_WINDOWS) {
            Collections.addAll(command,"start", "/wait", scriptName);
        } else
            Collections.addAll(command,"chmod", "+x", scriptName, "&&", "./" + scriptName);

        return command;
    }

    public static String compose(String name, String host) {
        return name + "@" + host;
    }
}
