package it.unipi.jenoma.cluster;

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


    private static List<String> getShellCommandPrefix() {
        List<String> command = new ArrayList<>();

        if (SystemUtils.IS_OS_WINDOWS) {
            Collections.addAll(command,"cmd", "/c");
        } else
            Collections.addAll(command,"sh", "-c");

        return command;
    }

    private static String asBackgroundCommand(String command) {
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

    public static List<String> getShellCommandStartEPMD() {
        List<String> command = new ArrayList<>(getShellCommandPrefix());
        Collections.addAll(command, "epmd", "-daemon");
        return command;
    }

    public static List<String> getShellCommandStartErlangCoordinator(Configuration configuration) {
        List<String> command = new ArrayList<>(getShellCommandPrefix());
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
        String deleteOutputFile = getClusterInitScriptDeleteFile("output");

        try {
            // Clean file if it already exists.
            BufferedWriter writer = new BufferedWriter(new FileWriter(scriptName, false));
            writer.close();

            writer = new BufferedWriter(new FileWriter(scriptName, true));
            writer.append("echo \"%%%%%% If an SSH command is successful, " +
                    "but hangs, press ENTER to continue the execution %%%%%%\"");
            writer.newLine();

            for (String worker : configuration.getWorkers()) {
                writer.append(getClusterInitScriptMkdir(configuration, worker));
                writer.newLine();

                writer.append(getClusterInitScriptSCP(configuration, worker));
                writer.newLine();

                writer.append(getClusterInitScriptJar(configuration, worker));
                writer.newLine();

                writer.append(asBackgroundCommand(getClusterInitScriptErl(configuration, worker)));
                writer.newLine();
            }

            writer.append(deleteOutputFile);
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
            Collections.addAll(command,"sh", "./" + scriptName);

        return command;
    }

    public static String compose(String name, String host) {
        return name + "@" + host;
    }
}
