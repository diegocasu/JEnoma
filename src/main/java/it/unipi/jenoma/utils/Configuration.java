package it.unipi.jenoma.utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonParseException;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.Serializable;
import java.util.List;


/**
 * Class representing a JSON configuration file.
 * It stores the information needed to execute a genetic algorithm on a remote cluster.
 */
public class Configuration implements Serializable {
    private final String filePath;
    private String jarPath;
    private String sshKeyFolder;
    private String sshUser;
    private String coordinator;
    private List<String> workers;
    private int timeoutSetupCluster;
    private int timeoutWorker;
    private int seed;


    /**
     * Parses the JSON configuration file.
     * @throws IOException         if an I/O error occurs while accessing the file.
     * @throws JsonParseException  if the file contains an invalid JSON syntax.
     */
    private void parseConfiguration() throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filePath));
        Configuration parsedConfiguration = new Gson().fromJson(reader, Configuration.class);

        this.coordinator = parsedConfiguration.getCoordinator();
        this.workers = parsedConfiguration.getWorkers();
        this.timeoutWorker = parsedConfiguration.getTimeoutWorker();
        this.timeoutSetupCluster = parsedConfiguration.getTimeoutSetupCluster();
        this.seed = parsedConfiguration.getSeed();
        this.jarPath = parsedConfiguration.getJarPath();
        this.sshKeyFolder = parsedConfiguration.getSshKeyFolder();
        this.sshUser = parsedConfiguration.getSshUser();

        reader.close();
    }

    /**
     * Parses a JSON configuration file and creates a corresponding Configuration object.
     * @param filePath             the path of the configuration file.
     * @throws IOException         if an I/O error occurs while accessing the file.
     * @throws JsonParseException  if the file contains an invalid JSON syntax.
     */
    public Configuration(String filePath) throws IOException {
        this.filePath = filePath;
        parseConfiguration();
    }

    public String getJarPath() {
        return jarPath;
    }

    public String getSshKeyFolder() {
        return sshKeyFolder;
    }

    public String getSshUser() {
        return sshUser;
    }

    public String getFilePath() {
        return filePath;
    }

    public String getCoordinator() {
        return coordinator;
    }

    public List<String> getWorkers() {
        return workers;
    }

    public int getTimeoutSetupCluster() {
        return timeoutSetupCluster;
    }

    public int getTimeoutWorker() {
        return timeoutWorker;
    }

    public int getSeed() {
        return seed;
    }

    @Override
    public String toString() {
        return new GsonBuilder().setPrettyPrinting().create().toJson(this);
    }
}
