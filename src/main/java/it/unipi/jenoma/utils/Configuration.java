package it.unipi.jenoma.utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;


public class Configuration {
    private String filePath;
    private String jarPath;
    private String sshKeyFolder;
    private String sshUser;
    private List<String> workers;
    private int seed;


    private void parseConfiguration() throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filePath));
        Configuration parsedConfiguration = new Gson().fromJson(reader, Configuration.class);

        this.workers = parsedConfiguration.getWorkers();
        this.seed = parsedConfiguration.getSeed();
        this.jarPath = parsedConfiguration.getJarPath();
        this.sshKeyFolder = parsedConfiguration.getSshKeyFolder();
        this.sshUser = parsedConfiguration.getSshUser();

        reader.close();
    }

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

    public List<String> getWorkers() {
        return workers;
    }

    public int getSeed() {
        return seed;
    }

    @Override
    public String toString() {
        return new GsonBuilder().setPrettyPrinting().create().toJson(this);
    }
}
