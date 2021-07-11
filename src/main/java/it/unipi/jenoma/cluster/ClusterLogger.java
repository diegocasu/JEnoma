package it.unipi.jenoma.cluster;


/**
 * An interface that gives access to a logger that sends messages towards the coordinator machine.
 * The interface hides the implementation of WorkerLogger, which cannot be instantiated directly
 * by the user, but only inside Worker.
 */
public interface ClusterLogger {

    /**
     * Sends a log to the coordinator machine.
     * @param msg  the log message.
     */
    void log(String msg);
}
