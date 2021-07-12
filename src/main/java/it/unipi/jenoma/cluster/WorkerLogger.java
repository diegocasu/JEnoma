package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import java.io.IOException;


/**
 * A logger used in a remote worker machine to send log messages to the coordinator,
 * so that they can be shown to the user.
 */
class WorkerLogger implements ClusterLogger {
    private final String host;
    private final String coordinatorHost;
    private final String thisNodeName;
    private final OtpNode node;
    private final OtpMbox mailBox;


    /**
     * Creates a new <code>WorkerLogger</code>.
     * @param host             the host name of the Java <code>OtpNode</code> representing the logger.
     * @param coordinatorHost  the host name of the coordinator machine.
     * @param thisNodeName     the name of the Java <code>OtpNode</code> that will appear on the log messages.
     * @throws IOException     if the Java <code>OtpNode</code> node cannot be instantiated correctly.
     */
    public WorkerLogger(String host, String coordinatorHost, String thisNodeName) throws IOException {
        this.host = host;
        this.coordinatorHost = coordinatorHost;
        this.thisNodeName = thisNodeName;
        this.node = new OtpNode(ClusterUtils.compose(ClusterUtils.Node.LOGGER, host));
        this.mailBox = node.createMbox(ClusterUtils.Process.LOGGER);
        node.setCookie(ClusterUtils.Cluster.COOKIE);
    }

    /**
     * Stops the Java <code>OtpNode</code> and deallocates its resources.
     */
    public void stop() {
        mailBox.close();
        node.close();
    }

    @Override
    public void log(String msg) {
        mailBox.send(
                ClusterUtils.Process.LOGGER,
                ClusterUtils.compose(ClusterUtils.Node.LOGGER, coordinatorHost),
                new OtpErlangString(String.format("%s: %s", thisNodeName, msg)));
    }
}
