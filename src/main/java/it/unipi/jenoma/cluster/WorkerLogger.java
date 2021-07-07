package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import java.io.IOException;


class WorkerLogger implements ClusterLogger {
    private final String host;
    private final String coordinatorHost;
    private final String thisNodeName;
    private final OtpNode node;
    private final OtpMbox mailBox;


    public WorkerLogger(String host, String coordinatorHost, String thisNodeName) throws IOException {
        this.host = host;
        this.coordinatorHost = coordinatorHost;
        this.thisNodeName = thisNodeName;
        this.node = new OtpNode(ClusterUtils.compose(ClusterUtils.Node.LOGGER, host));
        this.mailBox = node.createMbox(ClusterUtils.Process.LOGGER);
        node.setCookie(ClusterUtils.Cluster.COOKIE);
    }

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
