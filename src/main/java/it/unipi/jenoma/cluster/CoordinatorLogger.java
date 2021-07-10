package it.unipi.jenoma.cluster;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;


class CoordinatorLogger implements Runnable {
    private final Logger logger;
    private final String host;
    private final OtpNode node;
    private final OtpMbox mailBox;


    private void stopNode() {
        mailBox.close();
        node.close();
    }

    public CoordinatorLogger(String host, String loggerName) throws IOException {
        this.logger = Logger.getLogger(loggerName);
        this.host = host;
        this.node = new OtpNode(ClusterUtils.compose(ClusterUtils.Node.LOGGER, host));
        this.mailBox = node.createMbox(ClusterUtils.Process.LOGGER);
        node.setCookie(ClusterUtils.Cluster.COOKIE);
    }

    @Override
    public void run() {
        OtpErlangObject msg;

        while(true) {
            try {
                msg = mailBox.receive();
            } catch (OtpErlangExit | OtpErlangDecodeException e) {
                logger.log(Level.FINE, ExceptionUtils.getStackTrace(e));
                continue;
            }

            if (msg instanceof OtpErlangAtom msgAtom && msgAtom.equals(ClusterUtils.Atom.STOP)) {
                stopNode();
                return;
            }

            if (msg instanceof OtpErlangString msgLog)
                logger.log(Level.INFO, msgLog.stringValue());
        }
    }
}
