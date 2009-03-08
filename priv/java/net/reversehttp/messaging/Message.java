package net.reversehttp.messaging;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

public class Message {
    private static final byte[] EMPTY_BODY = new byte[0];

    private Address sender;
    private Address target;
    private byte[] body;
    private String contentType;
    private String method;
    private List<Integer> errorCodes;
    private List<Exception> exceptions;

    public Message() {
    }

    public Message(Address sender, Address target, byte[] body) {
        this(sender, target, body, "text/plain");
    }

    public Message(Address sender, Address target, byte[] body,
            String contentType) {
        this(sender, target, body, contentType, "send");
    }

    public Message(Address sender, Address target, byte[] body,
            String contentType, String method) {
        this.sender = sender;
        this.target = target;
        this.setBody(body);
        this.contentType = contentType;
        this.method = method;
        this.errorCodes = null;
        this.exceptions = null;
    }

    public Message retarget(Address newSender, Address newTarget) {
        return new Message(newSender, newTarget, body, contentType, method);
    }

    public boolean deliver() {
        return deliver(3, 2000, 30000, 2);
    }

    public boolean deliver(int maxAttemptCount, int initialFailureDelay,
            int failureDelayLimit, double failureDelayMultiplier) {
        int failureDelay = initialFailureDelay;

        errorCodes = null;
        exceptions = null;

        URL targetUrl;
        try {
            targetUrl = new URL("http://" + target.getDomain() + "/"
                    + target.getLocalName());
        } catch (MalformedURLException mue) {
            recordException(mue);
            return false;
        }

        for (int attemptCount = 0; attemptCount < maxAttemptCount; attemptCount++) {
            try {
                HttpURLConnection conn = (HttpURLConnection) targetUrl
                        .openConnection();
                conn.setRequestProperty("Content-type", contentType);
                if (sender != null) {
                    conn.setRequestProperty("X-SMQP-Sender", sender.toString());
                }
                if (method != null) {
                    conn.setRequestProperty("X-SMQP-Method", method);
                }
                try {
                    conn.setRequestMethod("POST");
                } catch (ProtocolException e) {
                    // This should totally, *so* not happen.
                    throw new RuntimeException("Oh dear", e);
                }
                conn.setDoOutput(true);
                if (body != null) {
                    OutputStream o = conn.getOutputStream();
                    o.write(body);
                    o.flush();
                    o.close();
                }
                conn.connect();

                int responseCode = conn.getResponseCode();
                if (responseCode == 200) {
                    return true;
                }
                if (errorCodes == null) {
                    errorCodes = new ArrayList<Integer>();
                }
                errorCodes.add(responseCode);
                if (responseCode < 500 || responseCode >= 600) {
                    return false;
                }
            } catch (IOException ioe) {
                recordException(ioe);
            }

            try {
                Thread.sleep(failureDelay);
            } catch (InterruptedException e) {
                return false;
            }

            if (failureDelay < failureDelayLimit) {
                failureDelay = (int) (failureDelay * failureDelayMultiplier);
            }
        }

        return false;
    }

    private void recordException(Exception exn) {
        if (exceptions == null) {
            exceptions = new ArrayList<Exception>();
        }
        exceptions.add(exn);
    }

    public byte[] getBody() {
        return body;
    }

    public String getBody(String encoding) throws UnsupportedEncodingException {
        return new String(body, encoding);
    }

    public void setBody(byte[] body) {
        this.body = body == null ? EMPTY_BODY : body;
    }

    public String getContentType() {
        return contentType;
    }

    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public Address getSender() {
        return sender;
    }

    public void setSender(Address sender) {
        this.sender = sender;
    }

    public Address getTarget() {
        return target;
    }

    public void setTarget(Address target) {
        this.target = target;
    }

    public String toString() {
        return this.getClass().getName() + "(" + sender + "->" + target + ":"
                + method + ";" + contentType + ")";
    }
}
