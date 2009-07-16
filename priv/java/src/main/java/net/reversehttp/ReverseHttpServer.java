package net.reversehttp;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

public class ReverseHttpServer extends HttpServer {
    private String label;
    private URL nextReq;
    private URL location;
    private URL serverAddress;
    private String token;
    private int leaseSeconds;
    private int failureDelay;
    private PropertyChangeSupport support;

    public ReverseHttpServer(String label, URL serverAddress,
            RequestHandler handler) {
        super(handler);
        this.label = label;
        this.nextReq = null;
        this.location = null;
        this.serverAddress = serverAddress;
        this.handler = handler;
        this.token = "-";
        this.leaseSeconds = 30;
        this.support = new PropertyChangeSupport(this);
    }

    public int getFailureDelay() {
        return failureDelay;
    }

    public void setFailureDelay(int failureDelay) {
        this.failureDelay = failureDelay;
    }

    protected URL getNextReq() {
        return nextReq;
    }

    public String getToken() {
        return token;
    }

    public void setToken(String token) {
        this.token = token;
    }

    public int getLeaseSeconds() {
        return leaseSeconds;
    }

    public void setLeaseSeconds(int leaseSeconds) {
        this.leaseSeconds = leaseSeconds;
    }

    @Override
    public void pollOnce() throws IOException {
        this.failureDelay = 2000;
        while (true) {
            try {
                pollOnceNoRetry();
                break;
            } catch (IOException ioe) {
                debugLog("IOException from pollOnceNoRetry: " + ioe);
                if (!shouldRetry(ioe)) {
                    break;
                } else {
                    try {
                        Thread.sleep(this.failureDelay);
                    } catch (InterruptedException e) {
                        break;
                    }
                    incrementFailureDelay();
                }
            }
        }
    }

    protected void incrementFailureDelay() {
        if (this.failureDelay < 30000) {
            this.failureDelay = this.failureDelay * 2;
        }
    }

    protected boolean shouldRetry(IOException ioe) {
        return isRunning();
    }

    public void pollOnceNoRetry() throws IOException {
        boolean declareMode = (this.nextReq == null);

        HttpURLConnection conn;
        if (declareMode) {
            String params = "name=" + this.label + "&token=" + this.token;
            conn = openServerConnection(this.serverAddress);
            conn.setRequestMethod("POST");
            conn.setRequestProperty("content-type",
                    "application/x-www-form-urlencoded");
            conn.setDoOutput(true);
            OutputStream os = conn.getOutputStream();
            os.write(params.getBytes("ASCII"));
            os.flush();
            os.close();
            debugLog("Registering " + this.label + " at " + this.serverAddress);
        } else {
            conn = openServerConnection(this.nextReq);
            debugLog("Polling " + this.nextReq);
        }
        conn.connect();

        int pollResponseCode = conn.getResponseCode();
        if (pollResponseCode < 200 || pollResponseCode >= 300) {
            throw new IOException("Unexpected response code: "
                    + pollResponseCode);
        }

        if (declareMode) {
            Map<String, URL> linkHeaders = parseLinkHeaders(conn);
            setNextReq(linkHeaders.get("first"));
            if (linkHeaders.containsKey("related")) {
                setLocation(linkHeaders.get("related"));
            }
            return;
        }

        if (pollResponseCode == 204) {
            // Poll timeout. Should retry immediately.
            return;
        }

        URL replyUrl = this.nextReq;
        setNextReq(parseLinkHeaders(conn).get("next"));

        String clientHost;
        int clientPort;

        String clientHostAndPort = conn.getHeaderField("requesting-client");
        if (clientHostAndPort != null) {
            String[] pieces = clientHostAndPort.split(":", 2);
            clientHost = (pieces.length > 0) ? pieces[0] : "";
            clientPort = (pieces.length > 1) ? Integer.parseInt(pieces[1]) : 0;
        } else {
            clientHost = "";
            clientPort = -1;
        }

        InputStream rawInput = conn.getInputStream();
        BufferedInputStream s = new BufferedInputStream(rawInput);

        List<HttpResponse> responses = new ArrayList<HttpResponse>();
        while (true) {
            HttpRequest req = new HttpRequest();
            req.setClientHost(clientHost);
            req.setClientPort(clientPort);
            if (!req.readFrom(s)) {
                break;
            }
            debugLog(req.toString());
            handleRequest(req);
            HttpResponse resp = req.getResponse();
            if (resp != null) {
                debugLog(resp.toString());
                responses.add(resp);
            }
        }

        if (!responses.isEmpty()) {
            conn.disconnect();
            conn = openServerConnection(replyUrl);
            conn.setRequestMethod("POST");
            conn.setRequestProperty("content-type", "message/http");
            conn.setDoOutput(true);
            debugLog("Sending replies...");
            OutputStream o = new BufferedOutputStream(conn.getOutputStream());
            for (HttpMessage resp : responses) {
                resp.writeOn(o);
            }
            o.flush();
            o.close();

            conn.connect();
            int replyResponseCode = conn.getResponseCode();
            if (replyResponseCode < 200 || replyResponseCode >= 300) {
                complain("Posting replies failed with code "
                        + replyResponseCode + " and message \""
                        + conn.getResponseMessage() + "\"");
            }
        }

        conn.disconnect();
        debugLog("Poll complete.");
    }

    private void setNextReq(URL u) throws IOException {
        if (u == null) {
            throw new IOException(
                    "Missing link header; is this really a reversehttp service?");
        }
        this.nextReq = u;
    }

    private Map<String, URL> parseLinkHeaders(HttpURLConnection conn) {
        HashMap<String, URL> result = new HashMap<String, URL>();
        List<String> values = conn.getHeaderFields().get("Link");
        if (values != null) {
            for (String value : values) {
                for (String linkHeader : value.split(", ")) {
                    parseLinkHeader(result, linkHeader);
                }
            }
        }
        return result;
    }

    private void parseLinkHeader(Map<String, URL> result, String linkHeader) {
        String rel = null;
        URL url = null;
        for (String piece : linkHeader.split(";")) {
            piece = piece.trim();
            if (piece.charAt(0) == '<') {
                try {
                    url = new URL(piece.substring(1, piece.length() - 1));
                } catch (MalformedURLException e) {
                    // Ignore it.
                }
            } else if (piece.toLowerCase().startsWith("rel=\"")) {
                rel = piece.substring(5, piece.length() - 1);
            }
        }
        if (rel != null && url != null) {
            result.put(rel, url);
        }
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        support.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        support.removePropertyChangeListener(listener);
    }

    protected void setLocation(URL url) {
        URL oldLoc = location;
        location = url;
        support.firePropertyChange("location", oldLoc, url);
    }

    private HttpURLConnection openServerConnection(URL loc) throws IOException {
        return (HttpURLConnection) loc.openConnection();
    }

    private void debugLog(String msg) {
        Logger.getLogger(ReverseHttpServer.class.getName()).fine(msg);
    }

    public String getLabel() {
        return label;
    }

    public URL getServerAddress() {
        return serverAddress;
    }

    public URL getLocation() {
        return location;
    }
}
