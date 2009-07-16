package net.reversehttp;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

abstract public class HttpServer {
    protected RequestHandler handler;
    private boolean running;

    public HttpServer(RequestHandler handler) {
        this.handler = handler;
        this.running = false;
    }

    public void serve() throws IOException {
        running = true;
        while (running) {
            pollOnce();
        }
    }

    public void stop() {
        running = false;
    }

    abstract public void pollOnce() throws IOException;

    protected void complain(String msg) {
        Logger.getLogger(this.getClass().getName()).severe(msg);
    }

    protected void complain(String msg, Throwable thrown) {
        LogRecord r = new LogRecord(Level.SEVERE, msg);
        r.setThrown(thrown);
        Logger.getLogger(this.getClass().getName()).log(r);
    }

    public Object getHandler() {
        return handler;
    }

    public void setHandler(RequestHandler handler) {
        this.handler = handler;
    }

    protected boolean isRunning() {
        return running;
    }

    protected void handleRequest(HttpRequest req) {
        try {
            handler.handleRequest(req);
        } catch (Exception e) {
            if (req.getResponse() == null) {
                req.setResponse(500, "Internal Server Error");
            }
        }
    }
}