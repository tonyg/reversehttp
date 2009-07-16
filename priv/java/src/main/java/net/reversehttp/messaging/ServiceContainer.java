package net.reversehttp.messaging;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import net.reversehttp.HttpRequest;
import net.reversehttp.RequestHandler;

public class ServiceContainer implements RequestHandler {
    private String domain;
    private Map<String, MessageHandler> pathMap;

    public ServiceContainer(String domain) {
        this.domain = domain;
        this.pathMap = new HashMap<String, MessageHandler>();
    }

    public String getDomain() {
        return domain;
    }

    public void bindName(Address name, MessageHandler receiver) {
        pathMap.put(extractLocalname(name), receiver);
    }

    private String extractLocalname(Address name) {
        if (name.getDomain().equals(domain)) {
            return name.getLocalName();
        } else {
            throw new IllegalArgumentException("Domain mismatch: " + name
                    + ", " + domain);
        }
    }

    public void handleRequest(HttpRequest req) {
        String targetLocalname = req.getRawPath().substring(1);
        Address target = new Address(targetLocalname, domain);
        String senderStr = req.getHeader("X-SMQP-Sender");
        Address sender = senderStr == null ? null : Address.parse(senderStr);
        String contentType = req.getHeader("Content-type");
        String method = req.getHeader("X-SMQP-Method", "send");
        Message msg = new Message(sender, target, req.getBody(), contentType,
                method);

        try {
            if (pathMap.containsKey(targetLocalname)) {
                MessageHandler handler = pathMap.get(targetLocalname);
                int responseCode = handler.handleMessage(msg);
                req.setResponse(responseCode, "");
            } else {
                req.setResponse(404, "Destination not found");
            }
        } catch (Exception e) {
            LogRecord r = new LogRecord(Level.SEVERE, "Exception at " + target);
            r.setThrown(e);
            Logger.getLogger(this.getClass().getName()).log(r);
            req.setResponse(500, "Internal error");
        }
    }
}
