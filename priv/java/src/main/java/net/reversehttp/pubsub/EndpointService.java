package net.reversehttp.pubsub;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.reversehttp.HttpQuery;
import net.reversehttp.HttpRequest;
import net.reversehttp.HttpServer;
import net.reversehttp.NormalHttpServer;
import net.reversehttp.RequestHandler;

public class EndpointService implements RequestHandler {
    public static void main(String[] args) {
        configureLogging();
        try {
            int port = (args.length > 0) ? Integer.parseInt(args[0]) : 8888;
            final Logger logger = Logger.getLogger(EndpointService.class.getName());
            logger.log(Level.FINE, "Starting on port " + port);
            HttpServer httpd = new NormalHttpServer(port,
                    new EndpointService(new Endpoint() {
                        public boolean acceptSubscriptionChange(String mode, String topic, String token, int leaseSeconds) {
                            System.out.println("Subscription change: "+mode+", "+topic+", "+token+", "+leaseSeconds);
                            return true;
                        }
                        public void handleDelivery(String topic, byte[] body) {
                            System.out.println("Received message:\n\"" + new String(body) + "\"");
                        }
                    }));
            httpd.serve();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void configureLogging() {
        ConsoleHandler handler = new ConsoleHandler();
        Logger root = Logger.getLogger("");
        root.addHandler(handler);
        handler.setLevel(Level.FINER);
        root.setLevel(Level.FINER);
    }

    private Endpoint handler;

    public EndpointService(Endpoint handler) {
        this.handler = handler;
    }

    public void handleRequest(HttpRequest req) {
        URI u;
        try {
            u = new URI(req.getRawPath());
        } catch (URISyntaxException ue) {
            req.setResponse(400, "Invalid path");
            return;
        }
        Map<String, String> params = HttpQuery.parse(u.getQuery());

        if ("GET".equals(req.getMethod())) {
            String challenge = params.get("hub.challenge");
            if (challenge == null) challenge = "";
            int leaseSeconds;
            try {
                leaseSeconds = Integer.parseInt(params.get("hub.lease_seconds"));
            } catch (NumberFormatException nfe) {
                leaseSeconds = -1;
            }
            if (handler.acceptSubscriptionChange(params.get("hub.mode"),
                                                 params.get("hub.topic"),
                                                 params.get("hub.verify_token"),
                                                 leaseSeconds)) {
                req.setResponse(200, "OK");
                req.getResponse().setHeader("Content-type", "text/plain; charset=utf-8");
                req.getResponse().setBody(challenge);
            } else {
                req.setResponse(403, "Forbidden");
            }
        } else if ("POST".equals(req.getMethod())) {
            String topic = params.get("hub.topic");
            this.handler.handleDelivery(topic, req.getBody());
            req.setResponse(204, "OK");
        } else {
            req.setResponse(405, "Invalid HTTP method");
        }
    }
}
