package net.reversehttp.examples;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.URL;
import java.text.DateFormat;
import java.util.Date;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.reversehttp.HttpRequest;
import net.reversehttp.HttpServer;
import net.reversehttp.RequestHandler;
import net.reversehttp.ReverseHttpServer;

public class TestReverseHttpService implements RequestHandler,
        PropertyChangeListener {
    public static void main(String[] args) {
        configureLogging();
        Logger.getLogger(TestReverseHttpService.class.getName()).log(
                Level.FINE, "Starting");
        try {
            String label;

            label = (args.length > 0) ? args[0] : "java";
            String hostAndPort = (args.length > 1) ? args[1]
                    : "localhost:8000";

            URL reflectorUrl = new URL("http://" + hostAndPort + "/reversehttp");
            TestReverseHttpService service = new TestReverseHttpService();
            HttpServer httpd = new ReverseHttpServer(label, reflectorUrl,
                    service);
            ((ReverseHttpServer) httpd).addPropertyChangeListener(service);
            httpd.serve();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void configureLogging() {
        ConsoleHandler handler = new ConsoleHandler();
        Logger root = Logger.getLogger("");
        root.addHandler(handler);
        // handler.setLevel(Level.FINER);
        // root.setLevel(Level.FINER);
    }

    private int counter = 0;
    private DateFormat df = DateFormat.getDateTimeInstance();

    public void handleRequest(HttpRequest req) {
        Date now = new Date();
        System.err.println(df.format(now) + " [" + req.getClientHost() + ":"
                + req.getClientPort() + "] " + req.getMethod() + " "
                + req.getRawPath() + " HTTP/" + req.getHttpVersion());
        req.setResponse(200, "OK");
        req.getResponse()
                .setHeader("Content-type", "text/plain; charset=utf-8");
        req.getResponse()
                .setBody("This is document #" + (counter++) + " served from Java\n");
    }

    public void propertyChange(PropertyChangeEvent evt) {
        if (evt.getPropertyName().equals("location")) {
            System.out.println("Location changed to " + evt.getNewValue());
        }
    }
}
