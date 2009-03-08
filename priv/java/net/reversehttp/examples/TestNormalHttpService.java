package net.reversehttp.examples;

import java.io.UnsupportedEncodingException;
import java.text.DateFormat;
import java.util.Date;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import net.reversehttp.HttpRequest;
import net.reversehttp.HttpServer;
import net.reversehttp.NormalHttpServer;
import net.reversehttp.RequestHandler;

public class TestNormalHttpService implements RequestHandler {
    public static void main(String[] args) {
        configureLogging();
        Logger.getLogger(TestReverseHttpService.class.getName()).log(
                Level.FINE, "Starting");
        try {
            int port = (args.length > 0) ? Integer.parseInt(args[0]) : 8000;
            HttpServer httpd = new NormalHttpServer(port,
                    new TestNormalHttpService());
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
        byte[] b;
        try {
            b = ("This is document #" + (counter++) + " served from Java\n")
                    .getBytes("UTF-8");
            req.getResponse().setBody(b);
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("Oh man this sucks", e);
        }
    }
}
