package net.reversehttp.examples;

import java.net.URL;
import java.util.Random;

import net.reversehttp.HttpServer;
import net.reversehttp.ReverseHttpServer;
import net.reversehttp.messaging.Address;
import net.reversehttp.messaging.ServiceContainer;

public class ReverseSub {
    public static void main(String[] args) {
        try {
            String sourceStr = (args.length > 0) ? args[0]
                    : "relay@relay.localhost.lshift.net:8000";
            String hostAndPort = (args.length > 1) ? args[1]
                    : "localhost.lshift.net:8000";
            URL serverUrl = new URL("http://" + hostAndPort + "/reversehttp");
            String label = "javasub" + new Random().nextInt(100000);
            String containerDomain = label + "." + hostAndPort;
            ServiceContainer container = new ServiceContainer(containerDomain);
            Address targetAddress = new Address("queue", containerDomain);
            container.bindName(targetAddress, new Sub(targetAddress, Address
                    .parse(sourceStr)));
            HttpServer httpd = new ReverseHttpServer(label, serverUrl,
                    container);
            httpd.serve();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
