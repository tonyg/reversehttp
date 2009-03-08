package net.reversehttp.examples;

import net.reversehttp.HttpServer;
import net.reversehttp.NormalHttpServer;
import net.reversehttp.messaging.Address;
import net.reversehttp.messaging.ServiceContainer;

public class NormalSub {
    public static void main(String[] args) {
        try {
            String sourceStr = (args.length > 0) ? args[0]
                    : "relay@relay.localhost.lshift.net:8000";
            String ownAddressStr = (args.length > 1) ? args[1]
                    : "queue@localhost.lshift.net:8001";
            int port = (args.length > 2) ? Integer.parseInt(args[2]) : 8001;
            Address ownAddress = Address.parse(ownAddressStr);

            ServiceContainer container = new ServiceContainer(ownAddress
                    .getDomain());
            container.bindName(ownAddress, new Sub(ownAddress, Address
                    .parse(sourceStr)));
            HttpServer httpd = new NormalHttpServer(port, container);
            httpd.serve();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
