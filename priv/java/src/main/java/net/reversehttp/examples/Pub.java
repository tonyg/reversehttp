package net.reversehttp.examples;

import java.util.Date;

import net.reversehttp.messaging.Address;
import net.reversehttp.messaging.Message;

public class Pub {
    public static void main(String[] args) {
        int count = (args.length > 2) ? Integer.parseInt(args[2]) : 1;
        for (int i = 0; i < count; i++) {
            String targetStr = (args.length > 0) ? args[0]
                    : "relay@relay.localhost.lshift.net:8000";
            String body = (args.length > 1 && !args[1].equals("")) ? args[1]
                    : new Date().toString();
            Message msg = new Message(null, Address.parse(targetStr), body
                    .getBytes());
            msg.deliver();
        }
    }
}
