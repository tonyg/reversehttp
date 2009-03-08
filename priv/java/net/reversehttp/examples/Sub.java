package net.reversehttp.examples;

import java.io.UnsupportedEncodingException;

import net.reversehttp.messaging.Address;
import net.reversehttp.messaging.Message;
import net.reversehttp.messaging.MessageHandler;

public class Sub implements MessageHandler {
    public Sub(Address ownAddress, Address source) {
        boolean subscribedOk = new Message(ownAddress, source, ("{\"name\":\""
                + ownAddress + "\"}").getBytes(), "application/json",
                "subscribe").deliver();
        if (subscribedOk) {
            System.out.println("Subscribed " + source + " -> " + ownAddress);
        } else {
            throw new RuntimeException("Couldn't subscribe to " + source);
        }
    }

    public int handleMessage(Message msg) {
        try {
            System.out
                    .println(msg.getSender() + " -> " + msg.getTarget() + ", "
                            + msg.getContentType() + ": "
                            + msg.getBody("ASCII"));
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        return 200;
    }
}
