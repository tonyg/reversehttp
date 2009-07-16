package net.reversehttp.pubsub;

public interface Endpoint {
    boolean acceptSubscriptionChange(String mode, String topic, String token, int leaseSeconds);
    void handleDelivery(String topic, byte[] body);
}
