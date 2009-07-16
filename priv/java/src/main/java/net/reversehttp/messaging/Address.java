package net.reversehttp.messaging;

public class Address {
    private String localName;
    private String domain;

    public Address(String localName, String domain) {
        this.localName = localName;
        this.domain = domain;
    }

    public static Address parse(String str) throws IllegalArgumentException {
        int atPos = str.indexOf('@');
        if (atPos == -1) {
            return new Address(null, str);
        } else {
            return new Address(str.substring(0, atPos), str
                    .substring(atPos + 1));
        }
    }

    public String getDomain() {
        return domain;
    }

    public String getLocalName() {
        return localName == null ? "" : localName;
    }

    public String toString() {
        return localName == null ? domain : localName + "@" + domain;
    }
}
