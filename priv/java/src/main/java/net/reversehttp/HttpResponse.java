package net.reversehttp;

import java.io.IOException;
import java.io.PushbackInputStream;
import java.util.Map;

public class HttpResponse extends HttpMessage {
    private int statusCode;
    private String statusText;

    public HttpResponse() {
    }

    public HttpResponse(String httpVersion, int statusCode, String statusText,
            Map<String, String> headers, byte[] body) {
        this.httpVersion = httpVersion;
        this.statusCode = statusCode;
        this.statusText = statusText;
        this.headers = headers;
        this.body = body;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public String getStatusText() {
        return statusText;
    }

    public void setStatusText(String statusText) {
        this.statusText = statusText;
    }

    @Override
    public String firstLine() {
        return "HTTP/" + httpVersion + " " + statusCode + " " + statusText
                + "\r\n";
    }

    protected void readFirstLine(PushbackInputStream r) throws IOException {
        httpVersion = checkHttpVersion(readUpTo(r, ' '));
        statusCode = Integer.parseInt(readUpTo(r, ' '));
        statusText = readUpToEol(r);
    }
}
