package net.reversehttp;

import java.io.IOException;
import java.io.PushbackInputStream;
import java.util.Map;

public class HttpRequest extends HttpMessage {
    private String method;
    private String rawPath;
    private String clientHost;
    private int clientPort;
    private HttpResponse response;

    public HttpRequest() {
    }

    public HttpRequest(String clientHost, int clientPort, String method,
            String rawPath, String httpVersion, Map<String, String> headers,
            byte[] body) {
        this.clientHost = clientHost;
        this.clientPort = clientPort;
        this.method = method;
        this.rawPath = rawPath;
        this.httpVersion = httpVersion;
        this.headers = headers;
        this.body = body;
        this.response = null;
    }

    public HttpResponse getResponse() {
        return response;
    }

    public void setResponse(HttpResponse response) {
        this.response = response;
    }

    public String getMethod() {
        return method;
    }

    public void setMethod(String method) {
        this.method = method;
    }

    public String getRawPath() {
        return rawPath;
    }

    public void setRawPath(String rawPath) {
        this.rawPath = rawPath;
    }

    public String getClientHost() {
        return clientHost;
    }

    public int getClientPort() {
        return clientPort;
    }

    public void setClientHost(String clientHost) {
        this.clientHost = clientHost;
    }

    public void setClientPort(int clientPort) {
        this.clientPort = clientPort;
    }

    @Override
    protected void readFirstLine(PushbackInputStream r) throws IOException {
        method = readUpTo(r, ' ');
        rawPath = readUpTo(r, ' ');
        httpVersion = checkHttpVersion(readUpToEol(r));
    }

    public void setResponse(int statusCode, String statusText) {
        setResponse(new HttpResponse(httpVersion, statusCode, statusText, null,
                null));
    }

    public String firstLine() {
        return method + " " + rawPath + " HTTP/" + httpVersion + "\r\n";
    }
}
