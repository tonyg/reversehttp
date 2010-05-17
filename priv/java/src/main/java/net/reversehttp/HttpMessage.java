package net.reversehttp;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PushbackInputStream;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

public abstract class HttpMessage {
    protected Map<String, String> headers;
    public abstract String firstLine();
    protected byte[] body;
    protected String httpVersion;

    public HttpMessage() {
        super();
    }

    public Map<String, String> getHeaders() {
        return headers;
    }

    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }

    public void setHeader(String key, String value) {
        if (this.headers == null) {
            this.headers = new HashMap<String, String>();
        }
        this.headers.put(key, value);
    }

    public String getHeader(String headerName) {
        return getHeader(headerName, null);
    }

    public String getHeader(String headerName, String defaultValue) {
        if (headers != null) {
            String lcHeader = headerName.toLowerCase();
            if (headers.containsKey(lcHeader)) {
                return headers.get(lcHeader);
            }
        }
        return defaultValue;
    }

    public byte[] getBody() {
        return body;
    }

    public void setBody(byte[] body) {
        this.body = body;
    }

    /**
     * Sets the body to a UTF-8 encoded representation of the parameter. Fails with
     * a {@link RuntimeException} if UTF-8 encoding is not available (which should
     * never happen in normal JVMs).
     */
    public void setBody(String body) {
        try {
            setBody(body.getBytes("UTF-8"));
        } catch (UnsupportedEncodingException uee) {
            throw new RuntimeException(uee);
        }
    }

    public String toString() {
        return this.getClass().getName() + "(" + this.firstLine().trim() + ")";
    }

    public String getHttpVersion() {
        return httpVersion;
    }

    public void setHttpVersion(String httpVersion) {
        this.httpVersion = httpVersion;
    }

    protected String checkHttpVersion(String v) throws IOException {
        if (v.startsWith("HTTP/")) {
            return v.substring(5);
        } else {
            throw new IOException("Invalid http version string: " + v);
        }
    }

    public void writeOn(OutputStream o) throws IOException {
        BufferedWriter w = new BufferedWriter(new OutputStreamWriter(o));
        w.write(this.firstLine());
        if (headers != null) {
            for (Entry<String, String> entry : headers.entrySet()) {
                w.write(entry.getKey() + ": " + entry.getValue() + "\r\n");
            }
        }
        w.write("\r\n");
        w.flush();
        if (body != null) {
            o.write(body);
        }
    }

    protected abstract void readFirstLine(PushbackInputStream r)
            throws IOException;

    public boolean readFrom(InputStream s) {
        PushbackInputStream r = new PushbackInputStream(s);

        try {
            readFirstLine(r);
            readHeaders(r);
            return readBody(s);
        } catch (NumberFormatException nfe) {
            return false;
        } catch (IOException ioe) {
            return false;
        }
    }

    protected boolean readBody(InputStream s) throws IOException {
        if (headers != null && headers.containsKey("content-length")) {
            int contentLength = Integer.parseInt((String) headers
                    .get("content-length"));
            body = new byte[contentLength];
            int readCount = 0;
            int remaining = contentLength;
            while (readCount < contentLength) {
                int n = s.read(body, readCount, remaining);
                if (n == -1)
                    return false;
                readCount += n;
                remaining -= n;
            }
        }
        return true;
    }

    protected void readHeaders(PushbackInputStream r) throws IOException {
        headers = null;
        while (true) {
            if (consumeEol(r)) {
                break;
            }
            String key = readUpTo(r, ':');
            String value = readUpToEol(r);
            setHeader(key.trim().toLowerCase(), value.trim());
        }
    }

    protected boolean consumeEol(PushbackInputStream r) throws IOException {
        int ch = r.read();
        if (ch == -1) {
            return false;
        }
        if (ch == '\r') {
            ch = r.read();
            if (ch != -1 && ch != '\n') {
                r.unread(ch);
            }
            return true;
        }
        if (ch == '\n') {
            return true;
        }
        r.unread(ch);
        return false;
    }

    protected String readUpTo(PushbackInputStream r, char separator)
            throws IOException {
        StringBuffer buf = new StringBuffer();
        while (true) {
            int ch = r.read();
            if (ch == -1 || ch == separator) {
                return buf.toString();
            }
            if (ch == '\r' || ch == '\n') {
                throw new IOException("Unexpected embedded CR or LF");
            }
            buf.append((char) ch);
        }
    }

    protected String readUpToEol(PushbackInputStream r) throws IOException {
        StringBuffer buf = new StringBuffer();
        while (true) {
            if (consumeEol(r)) {
                return buf.toString();
            }
            int ch = r.read();
            if (ch == -1) {
                throw new IOException("Unexpected EOF");
            }
            buf.append((char) ch);
        }
    }
}