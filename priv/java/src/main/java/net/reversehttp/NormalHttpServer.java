package net.reversehttp;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class NormalHttpServer extends HttpServer {
    private int port;
    private ServerSocket serverSocket;

    public NormalHttpServer(int port, RequestHandler handler)
            throws IOException {
        super(handler);
        this.port = port;
        this.serverSocket = new ServerSocket(port);
    }

    public int getPort() {
        return port;
    }

    public ServerSocket getServerSocket() {
        return serverSocket;
    }

    @Override
    public void pollOnce() throws IOException {
        Socket conn = serverSocket.accept();
        try {
            singleRequest(conn);
        } catch (IOException ioe) {
            complain(ioe.toString(), ioe);
        }
    }

    protected void singleRequest(Socket conn) throws IOException {
        HttpRequest req = new HttpRequest();
        req.setClientHost(conn.getInetAddress().getHostAddress());
        req.setClientPort(conn.getPort());
        if (!req.readFrom(conn.getInputStream())) {
            req.setResponse(400, "Bad syntax");
        } else {
            req.setHttpVersion("1.0");
            handleRequest(req);
        }
        HttpResponse resp = req.getResponse();
        if (resp != null) {
            resp.writeOn(conn.getOutputStream());
        }
        conn.close();
    }
}
