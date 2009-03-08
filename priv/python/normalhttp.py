import BaseHTTPServer

def test():
    class TestHandler(BaseHTTPServer.BaseHTTPRequestHandler):
        counter = 0
        def do_POST(self):
            self.do_GET()
        def do_GET(self):
            self.send_response(200)
            self.send_header("Content-type", "text/plain")
            self.end_headers()
            self.wfile.write("This is response #" + str(TestHandler.counter) + "\r\n")
            TestHandler.counter = TestHandler.counter + 1

    httpd = BaseHTTPServer.HTTPServer(('0.0.0.0', 8000), TestHandler)
    print 'Serving HTTP on 0.0.0.0:8000...'
    httpd.serve_forever()

if __name__ == '__main__':
    test()
