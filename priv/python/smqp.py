import BaseHTTPServer
import httplib
import time
import reversehttp
import base64
import time
import pickle
import zlib
import hmac
import random
import sha
from urlparse import urlsplit
from cgi import parse_qsl

class ServiceContainer:
    def __init__(self, server):
        self.pathprefix = ''
        self.server = server
        self.pathMap = {}
        server._smqp_service_container = self

    def setBaseurl(self, baseurl):
        self.pathprefix = urlsplit(baseurl)[2]

    def bindName(self, name, receiver):
        self.pathMap[name] = receiver

    def unbindName(self, name):
        del self.pathMap[name]

    def respondTo(self, req):
        if not req.path.startswith(self.pathprefix):
            req.send_error(404, "Destination not found")
            return

        reqUrl = urlsplit(req.path[len(self.pathprefix):])
        pathPieces = reqUrl.path.split("/", 1)
        target = pathPieces[0]
        if len(pathPieces) > 1:
            remainder = pathPieces[1]
        else:
            remainder = ''

        try:
            if self.pathMap.has_key(target):
                mname = 'do_' + req.command
                recipient = self.pathMap[target]
                params = dict(parse_qsl(reqUrl.query))
                if hasattr(recipient, mname):
                    getattr(recipient, mname)(req, remainder, params)
                elif hasattr(recipient, 'do_FALLBACK'):
                    recipient.do_FALLBACK(req, remainder, params)
                else:
                    req.send_error(501, "Unsupported method (%s)" % (req.command,))
            else:
                req.send_error(404, "Target not found")
        except:
            import traceback
            traceback.print_exc()
            req.send_error(500)

class RequestHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_PUT(self): self._handle()
    def do_DELETE(self): self._handle()
    def do_POST(self): self._handle()
    def do_GET(self): self._handle()
    def _handle(self):
        self.server._smqp_service_container.respondTo(self)

class HubModeFacet:
    def do_FALLBACK(self, req, path, params):
        hubMode = params.get('hub.mode', '')
        mname = 'do_' + req.command + '_' + hubMode
        if hasattr(self, mname):
            getattr(self, mname)(req, path, params)
        else:
            req.send_error(501, "Unsupported method/hub.mode (%s, %s)" % (req.command, hubMode))

def random_bytes(n):
    import array
    a = array.array('B')
    for i in xrange(n):
        a.append(random.randint(0, 255))
    return a.tostring()

instance_key = None
def _key():
    global instance_key
    if instance_key is None:
        instance_key = random_bytes(64)
    return instance_key

class SignedData:
    def __init__(self, datum):
        self.timestamp = time.time()
        self.nonce = random_bytes(8)
        self.datum = datum

def sign_term(v):
    message = SignedData(v)
    dataBlock = zlib.compress(pickle.dumps(message))
    mac = hmac.new(_key(), dataBlock, sha).digest()
    if len(mac) != 20:
        raise 'Invalid HMAC length'
    return mac + dataBlock

max_age = 300
def validate_token(macAndData):
    global max_age
    mac = macAndData[:20]
    dataBlock = macAndData[20:]
    if hmac.new(_key(), dataBlock, sha).digest() != mac:
        raise 'HMAC does not match'
    message = pickle.loads(zlib.decompress(dataBlock))
    if time.time() - message.timestamp > max_age:
        raise 'Token expired'
    return message.datum

class EndpointFacet(HubModeFacet):
    def __init__(self, sink):
        self.sink = sink

    def generate_token(self, path, intendedUse):
        signedTerm = sign_term((path, intendedUse))
        return base64.urlsafe_b64encode(signedTerm)

    def check_token(self, token, path, actualUse):
        try:
            (tPath, tUse) = validate_token(base64.urlsafe_b64decode(token))
            if tPath != path: return False
            if tUse != actualUse: return False
            return self.sink.check_action(actualUse, path)
        except:
            return False

    def do_check_token(self, req, path, params, actualUse):
        if self.check_token(params.get('hub.verify_token', ''), path, actualUse):
            req.send_response(204)
            req.end_headers()
        else:
            req.send_error(400, "Bad hub.verify_token")

    def do_GET_(self, req, path, params):
        req.send_response(200)
        req.send_header("Content-type", "text/plain")
        req.end_headers()
        req.wfile.write("Endpoint facet: " + str(self.sink))

    def do_GET_subscribe(self, req, path, params):
        self.do_check_token(req, path, params, "subscribe")

    def do_GET_unsubscribe(self, req, path, params):
        self.do_check_token(req, path, params, "unsubscribe")

    def do_GET_generate_token(self, req, path, params):
        token = self.generate_token(path, params['hub.intended_use'])
        req.send_response(200)
        req.send_header("Content-type", "application/x-www-form-urlencoded")
        req.end_headers()
        req.wfile.write("hub.verify_token=" + token)

    def do_POST(self, req, path, params):
        self.sink.deliver(params.get('hub.topic', ''),
                          req.headers.getheader('content-type'),
                          req.rfile.read())
        req.send_response(204)
        req.end_headers()

def test_container(label, hostAndPort):
    httpd = reversehttp.ReverseHttpServer(label,
                                          "http://" + hostAndPort + "/reversehttp",
                                          RequestHandler)
    container = ServiceContainer(httpd)

    def cb(x):
        print 'Location:', x.location
        container.setBaseurl(x.location)

    httpd.locationChangeCallback = cb
    return httpd, container

def test_endpoint(hostAndPort = 'localhost:8000'):
    class S:
        def check_action(self, actualUse, path):
            print repr(("check_action", actualUse, path))
            return True

        def deliver(self, topic, contentType, body):
            print repr(("deliver", topic, contentType, body))

    import random
    label = 'pythonsub' ## + str(int(random.uniform(0, 100000)))
    httpd, container = test_container(label, hostAndPort)
    sink = S()
    container.bindName("ep", EndpointFacet(sink))
    httpd.serve_forever()

if __name__ == '__main__':
    import sys
    if len(sys.argv) <= 1:
        print 'Usage: smqp.py endpoint [arg ...]'
        sys.exit(1)
    mode = sys.argv[1]
    if mode == 'endpoint':
        test_endpoint(*sys.argv[2:])
    else:
        print 'Invalid mode %s' % (mode,)
