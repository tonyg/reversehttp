import BaseHTTPServer
import httplib
import time
import reversehttp

try:
    import simplejson as json
except ImportError, e:
    import sys
    if sys.hexversion >= 0x20600f0:
        import json
    else:
        raise e

class InvalidApplicationNameError(ValueError): pass

def parseName(name):
    parts = name.split('@', 1)
    if len(parts) == 1:
        return (None, parts[0])
    else:
        return (parts[0], parts[1])

def joinName((localName, domain)):
    return localName + '@' + domain

class SmqpMessage:
    def __init__(self, sender, target, body, contentType = "text/plain", method = "send"):
        self.sender = sender
        self.target = target
        self.body = body
        self.contentType = contentType
        self.method = method
        self.errorReports = []

    def retarget(self, sender, target):
        return SmqpMessage(sender, target, self.body, self.contentType, self.method)

    def deliver(self,
                onRetry = None,
                initialFailureDelay = 2,
                failureDelayLimit = 30,
                maxAttemptCount = 3,
                failureDelayMultiplier = 2):
        (rawPath, hostAndPort) = parseName(self.target)
        rawPath = '/' + rawPath

        attemptCount = 0
        failureDelay = initialFailureDelay

        headers = {'Content-type': self.contentType}
        if self.sender: headers['X-SMQP-Sender'] = self.sender
        if self.method: headers['X-SMQP-Method'] = self.method

        self.errorReports = []
        while attemptCount < maxAttemptCount:
            try:
                attemptCount = attemptCount + 1
                conn = httplib.HTTPConnection(hostAndPort)
                conn.request("POST", rawPath, body = self.body, headers = headers)
                resp = conn.getresponse()
                if resp.status >= 200 and resp.status < 300:
                    return True
                else:
                    self.errorReports.append({'response': resp})
                    if (resp.status < 500) or (resp.status >= 600):
                        break
            except Exception, e:
                self.errorReports.append({'exception': e})
            time.sleep(failureDelay)
            if failureDelay < failureDelayLimit:
                failureDelay = failureDelay * failureDelayMultiplier
            if onRetry: onRetry(self)
        return False

    def jsonBody(self):
        if not hasattr(self, '_jsonBody'):
            self._jsonBody = json.loads(self.body)
        return self._jsonBody

class SmqpServiceContainer:
    def __init__(self, domain, server):
        self.domain = domain
        self.server = server
        self.pathMap = {}
        server._smqp_service_container = self

    def extractLocalname(self, name):
        suffixLength = len(self.domain) + 1
        prefixLength = len(name) - suffixLength
        if name[prefixLength:] == "@" + self.domain:
            return name[:prefixLength]
        else:
            raise InvalidApplicationNameError(name)

    def expandLocalname(self, localName):
        return joinName((localName, self.domain))

    def bindName(self, name, receiver):
        self.pathMap[self.extractLocalname(name)] = receiver

    def unbindName(self, name):
        del self.pathMap[self.extractLocalname(name)]

    def respondTo(self, req):
        targetLocalname = req.path[1:]
        target = self.expandLocalname(targetLocalname)
        sender = req.headers.getheader('X-SMQP-Sender')
        contentType = req.headers.getheader('Content-type')
        method = req.headers.getheader('X-SMQP-Method', 'send')
        m = SmqpMessage(sender, target, req.rfile.read(), contentType)

        try:
            if self.pathMap.has_key(targetLocalname):
                mname = 'do_' + method
                recipient = self.pathMap[targetLocalname]
                if hasattr(recipient, mname):
                    responseCode = (getattr(recipient, mname)(m)) or 200
                    req.send_response(responseCode)
                    req.end_headers()
                else:
                    req.send_error(501, "Unsupported method (%s)" % (method,))
            else:
                req.send_error(404, "Destination not found")
        except:
            import traceback
            traceback.print_exc()
            req.send_error(500)

class SmqpRequestHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    def do_POST(self):
        self.do_GET()

    def do_GET(self):
        self.server._smqp_service_container.respondTo(self)

def jsonRequest(sender, target, method, body):
    return SmqpMessage(sender, target, json.dumps(body), "application/json", method)

class SmqpRelay:
    def __init__(self):
        self.subscribers = {}

    def addSubscriber(self, name, filter):
        if not self.vetSubscriber(name, filter):
            return False
        self.subscribers[name] = filter
        self.onSubscriberAdded(name, filter)
        return True

    def removeSubscriber(self, name):
        del self.subscribers[name]
        self.onSubscriberRemoved(name)

    def matchingSubscribers(self, message):
        result = {}
        for (name, filter) in self.subscribers.items():
            if filter(message):
                result[name] = filter
        return result

    def decodeFilter(self, spec):
        return self.defaultFilter() # TODO -- simple filter language?

    def defaultFilter(self):
        return lambda message: True

    def vetSubscriber(self, name, filter):
        return True

    def onSubscriberAdded(self, name, filter):
        pass

    def onSubscriberRemoved(self, name):
        pass

    def onDeliveryFailure(self, message, target, errorReports):
        pass

    def checkDeliveryAcceptance(self, completeCount, abortedCount, totalCount):
        if (totalCount == 0) or (completeCount > 0):
            return 200
        if (abortedCount == totalCount):
            return 200  # absorb failures
        return None

    def do_send(self, message):
        return self.acceptMessage(message)

    def do_subscribe(self, message):
        args = message.jsonBody()
        if args.has_key('filter'):
            f = self.decodeFilter(args['filter'])
        else:
            f = self.defaultFilter()
        self.addSubscriber(args['name'], f)

    def do_unsubscribe(self, message):
        args = message.jsonBody()
        self.removeSubscriber(args['name'])

    def acceptMessage(self, message):
        subs = self.matchingSubscribers(message)
        return self.deliver(message, subs)

    def deliver(self, message, subs):
        completeCount = 0
        abortedCount = 0

        for sub in subs:
            outbound = message.retarget(message.target, sub)
            if outbound.deliver():
                completeCount = completeCount + 1
            else:
                self.onDeliveryFailure(message, sub, outbound.errorReports)
                self.removeSubscriber(sub)
                abortedCount = abortedCount + 1

        return self.checkDeliveryAcceptance(completeCount, abortedCount, len(subs))

def test_container(label, hostAndPort):
    httpd = reversehttp.ReverseHttpServer(label,
                                          "http://" + hostAndPort + "/reversehttp",
                                          SmqpRequestHandler)
    container = SmqpServiceContainer(label + '.' + hostAndPort, httpd)
    return httpd, container

def test_relay(label = 'relay', hostAndPort = 'localhost.lshift.net:8000'):
    class R(SmqpRelay):
        def onSubscriberAdded(self, name, filter):
            print 'Subscription added:', name
        def onSubscriberRemoved(self, name):
            print 'Subscription removed:', name
        def onDeliveryFailure(self, message, target, errorReports):
            print 'Delivery failure:', target, errorReports
    httpd, container = test_container(label, hostAndPort)
    container.bindName(container.expandLocalname("relay"), R())
    httpd.serve_forever()

def test_pub(target = 'relay@relay.localhost.lshift.net:8000', body = 'testing'):
    SmqpMessage(None, target, body).deliver()

def test_pubmany(target = 'relay@relay.localhost.lshift.net:8000', body = 'testing'):
    counter = 0
    startTime = time.time()
    reportEvery = 1000
    while 1:
        SmqpMessage(None, target, body + str(counter)).deliver()
        counter = counter + 1
        if (counter % reportEvery) == 0:
            now = time.time()
            print counter, str(float(reportEvery) / (now - startTime)) + 'Hz'
            startTime = now

def test_sub(source = 'relay@relay.localhost.lshift.net:8000',
             hostAndPort = 'localhost.lshift.net:8000'):
    class S:
        def do_send(self, message):
            print message.sender,'->',message.target,message.contentType,':',message.body
    import random
    label = 'pythonsub' + str(int(random.uniform(0, 100000)))
    httpd, container = test_container(label, hostAndPort)
    qName = container.expandLocalname("queue")
    container.bindName(qName, S())
    req = jsonRequest(qName, source, 'subscribe', {'name': qName, 'filter': None})
    if not req.deliver():
        print 'Subscription failed!'
        print req.errorReports
        import sys
        sys.exit(1)
    print 'Subscribed...'
    httpd.serve_forever()

if __name__ == '__main__':
    import sys
    if len(sys.argv) <= 1:
        print 'Usage: smqp.py (relay|pub|pubmany|sub) [arg ...]'
        sys.exit(1)
    mode = sys.argv[1]
    if mode == 'relay':
        test_relay(*sys.argv[2:])
    elif mode == 'pub':
        test_pub(*sys.argv[2:])
    elif mode == 'pubmany':
        test_pubmany(*sys.argv[2:])
    elif mode == 'sub':
        test_sub(*sys.argv[2:])
    else:
        print 'Invalid mode %s' % (mode,)
