var Messaging = {};

Messaging._nameToAddress = function (name) {
    var r = name.match(/^(([^@]+)@)?([^@]+)/);
    if (!r) {
	throw {error: "name_invalid", name: name};
    }
    return "http://" + r[3] + "/" + (r[2] || "");
};

Messaging.Message = function(sender, target, body, contentType, method) {
    this.sender = sender;
    this.target = target;
    this.body = body;
    this.contentType = contentType || "text/plain";
    this.method = method || "send";
};

Messaging.Message.prototype.retarget = function (sender, target) {
    return new Messaging.Message(sender, target, this.body, this.contentType, this.method);
};

Messaging.Message.prototype.deliver = function (options) {
    var $elf = this;

    var o = {
	onComplete: null,
	onRetry: null,
	onAborted: null,
	username: null,
	password: null,
	initialFailureDelay: 2000,
	failureDelayLimit: 30000,
	maxAttemptCount: 3,
	failureDelayMultiplier: 2
    };
    Object.extend(o, options || {});

    var address = Messaging._nameToAddress(this.target);
    var attemptCount = 0;
    var errorReports = [];
    var failureDelay = o.initialFailureDelay;

    var headers = {'Content-type': this.contentType};
    if (this.sender) {
	headers['x-smqp-sender'] = this.sender;
    }
    if (this.method) {
	headers['x-smqp-method'] = this.method;
    }

    var requestOptions = (o.onComplete || o.onAborted) ? {onComplete: handleCompletion} : null;

    function failNow() {
	if (o.onAborted) {
	    o.onAborted(this, options, errorReports);
	}
    }

    function attemptDelivery() {
	if (attemptCount >= o.maxAttemptCount) {
	    failNow();
	} else {
	    attemptCount++;
	    new HttpRelay("POST", address, headers, $elf.body, requestOptions);
	}
    }

    function retryDelivery() {
	if (o.onRetry) {
	    o.onRetry(this, options, errorReports);
	}
	attemptDelivery();
    }

    function handleCompletion(req) {
	if (req.status == 200) {
	    var resp = parseHttpResponse(req.responseText);
	    if (resp.status >= 200 && resp.status < 300) {
		if (o.onComplete) {
		    o.onComplete(this, options, errorReports);
		}
	    } else {
		handleFailure({status: resp.status, relay: req, response: resp});
	    }
	} else {
	    handleFailure({status: req.status, relay: req, response: null});
	}
    }

    function handleFailure(report) {
	errorReports.push(report);
	if (report.status >= 500 && report.status < 600) {
	    setTimeout(retryDelivery, failureDelay);
	    if (failureDelay < o.failureDelayLimit) {
		failureDelay = failureDelay * o.failureDelayMultiplier;
	    }
	} else {
	    failNow();
	}
    }

    attemptDelivery(0);
};

Messaging.Server = function (vhostPrefix, options) {
    var $elf = this;

    var o = {
	baseUrl: window.location,
	onInvalidName: null,
	serverOptions: {}
    };
    Object.extend(o, options || {});

    this.listenAddress = new Url(o.baseUrl);
    this.listenAddress.host = vhostPrefix + "." + this.listenAddress.host;
    this.host = this.listenAddress.getHostPort();

    this.onInvalidName = o.onInvalidName;
    this.serverOptions = o.serverOptions;

    function respondTo(httpReq) { return $elf.respondTo(httpReq); }

    this.pathMap = {};
    this.server = new HttpServer(vhostPrefix, respondTo, o.serverOptions);
};

Messaging.Server.prototype.stop = function () {
    this.server.stop();
};

Messaging.Server.prototype.expandLocalname = function (localName) {
    return localName ? localName + "@" + this.host : this.host;
};

Messaging.Server.prototype.respondTo = function (httpReq) {
    var targetLocalname = httpReq.rawPath.substring(1);
    var target = this.expandLocalname(targetLocalname);
    var sender = httpReq.headers["x-smqp-sender"] || null;
    var contentType = httpReq.headers["content-type"] || null;
    var method = httpReq.headers["x-smqp-method"] || "send";
    var m = new Messaging.Message(sender, target, httpReq.body, contentType, method);

    function k(status) {
	httpReq.respond(status, Messaging.StatusMessages[status] || "Unknown", {}, "");
    }

    try {
	if (this.pathMap[targetLocalname]) {
	    this.pathMap[targetLocalname][method](m, k);
	} else {
	    if (this.onInvalidName) {
		status = this.onInvalidName[method](m, k);
	    } else {
		k(404);
	    }
	}
    } catch (e) {
	k(500);
    }
};

Messaging.Server.prototype.extractLocalname = function (name) {
    var suffixLength = this.host.length + 1;
    var prefixLength = name.length - suffixLength;
    if (name.substring(prefixLength) == ("@" + this.host)) {
	return name.substring(0, prefixLength);
    } else {
	throw {error: "invalid_application_name", name: name, host: this.host};
    }
};

Messaging.Server.prototype.bindName = function (name, receiver) {
    this.pathMap[this.extractLocalname(name)] = receiver;
};

Messaging.Server.prototype.unbindName = function (name) {
    delete this.pathMap[this.extractLocalname(name)];
};

Messaging.StatusMessages = {
    200: "OK",
    404: "Destination not found",
    500: "Internal messaging server error"
};

Messaging.Relay = function (options) {
    this.options = Object.extend({
				     vetSubscriber: null,
				     onSubscriberAdded: null,
				     onSubscriberRemoved: null,
				     onDeliveryFailure: null,
				     checkDeliveryAcceptance: null
				 }, options);
    this.subscribers = {};
};

Messaging.Relay.prototype.getCallback = function (name, defaultValue) {
    return this.options[name] || defaultValue;
};

Messaging.Relay.prototype.invokeCallback = function (name, args, defaultValue) {
    if (this.options[name]) {
	try {
	    return this.options[name].apply(this, args);
	} catch (e) {
	    return defaultValue;
	}
    } else {
	return defaultValue;
    }
};

Messaging.Relay.prototype.addSubscriber = function (name, filter) {
    if (!this.invokeCallback("vetSubscriber", [name, filter], true)) {
	return false;
    }
    this.subscribers[name] = filter;
    this.invokeCallback("onSubscriberAdded", [name, filter]);
    return true;
};

Messaging.Relay.prototype.removeSubscriber = function (name) {
    delete this.subscribers[name];
    this.invokeCallback("onSubscriberRemoved", [name]);
};

Messaging.Relay.prototype.matchingSubscribers = function (message) {
    var result = {};

    for (var name in this.subscribers) {
	var filter = this.subscribers[name];
	if (filter(message)) {
	    result[name] = filter;
	}
    }

    return result;
};

Messaging.Relay.prototype.decodeFilter = function (spec) {
    return this.defaultFilter(); // TODO -- simple filter language?
};

Messaging.Relay.prototype.defaultFilter = function () {
    return function (m) { return true; };
};

Messaging.jsonRequest = function (sender, target, method, body) {
    return new Messaging.Message(sender, target, Object.toJSON(body), "application/json", method);
};

Messaging.jsonHandler = function (handler) {
    return function (message, k) {
	handler(message.body.evalJSON(), message);
	k(200);
    };
};

Messaging.Relay.prototype.getMessageHandler = function () {
    var $elf = this;
    return {
	send: function (message, k) { $elf.acceptMessage(message, k); },
	subscribe: Messaging.jsonHandler(function (args) {
					     var f = args.filter
						 ? $elf.decodeFilter(args.filter)
						 : $elf.defaultFilter();
					     $elf.addSubscriber(args.name, f);
					 }),
	unsubscribe: Messaging.jsonHandler(function (args) {
					       $elf.removeSubscriber(args.name);
					   })
    };
};

Messaging.Relay.prototype.acceptMessage = function (message, k) {
    var subs = this.matchingSubscribers(message);
    this.deliver(message, subs, k);
};

Messaging.Relay.prototype.deliver = function (message, subs, k) {
    var $elf = this;

    var completeCount = 0;
    var abortedCount = 0;
    var totalCount = 0;
    var continuationFired = false;

    function ok() {
	completeCount++;
	checkCompletion();
    }

    function fail(sub) {
	return function (m, deliveryOptions, errorReports) {
	    $elf.invokeCallback("onDeliveryFailure", [message, sub, errorReports]);
	    $elf.removeSubscriber(sub);
	    abortedCount++;
	    checkCompletion();
	};
    }

    function defaultCheckDeliveryAcceptance(completeCount, abortedCount, totalCount) {
	if ((totalCount == 0) || (completeCount > 0)) { return 200; }
	if (abortedCount == totalCount) { return 200; /* absorb failures */ }
	return null;
    }

    function checkCompletion() {
	if (continuationFired) {
	    return;
	}

	var checker =
	    $elf.getCallback("checkDeliveryAcceptance", defaultCheckDeliveryAcceptance);
	var completionStatus = checker(completeCount, abortedCount, totalCount);
	if (completionStatus) {
	    k(completionStatus);
	    continuationFired = true;
	}
    }

    for (var sub in subs) { totalCount++; }
    for (var sub in subs) {
	message.retarget(message.target, sub).deliver({ onComplete: ok,
							onAborted: fail(sub) });
    }
    checkCompletion();
};
