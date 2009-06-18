var Messaging = {};

Messaging.Server = function (delegationLabel, options) {
    var $elf = this;

    var o = {
	serverOptions: {},
	onLocationChanged: function () {}
    };
    Object.extend(o, options || {});

    this.delegationLabel = delegationLabel;
    this.onLocationChanged = o.onLocationChanged;
    this.serverOptions = o.serverOptions;

    o.serverOptions.onLocationChanged = function (locationText) {
	$elf.locationChanged(locationText);
	$elf.onLocationChanged(locationText, $elf);
    };

    function respondTo(httpReq) { return $elf.respondTo(httpReq); }

    this.pathMap = {};
    this.server = new HttpServer(delegationLabel, respondTo, o.serverOptions);
};

Messaging.Server.prototype.stop = function () {
    this.server.stop();
};

Messaging.Server.prototype.locationChanged = function (locationText) {
    this.location = new Url(locationText);
    this.pathPrefix = this.location.pathname;
};

Messaging.Server.prototype.respondTo = function (httpReq) {
    function k(status, body, contentType) {
	var msg = Messaging.StatusMessages[status] || "Unknown";
	if (status == 204) {
	    httpReq.respond(status, msg,
			    {}, "");
	} else {
	    httpReq.respond(status, msg,
			    {"Content-type": (contentType || "text/plain")}, body || msg);
	}
    }

    if (httpReq.rawPath.substr(0, this.pathPrefix.length) != this.pathPrefix) {
	k(404, "Destination not found");
	return;
    }

    var reqUrl = new Url(httpReq.rawPath.substr(this.pathPrefix.length));

    var slashPos = reqUrl.pathname.indexOf('/');
    var target;
    var remainder;
    if (slashPos == -1) {
	target = reqUrl.pathname;
	remainder = '';
    } else {
	target = reqUrl.pathname.substr(0, slashPos);
	remainder = reqUrl.pathname.substr(slashPos + 1);
    }

    var params = parse_qs(reqUrl.querystring);

    try {
	if (this.pathMap[target]) {
	    var recipient = this.pathMap[target];
	    if (recipient[httpReq.method]) {
		recipient[httpReq.method](httpReq, remainder, params, k);
	    } else {
		if (recipient.FALLBACK) {
		    recipient.FALLBACK(httpReq, remainder, params, k);
		} else {
		    k(501);
		}
	    }
	} else {
	    k(404);
	}
    } catch (e) {
	k(500);
    }
};

Messaging.Server.prototype.bindName = function (name, receiver) {
    this.pathMap[name] = receiver;
};

Messaging.Server.prototype.unbindName = function (name) {
    delete this.pathMap[name];
};

Messaging.StatusMessages = {
    200: "OK",
    204: "OK",
    400: "Invalid request",
    403: "Forbidden",
    404: "Destination not found",
    500: "Internal messaging server error",
    501: "Unsupported method"
};

Messaging.HubModeDispatch = function (httpReq, path, params, k) {
    var hubMode = params['hub.mode'] || "";
    var mname = httpReq.method + '_' + hubMode;
    if (this[mname]) {
	this[mname](httpReq, path, params, k);
    } else {
	k(501);
    }
};

Messaging.EndpointFacet = function (sink) {
    this.sink = sink;
};

Messaging.EndpointFacet.MaxAge = 300; // seconds

Messaging.EndpointFacet.prototype.FALLBACK = Messaging.HubModeDispatch;

Messaging.EndpointFacet.prototype.generate_token = function (path, intendedUse) {
    return (new Number(new Date())) + ":" + intendedUse + ":" + path;
};

Messaging.EndpointFacet.prototype.check_token = function (token, path, actualUse) {
    var m = token.match(/^([0-9]+):([^:]+):(.*)/);
    if (!m) return false;
    var tokenTime = Number(m[1]);
    if (((new Number(new Date())) - tokenTime) > (Messaging.EndpointFacet.MaxAge * 1000)) {
	return false;
    }
    if (actualUse != m[2]) return false;
    if (path != m[3]) return false;
    return this.sink.check_action(actualUse, path);
};

Messaging.EndpointFacet.prototype.check_http_token =
    function (httpReq, path, params, k, actualUse) {
	if (this.check_token(params['hub.verify_token'] || "", path, actualUse)) {
	    k(204);
	} else {
	    k(400);
	}
    };

Messaging.EndpointFacet.prototype.get_ = function (httpReq, path, params, k) {
    k(200, "Endpoint facet");
};

Messaging.EndpointFacet.prototype.get_subscribe = function (httpReq, path, params, k) {
    this.check_http_token(httpReq, path, params, k, "subscribe");
};

Messaging.EndpointFacet.prototype.get_unsubscribe = function (httpReq, path, params, k) {
    this.check_http_token(httpReq, path, params, k, "unsubscribe");
};

Messaging.EndpointFacet.prototype.get_generate_token = function (httpReq, path, params, k) {
    var token = this.generate_token(path, params['hub.intended_use']);
    k(200, "hub.verify_token=" + token, "application/x-www-form-urlencoded");
};

Messaging.EndpointFacet.prototype.post = function (httpReq, path, params, k) {
    this.sink.deliver(params['hub.topic'] || '',
		      httpReq.headers['content-type'],
		      httpReq.body);
    k(204);
};

Messaging.HubModeRequest = function (url, method, mode, params) {
    var qs = unparse_qs(params);
    return new HttpRelay(method,
			 url + "?hub.mode=" + mode + (qs ? "&"+qs : ""),
			 {},
			 "",
			 {asynchronous: false});
};

Messaging.RemoteEndpoint = function (url) {
    this.url = url;
};

Messaging.RemoteEndpoint.prototype.generate_token = function (intended_use) {
    var r = Messaging.HubModeRequest(this.url, "GET", "generate_token",
				     {"hub.intended_use": intended_use});
    if (!r.isOk()) return null;
    var m = r.response.body.match(/^hub\.verify_token=(.*)$/);
    if (!m) return null;
    return m[1];
};

Messaging.RemoteEndpoint.prototype.check_token = function (token, actual_use) {
    var r = Messaging.HubModeRequest(this.url, "GET", actual_use,
				     {"hub.verify_token": token});
    return r.isOk();
};

Messaging.RemoteEndpoint.prototype.deliver = function (topic, body, contentType) {
    var r = new HttpRelay("POST", this.url + "?hub.topic=" + topic,
			  contentType ? {"Content-type": contentType} : {},
			  body,
			  {asynchronous: false});
    return r.isOk();
};

Messaging.RemoteSource = function (url) {
    this.url = url;
};

Messaging.RemoteSource.prototype.subscribe = function (callbackUrl, topic, verifyModes, token) {
    var r = Messaging.HubModeRequest(this.url, "POST", "subscribe",
				     {"hub.callback": callbackUrl,
				      "hub.topic": topic,
				      "hub.verify": verifyModes.join(","),
				      "hub.verify_token": token});
    return r.isOk();
};

Messaging.RemoteSource.prototype.unsubscribe = function (callbackUrl, topic, verifyModes, token) {
    var r = Messaging.HubModeRequest(this.url, "POST", "unsubscribe",
				     {"hub.callback": callbackUrl,
				      "hub.topic": topic,
				      "hub.verify": verifyModes.join(","),
				      "hub.verify_token": token});
    return r.isOk();
};
