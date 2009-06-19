function Url(maybeStr) {
    if (maybeStr instanceof Url) {
	this.url = maybeStr.url;
	this.protocol = maybeStr.protocol;
	this.username = maybeStr.username;
	this.password = maybeStr.password;
	this.host = maybeStr.host;
	this.port = maybeStr.port;
	this.pathname = maybeStr.pathname;
	this.querystring = maybeStr.querystring;
	this.fragment = maybeStr.fragment;
	return;
    } else {
	var r;
	if (maybeStr) {
	    r = maybeStr.toString().match(Url.regex);
	    if (!r) throw {error: "url_invalid", url: maybeStr};
	} else {
	    r = [null, null, null, null,
		 null, null, null, null,
		 null, null, null, null,
		 null, null, null, null,
		 null];
	}

	this.url = r[0];
	this.protocol = r[2];
	this.username = r[5];
	this.password = r[7];
	this.host = r[8] || "";
	this.port = r[10];
	this.pathname = r[11] || "";
	this.querystring = r[14] || "";
	this.fragment = r[16] || "";
    }
}

Url.regex = 
/*12       3    45     6 7         8          9 A        B   C                   D  E        F 0   */
/* proto         user    pass      host         port     path                       query      frag */
/^((\w+):)?(\/\/((\w+)?(:(\w+))?@)?([^\/\?:]+)(:(\d+))?)?(\/?([^\/\?#][^\?#]*)?)?(\?([^#]+))?(#(\w*))?/;

Url.prototype.getHostPort = function () {
    return this.host + (this.port ? ":" + this.port : "");
};

Url.prototype.getPathQuery = function () {
    return this.pathname + (this.querystring ? "?" + this.querystring : "");
};

Url.prototype.toString = function () {
    var r = [];
    if (this.protocol) { r.push(this.protocol + "://"); }
    if (this.username) {
	r.push(this.username);
	if (this.password) { r.push(this.password); }
	r.push("@");
    }
    if (this.host) { r.push(this.host); }
    if (this.port) { r.push(":" + this.port.toString()); }
    if (this.pathname) { r.push(this.pathname); }
    if (this.querystring) { r.push("?" + this.querystring); }
    if (this.fragment) { r.push("#" + this.fragment); }
    return r.join("");
};

function parse_qs(qs) {
    var result = {};
    if (qs) {
	var keyvals = qs.split('&');
	for (var i = 0; i < keyvals.length; i++) {
	    var eqPos = keyvals[i].indexOf('=');
	    if (eqPos == -1) {
		result[keyvals[i]] = true;
	    } else {
		result[keyvals[i].substr(0, eqPos)] = keyvals[i].substr(eqPos + 1);
	    }
	}
    }
    return result;
};

function unparse_qs(params) {
    result = [];
    for (var key in params) {
	result.push(key + "=" + params[key]);
    }
    return result.join("&");
};

function encode_utf8(s) {
    return unescape(encodeURIComponent(s));
}

function decode_utf8(s) {
    return decodeURIComponent(escape(s));
}

function parseHttpHeadersAndBody(o, sourceText) {
    o.headers = {};
    do {
	var tmp = sourceText.match(/([^:]+):[ \t]*([^\r\n]*)\r\n/);
	if (tmp != null) {
	    o.headers[tmp[1].toLowerCase()] = tmp[2];
	    sourceText = sourceText.substring(tmp[0].length);
	}
    } while (tmp != null);
    o.body = sourceText.substring(2);
}

var ReverseHttpAccessPoint = "/reversehttp";

function HttpRequest(replyUrl, sourceText) {
    this.replyUrl = replyUrl;
    var tmp = sourceText.match(/([^ ]+) ([^ ]+) HTTP\/([0-9]+\.[0-9]+)\r\n/);
    this.method = tmp[1].toLowerCase();
    this.rawPath = tmp[2];
    this.httpVersion = tmp[3];
    parseHttpHeadersAndBody(this, sourceText.substring(tmp[0].length));
    this.responseSent = false;
}

HttpRequest.prototype.respond = function (status, text, headers, body) {
    if (this.responseSent) {
	return;
    }

    var r = new HttpResponse(status, text, headers, body, this.httpVersion);
    new Ajax.Request(this.replyUrl,
		     { method: "post",
		       contentType: "message/http",
		       postBody: r.toString() });
    this.responseSent = true;
};

function formatHttpHeadersAndBody(lineList, headers, body) {
    var h = {};
    for (var key in headers) {
	h[key] = headers[key];
    }
    h["Content-length"] = encode_utf8(body).length;
    for (var key in h) {
	lineList.push(key + ": " + h[key]);
    }
    lineList.push("");
    lineList.push(body);
    return lineList.join("\r\n");
}

function HttpRelay(method, url, headers, body, options) {
    var $elf = this;

    this.method = method;
    this.url = new Url(url);
    this.headers = Object.extend({}, headers);
    this.headers["Host"] = this.url.getHostPort();
    this.body = body;
    this.response = null;

    this.options = {
	onComplete: null,
	onError: null,
	asynchronous: true,
	ajaxOptions: {}
    };
    Object.extend(this.options, options || {});

    var o = Object.extend({}, this.options.ajaxOptions);
    Object.extend(o, { method: "post",
		       contentType: "message/http",
		       postBody: this.toString(),
		       asynchronous: this.options.asynchronous,
		       onComplete: function (transport) { $elf.handleCompletion(transport); }
		     });
    this.request = new Ajax.Request(ReverseHttpAccessPoint + "/_relay/"+this.url.getHostPort(), o);
}

HttpRelay.prototype.handleCompletion = function (transport) {
    var status = transport.status;
    if ((status >= 200 && status < 300) ||
	(status == 1223 /* MSIE returns this sometimes instead of 204! */)) {
	this.response = parseHttpResponse(transport.responseText);
	if (this.options.onComplete) this.options.onComplete(this.response, this, status);
    } else {
	if (this.options.onError) this.options.onError(null, this, status);
    }
};

HttpRelay.prototype.isOk = function () {
    if (!this.response) return null;
    var status = this.response.status;
    return ((status >= 200 && status < 300) ||
	    (status == 1223 /* MSIE returns this sometimes instead of 204! */));
};

HttpRelay.prototype.toString = function () {
    var h;
    if (this.url.username == null) {
	h = this.headers;
    } else {
	throw "Unimplemented -- need base64 support for authentication";
	h = Object.extend({"Authorization":
			     "Basic " + b64enc(this.url.username + ":" + this.url.password)
			  }, this.headers);
    }
    return formatHttpHeadersAndBody([this.method + " " + this.url.getPathQuery() + " HTTP/1.0"],
				    h,
				    this.body);
};

function parseHttpResponse(sourceText) {
    var tmp = sourceText.match(/HTTP\/([0-9]+\.[0-9]+) ([0-9]+) ([^\r\n]*)\r\n/);
    var r = new HttpResponse(Number(tmp[2]), tmp[3], null, null, tmp[1]);
    parseHttpHeadersAndBody(r, sourceText.substring(tmp[0].length));
    return r;
}

function HttpResponse(status, text, headers, body, httpVersion) {
    this.status = status;
    this.text = text;
    this.headers = headers;
    this.body = body;
    this.httpVersion = httpVersion || "1.0";
}

HttpResponse.prototype.toString = function () {
    return formatHttpHeadersAndBody(["HTTP/" + this.httpVersion + " " +
				     this.status + " " + this.text],
				    this.headers,
				    this.body);
};

function parseLinkHeaders(s) {
    var result = {};
    if (s != null) {
	var headerValues = s.split(", ");
	for (var i = 0; i < headerValues.length; i++) {
	    var linkHeader = headerValues[i];
	    var pieces = linkHeader.split(";");
	    var url;
	    var rel;
	    for (var j = 0; j < pieces.length; j++) {
		var piece = pieces[j];
		var m = piece.match(/<\s*(\S+)\s*>/);
		if (m != null) {
		    url = m[1];
		} else {
		    m = piece.match(/(\w+)="(\w*)"/);
		    if (m != null) {
			if (m[1].toLowerCase() == "rel") {
			    rel = m[2];
			}
		    }
		}
	    }
	    if (rel && url) {
		result[rel] = url;
	    }
	}
    }
    return result;
}

function HttpServer(label, callback, options) {
    var $elf = this;

    this.label = label;
    this.failureDelay = 2000;
    this.callback = callback;
    this.options = {
	token: "-",
	debug: function () {},
	log: function () { this.debug.apply(this, arguments); },
	onLocationChanged: function () {}
    };
    Object.extend(this.options, options || {});

    this.running = true;
    this.nextReq = null;
    this.location = null;
    this.pollRequest = null;

    // Avoid endlessly-spinning loading-indicator in Safari.
    setTimeout(function () {
		   $elf.options.debug("Declaring label " + label);
		   $elf.serve();
	       }, 250);
}

HttpServer.prototype.stop = function () {
    this.running = false;
    if (this.pollRequest) {
	if (this.pollRequest.transport) {
	    this.pollRequest.transport.abort();
	}
	this.pollRequest = null;
    }
};

HttpServer.prototype.repeatWithBackoff = function (requestBuilder, responseHandler) {
    var $elf = this;

    if (!$elf.running) {
	return;
    }

    this.pollRequest = requestBuilder(receiveReply);

    function receiveReply(ajaxRequest) {
	this.pollRequest = null;

	if (!$elf.running) {
	    return;
	}

	if ((ajaxRequest.status < 200 || ajaxRequest.status >= 300) &&
	    (ajaxRequest.status != 1223 /* MSIE returns this sometimes instead of 204! */))
	{
	    $elf.options.debug("Poll request failed - status " + ajaxRequest.status +
			       "; delaying " + $elf.failureDelay);
	    setTimeout(function () { $elf.serve(); }, $elf.failureDelay);
	    if ($elf.failureDelay < 30000) {
		$elf.failureDelay = $elf.failureDelay * 2;
	    }
	    return;
	}

	if ($elf.failureDelay != 2000) {
	    $elf.options.debug("Recovered; resetting delay");
	}
	$elf.failureDelay = 2000;

	responseHandler(ajaxRequest);
    }
};

HttpServer.prototype.serve = function () {
    var $elf = this;

    var declareMode = ($elf.nextReq == null);
    $elf.repeatWithBackoff(requestBuilder, responseHandler);

    function requestBuilder(receiveReply) {
	if (declareMode) {
	    return new Ajax.Request(ReverseHttpAccessPoint,
				    { method: "post",
				      onComplete: receiveReply,
				      parameters: {"name": $elf.label,
						   "token": $elf.options.token} });
	} else {
	    return new Ajax.Request($elf.nextReq,
				    { method: "get",
				      requestHeaders: ['Accept', 'message/http'],
				      onComplete: receiveReply });
	}
    }

    function responseHandler(ajaxRequest) {
	if (declareMode) {
	    var linkHeaders = parseLinkHeaders(ajaxRequest.getResponseHeader("Link"));
	    $elf.nextReq = linkHeaders["first"];
	    var locationText = linkHeaders["related"];
	    if (locationText) {
		$elf.location = locationText;
		$elf.options.onLocationChanged(locationText, $elf);
	    }
	    $elf.options.debug("Label " + $elf.label + " maps to " + $elf.location);
	    $elf.options.debug("First request is at " + $elf.nextReq);
	} else {
	    var requestSourceText = ajaxRequest.responseText;
	    if (requestSourceText) {
		try {
		    var clientHostAndPort = ajaxRequest.getResponseHeader("Requesting-Client");
		    var httpReq = new HttpRequest($elf.nextReq, requestSourceText);
		    $elf.nextReq = parseLinkHeaders(ajaxRequest.getResponseHeader("Link"))["next"];
		    $elf.options.log(httpReq.headers["host"] + " " + httpReq.method + " " + httpReq.rawPath);
		    try {
			$elf.callback(httpReq);
		    } catch (userException) {
			$elf.options.log("HTTPD CALLBACK ERROR: " + Object.toJSON(userException));
			httpReq.respond(500, {}, "httpd.js callback internal server error");
		    }
		} catch (catchallException) {
		    $elf.options.log("HTTPD ERROR: " + Object.toJSON(catchallException));
		}
	    }
	}
	if (Prototype.Browser.IE) {
	    // IE's stack runs out when things are busy if we don't do this.
	    setTimeout(function () { $elf.serve(); }, 0);
	} else {
	    $elf.serve();
	}
    }
};
