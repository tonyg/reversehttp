if (!document.domain) switchAccessPoint("http://www.reversehttp.net/reversehttp");

function log() {
    for (var i = 0; i < arguments.length; i++) {
	var arg = arguments[i];
	if (typeof(arg) == 'string') {
	    $("#logOutput").append(document.createTextNode(arg + "\r\n"));
	} else {
	    $("#logOutput").append(document.createTextNode(Object.toJSON(arg) + "\r\n"));
	}
    }
}

function formatErrorReports(errorReports) {
    var r = [];
    for (var i = 0; i < errorReports.length; i++) {
	var x = errorReports[i];
	r.push({status1: x.relay ? x.relay.status : null,
		message1: x.relay? x.relay.responseText : null,
		status2: x.response ? x.response.status : null,
		message2: x.response ? x.response.body : null});
    }
    return Object.toJSON(r);
}

function makeLoggingDeliverOptions(what, k) {
    return {
	onComplete: function () {
	    log(what + " complete!");
	    if (k) {
		k();
	    }
	},
	onAborted: function (msg, options, errorReports) {
	    log(what + " aborted! " + formatErrorReports(errorReports));
	}
    };
}

function logMessage(m) {
    log(m.sender + " -> " + m.target + " " + m.contentType + ": " + m.body);
}
