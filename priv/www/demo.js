var demoServer = null;

function respondTo(httpReq) {
    var body = $("#replyText").attr("value");
    httpReq.respond(200, "OK", {"Content-type": "text/html"}, body);
}

var demoLabel_keypress_timeout = null;

function demoLabel_changed() {
    begin_serving();
}

function demoLabel_keypress() {
    if (demoLabel_keypress_timeout != null) {
	clearTimeout(demoLabel_keypress_timeout);
	demoLabel_keypress_timeout = null;
    }
    demoLabel_keypress_timeout = setTimeout(demoLabel_changed, 300);
}

function demo_main() {
    var initialValue = "demo" + Math.round(Math.random() * 100000);
    $("#demoLabel").attr("value", initialValue);
    begin_serving();
}

function begin_serving() {
    var label = $("#demoLabel").attr("value");
    if (demoServer == null || demoServer.label != label) {
	if (demoServer != null) { demoServer.stop(); }
	demoServer = new HttpServer(label, respondTo, {onLocationChanged: updateLocation,
						       debug: log});
    }

    function updateLocation(newLoc) {
	$("#demoLabel_link").attr("href", newLoc);
	$("#demoLabel_link").text(newLoc);
    }
}
