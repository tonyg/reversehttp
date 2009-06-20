function sub_clicked() {
    sub_or_unsub("subscribe");
}

function unsub_clicked() {
    sub_or_unsub("unsubscribe");
}

function sub_or_unsub(verb) {
    var s = new Messaging.RemoteSource($("#source_url").attr("value"));

    if ($("#token_generation_required").attr("checked")) {
	var e = new Messaging.RemoteEndpoint($("#sink_url").attr("value"));
	log("Generating token...");
	e.generate_token(verb, function (t) {
			     log("Generated token: " + t);
			     $("#sink_token").attr("value", t || "");
			     if (t) k();
			 });
    } else {
	k();
    }

    function k() {
	s[verb]($("#sink_url").attr("value"),
		$("#sub_topic").attr("value"),
		["sync", "async"],
		$("#sink_token").attr("value"),
		function (result) {
		    log("Action '" + verb + "' result: " + result);
		});
    }
}

function send_clicked() {
    var e = new Messaging.RemoteEndpoint($("#target_url").attr("value"));
    e.deliver($("#pub_topic").attr("value"), $("#pub_message").attr("value"), "text/plain",
	      function (result) {
		  log("Send attempt result: " + result);
	      });
}

function pshb_main() {
}
