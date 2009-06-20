function sub_clicked() {
    sub_or_unsub("subscribe");
}

function unsub_clicked() {
    sub_or_unsub("unsubscribe");
}

function sub_or_unsub(verb) {
    var s = new Messaging.RemoteSource($("source_url").value);

    if ($("token_generation_required").checked) {
	var e = new Messaging.RemoteEndpoint($("sink_url").value);
	log("Generating token...");
	e.generate_token(verb, function (t) {
			     log("Generated token: " + t);
			     $("sink_token").value = t || "";
			     if (t) k();
			 });
    } else {
	k();
    }

    function k() {
	s[verb]($("sink_url").value, $("sub_topic").value, ["sync", "async"],
		$("sink_token").value,
		function (result) {
		    log("Action '" + verb + "' result: " + result);
		});
    }
}

function send_clicked() {
    var e = new Messaging.RemoteEndpoint($("target_url").value);
    e.deliver($("pub_topic").value, $("pub_message").value, "text/plain",
	      function (result) {
		  log("Send attempt result: " + result);
	      });
}

function pshb_main() {
}
