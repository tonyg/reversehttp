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
	$("sink_token").value = e.generate_token(verb) || "";
    }
    log("Action '" + verb + "' result: " +
	s[verb]($("sink_url").value, $("sub_topic").value, ["sync", "async"],
		$("sink_token").value));
}

function send_clicked() {
    var e = new Messaging.RemoteEndpoint($("target_url").value);
    log("Send attempt result: " +
	e.deliver($("pub_topic").value, $("pub_message").value, "text/plain"));
}

function pshb_main() {
}
