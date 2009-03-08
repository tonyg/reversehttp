function sendMessage() {
    new Messaging.Message(null, $("targetName").value, $("messageBody").value, "text/plain")
    .deliver(makeLoggingDeliverOptions("send"));
}

function pub_main() {
    $("targetName").value =
	$("targetName").value.replace("???", new Url(window.location).getHostPort());
}