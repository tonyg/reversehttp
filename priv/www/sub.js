var msgServer;
var subscriptions = {};

function addSubscription(a, t) {
    subscriptions[a] = t;
    updateSubscriptions();
}

function delSubscription(a) {
    delete subscriptions[a];
    updateSubscriptions();
}

function updateSubscriptions() {
    var e = $("subscriptions");
    var s = [];
    for (var a in subscriptions) {
	s.push("<li>" + a + " --&gt; " + subscriptions[a] + "</li>");
    }
    e.innerHTML = s.join("");
}

function subscribe_clicked() {
    var subAddr = $("sourceName").value;
    var target = msgServer.expandLocalname("queue");
    Messaging.jsonRequest(target, subAddr, "subscribe", {name: target})
    .deliver(makeLoggingDeliverOptions("subscription",
				       function () { addSubscription(subAddr, target); }));
}

function unsubscribe_clicked(subAddr) {
    Messaging.jsonRequest(null, subAddr, "unsubscribe", {name: msgServer.expandLocalname("queue")})
    .deliver(makeLoggingDeliverOptions("unsubscription",
				       function () { delSubscription(subAddr); }));
}

function sub_main() {
    $("sourceName").value =
	$("sourceName").value.replace("???", new Url(window.location).getHostPort());

    var subName = "sub" + Math.round(Math.random() * 100000);
    msgServer = new Messaging.Server(subName, {serverOptions: {debug: log}});
    msgServer.bindName(msgServer.expandLocalname("queue"),
		       {
			   send: function (m, k) {
			       log(m.sender + " -> " + m.target + " " + m.contentType + ": " + m.body);
			       k(200);
			   }
		       });
}