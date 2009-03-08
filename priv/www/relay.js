var server;
var relay;
var currentRelayName;

function updateSubscriptions() {
    log("Subscriptions changed.");
    var e = $("subscriptions");
    var s = [];
    for (var a in relay.subscribers) {
	s.push("<li>--&gt; " + a + "</li>");
    }
    e.innerHTML = s.join("");
}

function relay_main() {
    server = new Messaging.Server("relay", {serverOptions: {debug: log}});
    $("hostPart").innerHTML = server.host;
    relay = new Messaging.Relay({ onSubscriberAdded: updateSubscriptions,
				  onSubscriberRemoved: updateSubscriptions,
				  onDeliveryFailure: function(m, s, e) {
				      log("Delivery failure to " + s +
					  ": " + formatErrorReports(e));
				  }
				});
    log("Listening for SMQP at " + server.host);
    log("HTTP at " + server.server.vhostName);
    setAppName();
}

function setAppName() {
    if (currentRelayName) { server.unbindName(currentRelayName); }
    currentRelayName = server.expandLocalname($("appName").value);
    if (currentRelayName) {
	log("Relay at " + currentRelayName);
	var nest = relay.getMessageHandler();
	server.bindName(currentRelayName, {
			    send: function (m, k) {
				logMessage(m);
				nest.send(m, k);
			    },
			    subscribe: function (m, k) {
				log("SUBSCRIBE " + m.body);
				nest.subscribe(m, k);
			    },
			    unsubscribe: function (m, k) {
				log("UNSUBSCRIBE " + m.body);
				nest.unsubscribe(m, k);
			    }
			});
    }
}
