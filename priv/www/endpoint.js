var msgServer;

function endpoint_main() {
    function updateLocation(locationText) {
	var u = locationText + "ep";
        $("#endpointLocation").attr("href", u);
        $("#endpointLocation").text(u);
    }

    var subName = "sub" + Math.round(Math.random() * 100000);
    msgServer = new Messaging.Server(subName, {serverOptions: {debug: log},
                                               onLocationChanged: updateLocation});

    msgServer.bindName("ep",
		       new Messaging.EndpointFacet(
			   {
                               check_action: function (actualUse, path) {
				   log("check_action: " + actualUse + ", " + path);
				   return true;
                               },
                               deliver: function (topic, contentType, body) {
				   log(topic + " (" + contentType + "): " + body);
                               }
			   }));
}
