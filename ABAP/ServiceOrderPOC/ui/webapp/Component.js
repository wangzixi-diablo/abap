jQuery.sap.declare("service_poc.Component");
sap.ui.getCore().loadLibrary("sap.ui.generic.app");
jQuery.sap.require("sap.ui.generic.app.AppComponent");

sap.ui.generic.app.AppComponent.extend("service_poc.Component", {
	metadata: {
		"manifest": "json"
	}
});