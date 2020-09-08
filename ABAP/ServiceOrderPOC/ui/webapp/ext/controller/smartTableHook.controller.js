sap.ui.controller("service_poc.ext.controller.smartTableHook", {


	onInitSmartFilterBarExtension: function(oEvent) {
		debugger;
		//the custom field in the filter bar might have to be bound to a custom data model
		// if a value change in the field shall trigger a follow up action, this method is the place to define and bind an event handler to the field	
		/*
		When you have defined a view extension, then you can access and modify the properties of all ui elements defined within these extensions (e.g. change the visibility programmatically).
        However, access to ui elements not defined within own view extensions is not allowed.

		*/
	},

	onBeforeRebindTableExtension: function(oEvent) {
		// usually the value of the custom field should have an effect on the selected data in the table. So this is the place to add a binding parameter depending on the value in the custom field.
        debugger;
	}
});