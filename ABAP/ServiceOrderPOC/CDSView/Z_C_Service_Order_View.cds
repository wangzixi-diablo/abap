@AbapCatalog.sqlViewName: 'zcorderview'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'service order consumption view'
@Search.searchable: true

@OData.publish: false
@ObjectModel: {
   compositionRoot: true,
   type: #CONSUMPTION,
   transactionalProcessingDelegated: true,
   createEnabled,  
   deleteEnabled, 
   updateEnabled 
}

@UI.headerInfo:{
  typeName:       'Service Order',
  typeNamePlural: 'Service Orders',
  title: { value: 'object_id' },
  description: { value: 'description' }
}

define view Z_C_Service_Order_View as select from Z_i_Order_View 
{
  key Z_i_Order_View.order_guid as order_guid,
  @UI.lineItem : [{position:10, label : 'Service Order ID'}]
  // @UI.identification: [ { position: 10 } ]
  @UI.selectionField: [ { position: 20, label : 'Service Order ID' } ]
  Z_i_Order_View.object_id,
  
  @UI.lineItem : [{position:20, label : 'Description'}]
  @UI.identification: [ { position: 20,  label : 'Description' } ]
  @Search:{ defaultSearchElement: true, ranking: #HIGH, fuzzinessThreshold: 0.8 }
  //@UI.selectionField: { position: 4, element: '_ProductCategory.MainProductCategory' }
  @UI.selectionField: [ { position: 10, label : 'Description' } ]
  Z_i_Order_View.description,
  
  Z_i_Order_View.partner_no,
  
  // Jerry2016-04-21: this is partner's title
  Z_i_Order_View.title,
  
  @UI.identification: [ { position: 20 } ]
  @ObjectModel.readOnly: true 
  Z_i_Order_View.posting_date,
  
  @UI.lineItem : [{position:30, label : 'Account'}]
  //@UI.identification: [ { position: 30, label : 'Account' } ]
  @ObjectModel.readOnly: true 
  // Jerry 2016-04-21 14:31PM this is partner name
  Z_i_Order_View.name,
    
  @UI.identification: [ { position: 35, label : 'Account id' } ]
  @Consumption.valueHelp: '_accounthelp'
  Z_i_Order_View.partner_id,
  
  // Jerry 2016-04-21 14:33PM do not display status value help  
  // @UI.identification: [ { position: 40 } ]
  //@ObjectModel: { foreignKey.association: '_statushelp', mandatory: true }
  @Consumption.valueHelp: '_statushelp'
  Z_i_Order_View.stat,
  
  @UI.identification: [ { position: 50 } ]
  @Consumption.valueHelp: '_statusfixedvalue'
  //@ObjectModel: { foreignKey.association: '_statusfixedvalue', mandatory: true }
  @ObjectModel.readOnly: true 
  Z_i_Order_View.txt04,
  
  @UI.lineItem : [{position:40}]
  //@UI.identification: [ { position: 40 } ]
  Z_i_Order_View.txt30,
  
  @UI.identification: [ { position: 60 } ]
  @EndUserText.label: 'Requested Start Date'
  Z_i_Order_View.requested_start,
  
  @UI.identification: [ { position: 70 } ]
  @EndUserText.label: 'Requested End Date'
  Z_i_Order_View.requested_end,
  
  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
  Z_i_Order_View._Item,
  
  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
  Z_i_Order_View._statushelp,
  
  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
  Z_i_Order_View._statusfixedvalue,
  
  @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
  Z_i_Order_View._accounthelp
}