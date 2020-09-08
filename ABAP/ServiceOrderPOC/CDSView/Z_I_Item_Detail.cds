@AbapCatalog.sqlViewName: 'zitemdetail'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'item detail'
define view Z_I_Item_Detail as select from crmd_orderadm_i 
inner join crmd_schedlin as _schedule 
  on crmd_orderadm_i.guid = _schedule.item_guid
inner join crmd_product_i as _prod on crmd_orderadm_i.guid = _prod.guid
inner join Z_I_Prod_Status as _prd_status
  on crmd_orderadm_i.guid = _prd_status.guid
{
   key crmd_orderadm_i.guid,
   key crmd_orderadm_i.header,
   @UI.lineItem : [{position:10, label : 'Item Number'}]
   crmd_orderadm_i.number_int,
   @UI.lineItem : [{position:20, label : 'Product Name'}]
   crmd_orderadm_i.description,
   @UI.lineItem : [{position:30, label : 'Quantity'}]
   _schedule.quantity,
   @UI.lineItem : [{position:40, label : 'Unit'}]
   _prod.process_qty_unit as unit,
   _prd_status.stat,
   _prd_status.txt04,
   @UI.lineItem : [{position:50}]
   _prd_status.txt30
   
}