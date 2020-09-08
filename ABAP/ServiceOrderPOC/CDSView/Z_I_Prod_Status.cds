@AbapCatalog.sqlViewName: 'zprdstatus'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'product guid and status code'
define view Z_I_Prod_Status as select from crmd_product_i 
inner join crm_jest as _status on crmd_product_i.guid = _status.objnr
inner join Z_C_Status_Text as _text on _status.stat = _text.istat
{
   key crmd_product_i.guid,
   _status.stat,
   _text.txt04,
   _text.txt30
}

where _status.inact = '' and _status.stat like 'I%' and _status.stat <> 'I1030';