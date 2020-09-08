@AbapCatalog.sqlViewName: 'zorderitem'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'order item detail'
define view Z_C_Order_Item as select from crmd_orderadm_h 
inner join crmc_proc_type as _type on crmd_orderadm_h.process_type = 
     _type.process_type and _type.object_type = 'BUS2000116'
     
// 2016-04-22 16:37PM debug
// and crmd_orderadm_h.created_by = 'WANGJER'
     
association [0..*] to Z_I_Item_Detail as _Item 
on $projection.guid = _Item.header
{
   key crmd_orderadm_h.guid,
   key crmd_orderadm_h.object_id,
   crmd_orderadm_h.description,
   crmd_orderadm_h.posting_date,
   @ObjectModel.association.type: #TO_COMPOSITION_CHILD
  _Item
}