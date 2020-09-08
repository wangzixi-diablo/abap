@AbapCatalog.sqlViewName: 'ziorder'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'order consumption view'

define view Z_i_Order_View as select from Z_C_Order_Item 
left outer join Zorder_Sys_Status as _sys_status 
on Z_C_Order_Item.guid = _sys_status.guid
left outer join Z_c_partner as _partner
on Z_C_Order_Item.guid = _partner.guid
left outer join Z_I_Start_Date as _start
on Z_C_Order_Item.guid = _start.order_guid
left outer join Z_I_end_Date as _end
on Z_C_Order_Item.guid = _end.order_guid
{
   key Z_C_Order_Item.guid as order_guid,
   Z_C_Order_Item.object_id,
   Z_C_Order_Item.description,
   Z_C_Order_Item.posting_date,
   _partner.name,
   _partner.partner_no,
   _partner.title,
   _partner.partner_id,
   _sys_status.stat,
   _sys_status.txt04,
   _sys_status.txt30,
   _start.requested_start,
   _end.requested_end,
   @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
   Z_C_Order_Item._Item,
   @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
   _sys_status._statushelp,
   @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
   _sys_status._statusfixedvalue,
   @ObjectModel.association.type: [#TO_COMPOSITION_CHILD]
   _partner._accounthelp
}