@AbapCatalog.sqlViewName: 'zcpartner'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'partner detail'
define view Z_c_partner as select from crmd_orderadm_h
inner join crmd_link as _link on crmd_orderadm_h.guid = _link.guid_hi and _link.objtype_hi = '05'
   and _link.objtype_set = '07'
inner join ztf_bp_detail( clnt: '001') as _bp on _link.guid_set = _bp.partset_guid
association [0..1] to Z_C_Account_Valuehelp as _accounthelp 
  on  $projection.partner_id = _accounthelp.partner
{
   key crmd_orderadm_h.guid,
   --_link.objtype_hi as header_type,
   --_link.objtype_set as item_type,
   _bp.bp_guid,
   _bp.partner_no,
   _bp.name,
   _bp.partner_id,
   case _bp.title 
     when '0001' then 'Ms.'
     when '0002' then 'Mr.'
     when '0003' then 'Company'
     when '0004' then 'Mr and Mrs'
     else 'Unknown'
   end as title,
   @ObjectModel.association.type: #TO_COMPOSITION_CHILD
   _accounthelp
}