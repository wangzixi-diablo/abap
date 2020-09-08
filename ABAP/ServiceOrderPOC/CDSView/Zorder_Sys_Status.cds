@AbapCatalog.sqlViewName: 'zsystatus'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'system status'
define view Zorder_Sys_Status as select from crmd_orderadm_h

inner join crm_jest as _crm_jest on crmd_orderadm_h.guid = _crm_jest.objnr
inner join Z_C_Status_Text as _status_text on _crm_jest.stat = _status_text.istat 
association [0..1] to Z_C_Status_Valuehelp as _statushelp 
  on  $projection.stat = _statushelp.status_key
association [0..1] to Z_C_Status_Fixedvalue as _statusfixedvalue 
  on  $projection.txt04 = _statusfixedvalue.status_code
                                                          
{
   key crmd_orderadm_h.guid,
   //@ObjectModel: { foreignKey.association: '_statushelp', mandatory: true }
   key _crm_jest.stat,
   _status_text.txt04,
   _status_text.txt30,

   @ObjectModel.association.type: #TO_COMPOSITION_CHILD
   _statushelp,
   @ObjectModel.association.type: #TO_COMPOSITION_CHILD
   _statusfixedvalue
}
/*where _crm_jest.inact = '' and _crm_jest.stat like 'I%' and _crm_jest.stat <> 'I1030' // has error 
and _crm_jest.stat <> 'I1026'; // do not transfer*/

where _crm_jest.inact = '' and ( _crm_jest.stat = 'I1002 ' or 
_crm_jest.stat = 'I1003' or _crm_jest.stat = 'I1005' )
/*I1002 open
I1003 in process
I1005 complete*/

