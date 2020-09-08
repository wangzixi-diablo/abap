@AbapCatalog.sqlViewName: 'zstatustext'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'system status code and description Jerry'
define view Z_C_Status_Text as select from tj02 inner join tj02t as _tj02t on 
 tj02.istat = _tj02t.istat and _tj02t.spras = 'E'                                
{
   key tj02.istat,
   _tj02t.txt04,
   _tj02t.txt30
}
