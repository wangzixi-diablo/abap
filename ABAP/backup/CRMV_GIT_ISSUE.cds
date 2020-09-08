@AbapCatalog.sqlViewName: 'CRMGIT'
@AbapCatalog.compiler.compareFilter: true
//@VDM.viewType: #BASIC
@ClientHandling.algorithm: #SESSION_VARIABLE
@EndUserText.label: 'ABAP github repository'
define view CRMV_GIT_ISSUE
/*  with parameters
    @Environment.systemField: #SYSTEM_LANGUAGE
    P_Language : spras*/
  as select from    crmd_git_issue {
  //CRMD_GIT_ISSUE 
  repo_name,
  count( * ) as issueCount
  } group by repo_name