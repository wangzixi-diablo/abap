@AbapCatalog.sqlViewName: 'CRMGITCRDATES'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Count on Issue Creation Date'
define view CRMV_ISSUE_CREATION_DATE_COUNT as select from 
CRMV_ISSUE_CREATION_DATE as date {
   date.repo_name,
   date.createdDate,
   count ( * ) as issueCreatedPerDay
} group by repo_name, createdDate 