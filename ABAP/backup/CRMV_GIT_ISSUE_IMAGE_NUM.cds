@AbapCatalog.sqlViewName: 'CRMGITIMNUM'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'issue image number overview'
@ClientHandling.algorithm: #SESSION_VARIABLE
define view CRMV_GIT_ISSUE_IMAGE_NUM as select from crmd_git_image
as image inner join crmd_git_issue as issue on image.repo_name
 = issue.repo_name and image.issue_num = issue.issue_num{
  image.repo_name, 
  image.issue_num,
  issue.created_at,
  count(*) as imageCount,
  issue.title
} group by image.repo_name,image.issue_num, issue.title, issue.created_at