
@ClientDependent: true
@AccessControl.authorizationCheck: #NOT_REQUIRED 
define table function ztf_BP_DETAIL 
  with parameters @Environment.systemField: #CLIENT 
                  clnt:abap.clnt
  returns { client:s_mandt;
            partner_guid:BU_PARTNER_GUID;
            partset_guid:CRMT_OBJECT_GUID;
            partner_no: CRMT_PARTNER_NO;
            bp_guid: BU_PARTNER_GUID;
            title:AD_TITLE;
            name: BU_NAME1TX;
            partner_id:BU_PARTNER;
          } 
  implemented by method 
    zcl_amdp_bp_detail=>crmd_partner_but000;