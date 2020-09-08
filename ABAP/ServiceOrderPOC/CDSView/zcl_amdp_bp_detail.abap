CLASS zcl_amdp_bp_detail DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_amdp_marker_hdb.

  CLASS-METHODS crmd_partner_but000 FOR TABLE FUNCTION ztf_bp_Detail.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_amdp_bp_detail IMPLEMENTATION.
METHOD crmd_partner_but000
         BY DATABASE FUNCTION FOR HDB
         LANGUAGE SQLSCRIPT
         OPTIONS READ-ONLY
         USING crmd_partner but000.
    RETURN SELECT sc.client as client,
                  sc.partner_guid as partner_guid,
                  sc.guid as partset_guid,
                  sc.partner_no as partner_no,
                  sp.partner_guid as bp_guid,
                  sp.title as title,
                  sp.name1_text as name,
                  sp.partner as partner_id
                  FROM crmd_partner AS sc
                    INNER JOIN but000 AS sp ON sc.client = sp.client AND
                                               sc.partner_no = sp.partner_guid
                    WHERE sc.client = :clnt AND
                          sc.partner_fct = '00000001'
                    ORDER BY sc.client;
  ENDMETHOD.
ENDCLASS.