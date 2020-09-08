*&---------------------------------------------------------------------*
*& Report  ZHANA_PRODUCT_SAT_COMPARE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZHANA_PRODUCT_SAT_COMPARE.
PARAMETERS: hn1 type string OBLIGATORY DEFAULT 'C:\Users\i042416\Desktop\HN1.xml',
            q2u type string OBLIGATORY DEFAULT 'C:\Users\i042416\Desktop\Q2U.xml',
            green type i OBLIGATORY DEFAULT 50,
            yellow type i OBLIGATORY DEFAULT 20.

data: ls_hn1_key type SATR_TAB_KEY,
      ls_q2u_key type SATR_TAB_KEY,
      lt_hn1_data type ZCL_CRM_HANA_TOOL=>TT_DATA,
      lt_q2u_data type ZCL_CRM_HANA_TOOL=>TT_DATA,
      lv_hn1_obj type SATR_DIRECTORY-obj_name,
      lv_q2u_obj type SATR_DIRECTORY-obj_name,
      ls_header type slis_t_listheader.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR hn1.
   CALL METHOD ZCL_CRM_HANA_TOOL=>VALUE_HELP_FOR_XML_PATH
      CHANGING
         cv_xml_path = hn1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR q2u.
   CALL METHOD ZCL_CRM_HANA_TOOL=>VALUE_HELP_FOR_XML_PATH
      CHANGING
         cv_xml_path = q2u.

START-OF-SELECTION.

call method ZCL_CRM_HANA_XML_TOOL=>upload_xml( iv_xml_path = hn1 ).
call method ZCL_CRM_HANA_XML_TOOL=>get_output
  IMPORTING
     out_header         = ls_hn1_key
     out_trace_data     = lt_hn1_data
     out_trace_obj_name = lv_hn1_obj.

call method ZCL_CRM_HANA_XML_TOOL=>upload_xml( iv_xml_path = q2u ).
call method ZCL_CRM_HANA_XML_TOOL=>get_output
  IMPORTING
     out_header         = ls_q2u_key
     out_trace_data     = lt_q2u_data
     out_trace_obj_name = lv_q2u_obj.

IF lv_q2u_obj <> lv_hn1_obj.
   WRITE:/ 'The trace on two systems are not done on the same report!' COLOR COL_NEGATIVE.
   RETURN.
ENDIF.

call method ZCL_CRM_HANA_COMPARE_TOOL=>compare
   EXPORTING
     iv_hn1_key     = ls_hn1_key
     iv_hn1_data    = lt_hn1_data
     iv_q2u_key     = ls_q2u_key
     iv_q2u_data    = lt_q2u_data
     iv_green       = green
     iv_yellow      = yellow
     iv_trace_obj   = lv_q2u_obj
   IMPORTING
     out_header     = ls_header.

call method ZCL_CRM_HANA_COMPARE_TOOL=>display_alv.

FORM callback.

  call function 'REUSE_ALV_COMMENTARY_WRITE'
       exporting
            it_list_commentary = ls_header.
ENDFORM.

FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSTANDARD'.
ENDFORM. "Set_pf_status

FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
    CASE sy-ucomm.
      WHEN 'QUIT' OR 'EXIT' OR 'BACK'.
          LEAVE PROGRAM.
      WHEN 'SORT'.
          ZCL_CRM_HANA_COMPARE_TOOL=>sort( ).
      WHEN 'SORTHN1'.
          ZCL_CRM_HANA_COMPARE_TOOL=>sort_net( ).
      WHEN OTHERS.
          ZCL_CRM_HANA_COMPARE_TOOL=>display_source( rs_selfield ).
    ENDCASE.

ENDFORM.  "User_command