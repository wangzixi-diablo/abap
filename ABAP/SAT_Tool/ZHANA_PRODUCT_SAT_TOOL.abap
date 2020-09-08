*&---------------------------------------------------------------------*
*& Report  ZHANA_PRODUCT_SAT_TOOL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZHANA_PRODUCT_SAT_TOOL.
PARAMETERS: date type SATR_DIRECTORY-TRACE_DATE OBLIGATORY DEFAULT '20120620',
            "time type SATR_DIRECTORY-trace_time OBLIGATORY DEFAULT '124732',
            time type SATR_DIRECTORY-trace_time OBLIGATORY DEFAULT '070809',
            code type string OBLIGATORY DEFAULT 'CRM_PRODUCT_GETLIST2',
            xml type string OBLIGATORY DEFAULT 'C:\Users\i042416\Desktop\TestJerry.xml'.
            "code type string OBLIGATORY DEFAULT 'ZCRM_PRODUCT_DUMMY'.

DATA: lt_alv TYPE ZCL_CRM_HANA_TOOL=>tt_data,
      ls_key TYPE SATR_TAB_KEY.

AT SELECTION-SCREEN.
   CALL METHOD ZCL_CRM_HANA_TOOL=>GET_XML_FILE_PATH
      EXPORTING
        IV_CODE       = code
        IV_TRACE_DATE = date
        IV_TRACE_TIME = time
      IMPORTING
        rv_result     = xml.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR xml.
   CALL METHOD ZCL_CRM_HANA_TOOL=>VALUE_HELP_FOR_XML_PATH
      CHANGING
         cv_xml_path = xml.

START-OF-SELECTION.

CALL METHOD ZCL_CRM_HANA_TOOL=>ASSEMBLE_DATA
   EXPORTING
      iv_code         = code
      IV_trace_date   = date
      iv_trace_time   = time
   IMPORTING
      out_data        = lt_alv
      out_key         = ls_key.

CHECK lt_alv IS NOT INITIAL.


  CALL METHOD ZCL_CRM_HANA_TOOL=>download_xml
    EXPORTING
       iv_data       = lt_alv
       iv_key        = ls_key
       iv_xml_path   = xml.

  CALL METHOD ZCL_CRM_HANA_TOOL=>display_alv
    CHANGING
       ct_data       = lt_alv.