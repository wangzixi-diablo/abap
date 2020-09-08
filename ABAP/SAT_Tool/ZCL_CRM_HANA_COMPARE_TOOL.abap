class ZCL_CRM_HANA_COMPARE_TOOL definition
  public
  final
  create public .

public section.

  class-methods SORT_NET .
  class ZCL_CRM_HANA_TOOL definition load .
  type-pools SLIS .
  class-methods COMPARE
    importing
      !IV_HN1_KEY type SATR_TAB_KEY
      !IV_Q2U_KEY type SATR_TAB_KEY
      !IV_HN1_DATA type ZCL_CRM_HANA_TOOL=>TT_DATA
      !IV_Q2U_DATA type ZCL_CRM_HANA_TOOL=>TT_DATA
      !IV_YELLOW type I
      !IV_TRACE_OBJ type SATR_DIRECTORY-OBJ_NAME
      !IV_GREEN type I
    exporting
      !OUT_HEADER type SLIS_T_LISTHEADER .
  class-methods DISPLAY_ALV .
  class-methods SORT .
  class-methods DISPLAY_SOURCE
    importing
      !IV_FIELD type SLIS_SELFIELD .
protected section.
private section.

  types:
    BEGIN OF ty_data,
         icon TYPE icon-id,
         EBENE TYPE SATR_AUSTAB_GESAMT-EBENE,
         HIER_FELD TYPE SATR_AUSTAB_GESAMT-HIER_FELD,
         q2u_BRUTTO TYPE SATR_AUSTAB_GESAMT-BRUTTO,
         hn1_BRUTTO TYPE SATR_AUSTAB_GESAMT-BRUTTO,
         q2u_net TYPE SATR_AUSTAB_GESAMT-NETTO,
         hn1_net TYPE SATR_AUSTAB_GESAMT-NETTO,
         faster type string,
         netfaster type string,
         fast_abs type i,
         fast_net type i, " including sign
         color(4) TYPE c,
  END OF ty_data .
  types:
    tt_data TYPE STANDARD TABLE OF ty_data .

  class-data MT_RESULT type TT_DATA .
  class-data MV_Q2U_KEY type SATR_TAB_KEY .
  class-data MV_HN1_KEY type SATR_TAB_KEY .
  class-data MT_Q2U_DATA type ZCL_CRM_HANA_TOOL=>TT_DATA .
  class-data MT_HN1_DATA type ZCL_CRM_HANA_TOOL=>TT_DATA .
  class-data MV_TRACE_OBJ type SATR_DIRECTORY-OBJ_NAME .
  class-data MV_HEADER type SLIS_T_LISTHEADER .
  class-data MV_GREEN type I .
  class-data MV_YELLOW type I .
  class-data MT_COLOR type STRING_TABLE .
  class-data MV_TOOL type ref to CL_ATRA_TOOL_SE30_MAIN .

  class-methods CAL_RATE
    importing
      !IV_HN1_TIME type SATR_AUSTAB_GESAMT-BRUTTO
      !IV_Q2U_TIME type SATR_AUSTAB_GESAMT-BRUTTO
    changing
      !CV_LINE type TY_DATA .
  class-methods CAL_RATE_NET
    importing
      !IV_HN1_TIME type SATR_AUSTAB_GESAMT-NETTO
      !IV_Q2U_TIME type SATR_AUSTAB_GESAMT-NETTO
    changing
      !CV_LINE type TY_DATA .
  class-methods GET_HEADER
    returning
      value(OUT_HEADER) type SLIS_T_LISTHEADER .
  class-methods INIT
    importing
      !IV_Q2U_KEY type SATR_TAB_KEY
      !IV_HN1_KEY type SATR_TAB_KEY
      !IV_Q2U_DATA type ZCL_CRM_HANA_TOOL=>TT_DATA
      !IV_HN1_DATA type ZCL_CRM_HANA_TOOL=>TT_DATA
      !IV_YELLOW type I
      !IV_GREEN type I .
  type-pools ABAP .
  class-methods CHECK_HEADER
    returning
      value(OUT_VALID) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_CRM_HANA_COMPARE_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_COMPARE_TOOL=>CAL_RATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_HN1_TIME                    TYPE        SATR_AUSTAB_GESAMT-BRUTTO
* | [--->] IV_Q2U_TIME                    TYPE        SATR_AUSTAB_GESAMT-BRUTTO
* | [<-->] CV_LINE                        TYPE        TY_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CAL_RATE.
  DATA: lv_diff type p,
        lv_color_index type i,
        lv_temp type string.

  CHECK iv_hn1_time <> 0.

  lv_diff = ( iv_q2u_time - iv_hn1_time ) * 100 / iv_q2u_time.
  cv_line-fast_abs = lv_diff.
  IF lv_diff < 0.
     lv_diff = abs( lv_diff ).
     lv_temp = lv_diff.
     lv_temp = '-' && lv_temp.
     "cv_line-icon = ICON_RED_LIGHT.
  ELSEIF lv_diff >= mv_green.
     lv_temp = lv_diff.
     "cv_line-icon = ICON_GREEN_LIGHT.
  ELSEIF lv_diff < mv_green AND lv_diff >= mv_yellow.
     lv_temp = lv_diff.
     "cv_line-icon = ICON_YELLOW_LIGHT.
  ELSE.
     lv_temp = lv_diff.
     "cv_line-icon = ICON_RED_LIGHT.
  ENDIF.

  cv_line-faster = lv_temp && '%'.

  lv_color_index = ( cv_line-EBENE MOD 7 ) + 1.
  READ TABLE mt_color INTO cv_line-color INDEX lv_color_index.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_COMPARE_TOOL=>CAL_RATE_NET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_HN1_TIME                    TYPE        SATR_AUSTAB_GESAMT-NETTO
* | [--->] IV_Q2U_TIME                    TYPE        SATR_AUSTAB_GESAMT-NETTO
* | [<-->] CV_LINE                        TYPE        TY_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CAL_RATE_NET.
  DATA: lv_diff type p,
        lv_temp type string.

  CHECK iv_hn1_time <> 0.

  lv_diff = ( iv_q2u_time - iv_hn1_time ) * 100 / iv_q2u_time.
  cv_line-fast_abs = lv_diff.
  IF lv_diff < 0.
     lv_diff = abs( lv_diff ).
     lv_temp = lv_diff.
     lv_temp = '-' && lv_temp.
     cv_line-icon = ICON_RED_LIGHT.
  ELSEIF lv_diff >= mv_green.
     lv_temp = lv_diff.
     cv_line-icon = ICON_GREEN_LIGHT.
  ELSEIF lv_diff < mv_green AND lv_diff >= mv_yellow.
     lv_temp = lv_diff.
     cv_line-icon = ICON_YELLOW_LIGHT.
  ELSE.
     lv_temp = lv_diff.
     cv_line-icon = ICON_RED_LIGHT.
  ENDIF.

  cv_line-netfaster = lv_temp && '%'.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_COMPARE_TOOL=>CHECK_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OUT_VALID                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CHECK_HEADER.

  out_valid = abap_false.

  IF mv_yellow >= mv_green.
     WRITE: / 'Invalid Threshold input. Green must > Yellow.' COLOR COL_NEGATIVE.
     RETURN.
  ENDIF.

  IF NOT mv_hn1_key-CPROG CS 'hn1'.
     WRITE:/ 'Please upload a valid SAT trace file for HN1.' COLOR COL_NEGATIVE.
     RETURN.
  ENDIF.

*  IF NOT mv_q2u_key-CPROG CS 'q2u'.
*     WRITE:/ 'Please upload a valid SAT trace file for Q2U.' COLOR COL_NEGATIVE.
*     RETURN.
*  ENDIF.

  out_valid = abap_true.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_COMPARE_TOOL=>COMPARE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_HN1_KEY                     TYPE        SATR_TAB_KEY
* | [--->] IV_Q2U_KEY                     TYPE        SATR_TAB_KEY
* | [--->] IV_HN1_DATA                    TYPE        ZCL_CRM_HANA_TOOL=>TT_DATA
* | [--->] IV_Q2U_DATA                    TYPE        ZCL_CRM_HANA_TOOL=>TT_DATA
* | [--->] IV_YELLOW                      TYPE        I
* | [--->] IV_TRACE_OBJ                   TYPE        SATR_DIRECTORY-OBJ_NAME
* | [--->] IV_GREEN                       TYPE        I
* | [<---] OUT_HEADER                     TYPE        SLIS_T_LISTHEADER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method COMPARE.

    DATA: ls_q2u_data TYPE ZCL_CRM_HANA_TOOL=>ty_data,
          ls_hn1_data LIKE ls_q2u_data,
          ls_line TYPE ty_data.

    mv_trace_obj = iv_trace_obj.
    CALL METHOD INIT
       EXPORTING
          iv_hn1_key  = iv_hn1_key
          iv_q2u_key  = iv_q2u_key
          iv_hn1_data = iv_hn1_data
          iv_q2u_data = iv_q2u_data
          iv_green    = iv_green
          iv_yellow   = iv_yellow.

    CHECK check_header( ) = abap_true.

    get_header( ).

    LOOP AT iv_q2u_data INTO ls_q2u_data.
       READ TABLE iv_hn1_data INTO ls_hn1_data WITH KEY EBENE = ls_q2u_data-ebene HIER_FELD = ls_q2u_data-HIER_FELD index = ls_q2u_data-index.
       CHECK sy-subrc = 0.
       ls_line-ebene      = ls_q2u_data-ebene.
       ls_line-HIER_FELD  = ls_q2u_data-HIER_FELD.
       ls_line-q2u_BRUTTO = ls_q2u_data-BRUTTO.
       ls_line-hn1_BRUTTO = ls_hn1_data-BRUTTO.
       ls_line-q2u_net    = ls_q2u_data-netto.
       ls_line-hn1_net    = ls_hn1_data-netto.
       cal_rate( EXPORTING iv_q2u_time = ls_q2u_data-BRUTTO iv_hn1_time = ls_hn1_data-BRUTTO CHANGING cv_line = ls_line ).
       cal_rate_net( EXPORTING iv_q2u_time = ls_q2u_data-NETTO iv_hn1_time = ls_hn1_data-NETTO CHANGING cv_line = ls_line ).
       ls_line-fast_net   = ls_hn1_data-netto - ls_q2u_data-netto.
       APPEND ls_line TO mt_result.
    ENDLOOP.

    out_header = mv_header.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_COMPARE_TOOL=>DISPLAY_ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method DISPLAY_ALV.

  CHECK mt_result IS NOT INITIAL.
  DATA:

  ls_fieldcat TYPE LINE OF slis_t_fieldcat_alv,
  it_fieldcat TYPE slis_t_fieldcat_alv,
  ls_layout TYPE slis_layout_alv.

  ls_fieldcat-fieldname     = 'ICON'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-seltext_l     = 'Status'.
  ls_fieldcat-seltext_m     = 'Status'.
  ls_fieldcat-seltext_s     = 'Status'.
  ls_fieldcat-icon = 'X'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'EBENE'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-seltext_l     = 'Call Stack Level'.
  ls_fieldcat-seltext_m     = 'Call Stack Level'.
  ls_fieldcat-seltext_s     = 'Call Stack Level'.
  ls_fieldcat-outputlen     = 18.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'HIER_FELD'.
  ls_fieldcat-seltext_l     = 'Code Name'.
  ls_fieldcat-seltext_m     = 'Code Name'.
  ls_fieldcat-seltext_s     = 'Code Name'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'Q2U_BRUTTO'.
  ls_fieldcat-seltext_s     = 'Q2U'.
  ls_fieldcat-seltext_m     = 'Q2U Gross'.
  ls_fieldcat-seltext_l     = 'Q2U Gross Time'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'HN1_BRUTTO'.
  ls_fieldcat-seltext_s     = 'HN1'.
  ls_fieldcat-seltext_m     = 'HN1 Gross'.
  ls_fieldcat-seltext_l     = 'HN1 Gross Time'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'FASTER'.
  ls_fieldcat-seltext_l     = 'Faster(Gross)'.
  ls_fieldcat-seltext_m     = 'Faster(Gross)'.
  ls_fieldcat-seltext_s     = 'Faster(Gross)'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'Q2U_NET'.
  ls_fieldcat-seltext_s     = 'Q2U'.
  ls_fieldcat-seltext_m     = 'Q2U Net'.
  ls_fieldcat-seltext_l     = 'Q2U Net Time'.
  ls_fieldcat-do_sum        = 'X'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'HN1_NET'.
  ls_fieldcat-seltext_s     = 'HN1'.
  ls_fieldcat-seltext_m     = 'HN1 Net'.
  ls_fieldcat-seltext_l     = 'HN1 Net Time'.
  ls_fieldcat-do_sum        = 'X'.

  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'NETFASTER'.
  ls_fieldcat-seltext_s     = 'Faster(Net)'.
  ls_fieldcat-seltext_m     = 'Faster(Net)'.
  ls_fieldcat-seltext_l     = 'Faster(Net)'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'FAST_NET'.
  ls_fieldcat-seltext_l     = 'Net Difference'.
  ls_fieldcat-seltext_m     = 'Net Difference'.
  ls_fieldcat-seltext_s     = 'Net Difference'.
  ls_fieldcat-do_sum        = 'X'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_layout-zebra             = 'X'.
  ls_layout-cell_merge        = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-info_fieldname  = 'COLOR'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-cprog
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      is_layout                = ls_layout
      I_CALLBACK_TOP_OF_PAGE   = 'CALLBACK'
      it_fieldcat              = it_fieldcat[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = mt_result.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_COMPARE_TOOL=>DISPLAY_SOURCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FIELD                       TYPE        SLIS_SELFIELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
method DISPLAY_SOURCE.
  DATA: ls_line LIKE LINE OF MT_Q2U_DATA,
        ls_index LIKE LINE OF mv_tool->IT_TRACEPROG.

  READ TABLE MT_Q2U_DATA INTO ls_line WITH KEY hier_feld = iv_field-VALUE.

  CHECK sy-subrc = 0.

  mv_tool->show_abap_source( trace_prog_index = ls_line-progindex
                                 cont_offset      = ls_line-contoffs ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_COMPARE_TOOL=>GET_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OUT_HEADER                     TYPE        SLIS_T_LISTHEADER
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_HEADER.
   data:
      wa_header type slis_listheader,
      t_line like wa_header-info,
      ld_lines type i,
      lv_green type string,
      lv_yellow type string,
      ld_linesc(10) type c.


  wa_header-typ  = 'H'.
  wa_header-info = 'Trace Report Name: ' && mv_trace_obj.
  append wa_header to mv_header.
  clear wa_header.

* Date
  wa_header-typ  = 'S'.
  wa_header-key = 'HN1 Trace Date:'.
  CONCATENATE  mv_hn1_key-datum+6(2) '.'
               mv_hn1_key-datum+4(2) '.'
               mv_hn1_key-datum(4) INTO wa_header-info.   "todays date

  CONCATENATE wa_header-info ` Trace Time: ` mv_hn1_key-UZEIT+0(2) ':'  mv_hn1_key-UZEIT+2(2)
   ':' mv_hn1_key-UZEIT+4(2) INTO wa_header-info.
  append wa_header to mv_header.
  clear: wa_header.

  wa_header-typ  = 'S'.
  wa_header-key = 'Q2U Trace Date:'.
  CONCATENATE  mv_q2u_key-datum+6(2) '.'
               mv_q2u_key-datum+4(2) '.'
               mv_q2u_key-datum(4) INTO wa_header-info.   "todays date

  CONCATENATE wa_header-info ` Trace Time: ` mv_q2u_key-UZEIT+0(2) ':'  mv_q2u_key-UZEIT+2(2)
   ':' mv_q2u_key-UZEIT+4(2) INTO wa_header-info.
  append wa_header to mv_header.
  clear: wa_header.

  wa_header-typ  = 'S'.
  wa_header-key = ''.
  append wa_header to mv_header.

  wa_header-typ  = 'S'.
  wa_header-key = 'Green Light:'.
  lv_green = mv_green.
  lv_yellow = mv_yellow.
  lv_green = lv_green && '%'.
  lv_yellow = lv_yellow && '%'.
  CONCATENATE ` HN1 is AT LEAST ` lv_green ` faster than Q2U.` INTO wa_header-info.
  append wa_header to mv_header.

  wa_header-typ  = 'S'.
  wa_header-key  = 'Yellow Light:'.
  CONCATENATE ` HN1 is LESS THAN ` lv_green ` faster but AT LEAST ` lv_yellow ` faster.` INTO wa_header-info.
  append wa_header to mv_header.


  wa_header-typ  = 'S'.
  wa_header-key  = 'Red Light:'.
  wa_header-info = 'HN1 is NOT FASTER than Q2U!!!'.
  append wa_header to mv_header.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_COMPARE_TOOL=>INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_Q2U_KEY                     TYPE        SATR_TAB_KEY
* | [--->] IV_HN1_KEY                     TYPE        SATR_TAB_KEY
* | [--->] IV_Q2U_DATA                    TYPE        ZCL_CRM_HANA_TOOL=>TT_DATA
* | [--->] IV_HN1_DATA                    TYPE        ZCL_CRM_HANA_TOOL=>TT_DATA
* | [--->] IV_YELLOW                      TYPE        I
* | [--->] IV_GREEN                       TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method INIT.
   DATA: ls_key        type SATR_DIRECTORY,
         lv_key        type SATR_TAB_KEY,
         lv_log        type TBDLS-LOGSYS,
         lv_trace_date type SATR_DIRECTORY-trace_Date,
         lv_trace_time type SATR_DIRECTORY-trace_time.

   CLEAR: mt_result, mv_hn1_key, mv_q2u_key, mt_hn1_data, mt_q2u_data, mv_header,mt_color, mv_tool.

   mv_hn1_key  = iv_hn1_key.
   mv_q2u_key  = iv_q2u_key.
   mt_hn1_data = iv_hn1_data.
   mt_q2u_data = iv_q2u_data.
   mv_yellow   = iv_yellow.
   mv_green    = iv_green.

  APPEND 'C100' TO mt_color.
  APPEND 'C200' TO mt_color.
  APPEND 'C300' TO mt_color.
  APPEND 'C400' TO mt_color.
  APPEND 'C500' TO mt_color.
  APPEND 'C600' TO mt_color.
  APPEND 'C700' TO mt_color.

  call FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
       OWN_LOGICAL_SYSTEM = lv_log.

  IF lv_log CS 'Q2U'.
     lv_trace_date = iv_q2u_key-DATUM.
     lv_trace_time = iv_q2u_key-UZEIT.
  ELSEIF lv_log CS 'HN1'.
     lv_trace_date = iv_hn1_key-DATUM.
     lv_trace_time = iv_hn1_key-UZEIT.
  ELSE.
     lv_trace_date = '20120626'.
     lv_trace_time = '100807'.
  ENDIF.

  select single * INTO ls_key FROM SATR_DIRECTORY where trace_date = lv_trace_date and trace_time = lv_trace_time.
  ASSERT sy-subrc = 0.

  lv_key-CPROG  = ls_key-SATR_KEY.
  lv_key-datum  = lv_trace_date.
  lv_key-uzeit  = lv_trace_time.

  call method CL_ATRA_TOOL_SE30_MAIN=>CREATE_OBJECT
    EXPORTING
       P_CONTAINER_KEY = lv_key
       P_TO_DO         = abap_true
       P_WITH_DB_TIMES = abap_true
       p_index         = 0
    IMPORTING
       EO_REF_TO_MAIN = mv_tool.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_COMPARE_TOOL=>SORT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method SORT.
  sort mt_result BY FAST_abs.
  display_alv( ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_COMPARE_TOOL=>SORT_NET
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method SORT_NET.
  sort mt_result BY fast_net DESCENDING.
  display_alv( ).
endmethod.
ENDCLASS.