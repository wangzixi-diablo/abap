class zCL_TOOL_SRC_CODE_LOCATION definition
  public
  inheriting from zCL_TOOL_SRC_CODE__ANALYSIS
  create public .

public section.

  types:
*"* public components of class CL_TOOL_SRC_CODE_LOC
*"* do not include other source files here!!!
    BEGIN OF ty_ms_result.
        INCLUDE TYPE ty_ms_result_common AS common.
        TYPES:
        num_lines_total   TYPE i,
        num_lines_code    TYPE i,
        num_lines_comment TYPE i,
        num_lines_blank   TYPE i.
        include type ty_ms_result_list.
      types END OF ty_ms_result .
  types:
    ty_mt_result TYPE STANDARD TABLE OF ty_ms_result .

  methods ZIF_TOOL_SRC_CODE__ANALYSIS~INIT
    redefinition .
protected section.

*"* protected components of class CL_TOOL_SRC_CODE_LOC
*"* do not include other source files here!!!
  data MR_RESULT type ref to DATA .

  methods ME_ANALYZE
    redefinition .
private section.
*"* private components of class CL_TOOL_SRC_CODE_LOC
*"* do not include other source files here!!!

  methods COUNT_LINES
    importing
      !IT_SOURCE type RSWSOURCET
    exporting
      !EV_CODE_LINES type I
      !EV_TOT_LINES type I
      !EV_COMMENT_LINES type I
      !EV_BLANK_LINES type I .
ENDCLASS.



CLASS ZCL_TOOL_SRC_CODE_LOCATION IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TOOL_SRC_CODE_LOCATION->COUNT_LINES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SOURCE                      TYPE        RSWSOURCET
* | [<---] EV_CODE_LINES                  TYPE        I
* | [<---] EV_TOT_LINES                   TYPE        I
* | [<---] EV_COMMENT_LINES               TYPE        I
* | [<---] EV_BLANK_LINES                 TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
method COUNT_LINES.

  DATA:
    lv_offset TYPE i,
    lv_substr1 TYPE string,
    lv_substr2 TYPE string.

  FIELD-SYMBOLS:
    <ls_source> TYPE LINE OF rswsourcet.

* INIT RESULTS
  ev_tot_lines     = 0.
  ev_comment_lines = 0.
  ev_code_lines    = 0.
  ev_blank_lines   = 0.

* BODY
  LOOP AT it_source ASSIGNING <ls_source>.
    ADD 1 TO ev_tot_lines.
*   Blank line
    IF <ls_source> IS INITIAL.
      ADD 1 TO ev_blank_lines.
      CONTINUE.
    ENDIF.
*   Comment lines
*   - starting with *
*   - containing only blanks followed by "
    IF <ls_source>(1) = '*'.
      ADD 1 TO ev_comment_lines.
      CONTINUE.
    ENDIF.
    SPLIT <ls_source> AT '"' INTO lv_substr1 lv_substr2 IN CHARACTER MODE.
    CALL METHOD cl_abap_string_utilities=>del_trailing_blanks
      CHANGING
        str = lv_substr1.
    IF lv_substr1 IS INITIAL.
      ADD 1 TO ev_comment_lines.
      CONTINUE.
    ENDIF.
*   Everything else is coding
    ADD 1 TO ev_code_lines.
  ENDLOOP.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_TOOL_SRC_CODE_LOCATION->ME_ANALYZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_OBJKEY                      TYPE        ZCL_TOOL_SRC_CODE_ANALYZE=>TY_MS_OBJKEY
* | [--->] IV_SUBOBJ                      TYPE        CSEQUENCE
* | [--->] IT_SOURCE                      TYPE        RSWSOURCET
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD ME_ANALYZE.

  DATA:
    ls_list TYPE ty_ms_result.

  FIELD-SYMBOLS:
    <lt_list> TYPE ty_mt_result.

* INIT RESULTS
  ls_list-num_lines_total   = 0.
  ls_list-num_lines_comment = 0.
  ls_list-num_lines_code    = 0.
  ls_list-num_lines_blank   = 0.

* BODY
  ASSIGN mr_list->* TO <lt_list>.
  CHECK sy-subrc = 0.

  CALL METHOD fill_object_info
    EXPORTING
      is_objkey        = is_objkey
      iv_subobj        = iv_subobj
    CHANGING
      cs_result_common = ls_list-common.

  CALL METHOD count_lines
    EXPORTING
      it_source        = it_source
    IMPORTING
      ev_code_lines    = ls_list-num_lines_code
      ev_tot_lines     = ls_list-num_lines_total
      ev_comment_lines = ls_list-num_lines_comment
      ev_blank_lines   = ls_list-num_lines_blank.

  APPEND ls_list TO <lt_list>.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TOOL_SRC_CODE_LOCATION->ZIF_TOOL_SRC_CODE__ANALYSIS~INIT
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_TOOL_SRC_CODE__ANALYSIS~INIT.
DATA:
    lo_structdescr TYPE REF TO cl_abap_structdescr,
    lo_tabledescr  TYPE REF TO cl_abap_tabledescr,
    lt_comp        TYPE cl_abap_structdescr=>component_table.

* BODY
  CALL METHOD super->ZIF_TOOL_SRC_CODE__ANALYSIS~init
    EXCEPTIONS
      error  = 1
      OTHERS = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
               RAISING error.
  ENDIF.

  lo_structdescr ?= cl_abap_typedescr=>describe_by_name( 'TY_MS_RESULT' ).
  CALL METHOD cl_abap_tabledescr=>create
    EXPORTING
      p_line_type = lo_structdescr
    RECEIVING
      p_result    = lo_tabledescr.

  CREATE DATA mr_list TYPE HANDLE lo_tabledescr.
  endmethod.
ENDCLASS.