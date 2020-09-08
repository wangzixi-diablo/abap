class ZCL_TOOL_SRC_CODE_ANALYZE definition
  public
  create public .

public section.

  types:
*"* public components of class ZCL_TOOL_SRC_CODE_ANALYZER
*"* do not include other source files here!!!
    begin of ty_ms_objkey,
    pgmid type tadir-pgmid,
    object type tadir-object,
    obj_name type tadir-obj_name,
  end of ty_ms_objkey .
  types:
    TY_MT_OBJKEY type standard table of ty_ms_objkey .

  methods CONSTRUCTOR
    importing
      !IO_ANALYSIS type ref to ZIF_TOOL_SRC_CODE__ANALYSIS
      !IT_OBJKEY type TY_MT_OBJKEY .
  methods GET_RESULT
    exporting
      !ET_TABLE type TABLE .
protected section.
*"* protected components of class ZCL_TOOL_SRC_CODE_ANALYZER
*"* do not include other source files here!!!

  data MT_OBJKEY type TY_MT_OBJKEY .
  data MO_ANALYSIS type ref to ZIF_TOOL_SRC_CODE__ANALYSIS .
private section.
*"* private components of class ZCL_TOOL_SRC_CODE_ANALYZER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_TOOL_SRC_CODE_ANALYZE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TOOL_SRC_CODE_ANALYZE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_ANALYSIS                    TYPE REF TO ZIF_TOOL_SRC_CODE__ANALYSIS
* | [--->] IT_OBJKEY                      TYPE        TY_MT_OBJKEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONSTRUCTOR.

* PRECONDITIONS
  assert io_analysis is bound.

* BODY
  mo_analysis = io_analysis.
  mt_objkey   = it_objkey.

  mo_analysis->init( ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TOOL_SRC_CODE_ANALYZE->GET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_TABLE                       TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_RESULT.

  DATA:
    lr_result TYPE REF TO data,
    lt_source TYPE zCL_TOOL_RS_SERVICE=>ty_mtx_source.

  FIELD-SYMBOLS:
    <ls_objkey> LIKE LINE OF mt_objkey,
    <ls_source> LIKE LINE OF lt_source.

* BODY
  LOOP AT mt_objkey ASSIGNING <ls_objkey>.
    CALL METHOD zCL_TOOL_RS_SERVICE=>get_source
      EXPORTING
        iv_objtype = <ls_objkey>-object
        iv_objname = <ls_objkey>-obj_name
      IMPORTING
        et_source  = lt_source.

    LOOP AT lt_source ASSIGNING <ls_source>.
      CALL METHOD mo_analysis->analyze
        EXPORTING
          is_objkey = <ls_objkey>
          iv_subobj = <ls_source>-subobj
          it_source = <ls_source>-t_src.
    ENDLOOP.
  ENDLOOP.

  CALL METHOD mo_analysis->get_result
    IMPORTING
      et_list = et_table.

ENDMETHOD.
ENDCLASS.