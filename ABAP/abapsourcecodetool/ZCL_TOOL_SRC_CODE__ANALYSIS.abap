class ZCL_TOOL_SRC_CODE__ANALYSIS definition
  public
  abstract
  create public .

public section.
*"* public components of class CL_TOOL_SRC_CODE_ANALYSIS
*"* do not include other source files here!!!
  type-pools SEOR .

  interfaces ZIF_TOOL_SRC_CODE__ANALYSIS .
PROTECTED SECTION.
*"* protected components of class CL_TOOL_SRC_CODE_ANALYSIS
*"* do not include other source files here!!!

  TYPES:
    BEGIN OF ty_ms_result_common,
    devlayer          type devlayer,
    obj_gentype       TYPE char4,
    devclass          TYPE devclass,
    object            TYPE trobjtype,
    objname           TYPE sobj_name,
    subobj            TYPE string,
    author            TYPE responsibl,
    srcsystem         TYPE srcsystem,
  END OF ty_ms_result_common .

  TYPES:
    BEGIN OF ty_ms_result_list,
      counter           TYPE i,
      t_color           TYPE lvc_t_scol,
    END OF ty_ms_result_list.

  DATA mr_list TYPE REF TO data .

  CLASS zcl_tool_src_code_analyze DEFINITION LOAD .
  METHODS me_analyze
  ABSTRACT
    IMPORTING
      !is_objkey TYPE zcl_tool_src_code_analyze=>ty_ms_objkey
      !iv_subobj TYPE csequence
      !it_source TYPE rswsourcet .
  METHODS fill_object_info
    IMPORTING
      !is_objkey TYPE zcl_tool_src_code_analyze=>ty_ms_objkey
      !iv_subobj TYPE csequence
    CHANGING
      !cs_result_common TYPE ty_ms_result_common .
  METHODS determine_gentype
    IMPORTING
      !is_objkey TYPE zcl_tool_src_code_analyze=>ty_ms_objkey
    RETURNING
      value(rv_obj_gentype) TYPE char4 .
private section.
*"* private components of class CL_TOOL_SRC_CODE_ANALYSIS
*"* do not include other source files here!!!

  types:
    TY_MT_SEOCLNAME type standard table of SEOCLNAME .

  type-pools ABAP .
  data MV_BOPF_BUFFERED type ABAP_BOOL .
  data MT_BOPF_BUFFER type TY_MT_SEOCLNAME .
  data MV_CFG_BUFFERED type ABAP_BOOL .
  data MT_CFG_BUFFER type TY_MT_SEOCLNAME .
  data MV_LCPI_BUFFERED type ABAP_BOOL .
  data MT_LCPI_BUFFER type TY_MT_SEOCLNAME .
ENDCLASS.



CLASS ZCL_TOOL_SRC_CODE__ANALYSIS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_TOOL_SRC_CODE__ANALYSIS->DETERMINE_GENTYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_OBJKEY                      TYPE        ZCL_TOOL_SRC_CODE_ANALYZE=>TY_MS_OBJKEY
* | [<-()] RV_OBJ_GENTYPE                 TYPE        CHAR4
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD DETERMINE_GENTYPE.

  DATA:
    lv_genflag TYPE tadir-genflag.

* INIT RESULTS
  CLEAR rv_obj_gentype.

* BODY

* GenType FLAG: GENFLAG set in TADIR
  CLEAR lv_genflag.
  SELECT SINGLE genflag
    INTO lv_genflag
    FROM tadir
    WHERE ( pgmid    = 'R3TR'          )
      AND ( object   = is_objkey-object   )
      AND ( obj_name = is_objkey-obj_name ).
  IF ( sy-subrc = 0              ) AND
     ( lv_genflag IS NOT INITIAL ).
    rv_obj_gentype = 'FLAG'.
    RETURN.
  ENDIF.

* GenType BOPF: Accessor class
  IF is_objkey-object = 'CLAS'.

    IF mv_bopf_buffered <> abap_true.


      mv_bopf_buffered = abap_true.
    ENDIF.

    READ TABLE mt_bopf_buffer TRANSPORTING NO FIELDS
               WITH KEY table_line = is_objkey-obj_name
               BINARY SEARCH.
    IF sy-subrc = 0.
      rv_obj_gentype = 'BOPF'.
      RETURN.
    ENDIF.

  ENDIF.

* GenType CFG: Access class for configuration data
  IF is_objkey-object = 'CLAS'.

    IF mv_cfg_buffered <> abap_true.
*     Table CRGENC_REG belongs to a HOME component and might not be
*     available in some systems. In order to avoid syntax errors
*     a dynamic specification of table name is used.
      TRY.
          SELECT genclsname
            INTO TABLE mt_cfg_buffer
            FROM ('CRGENC_REG').
        CATCH cx_sy_dynamic_osql_semantics.              "#EC NO_HANDLER
*         Table missing => ignore
      ENDTRY.

      SORT mt_cfg_buffer.

      mv_cfg_buffered = abap_true.
    ENDIF.

    READ TABLE mt_cfg_buffer TRANSPORTING NO FIELDS
               WITH KEY table_line = is_objkey-obj_name
               BINARY SEARCH.
    IF sy-subrc = 0.
      rv_obj_gentype = 'CFG'.
      RETURN.
    ENDIF.

  ENDIF.

* GenType LCP: Static-typed LCP Wrapper classes CL_*_LCP (Proxy)
  IF is_objkey-object = 'CLAS'.

    IF is_objkey-obj_name CP '*_LCP'.
      SELECT     COUNT( * )
        FROM     seocompo UP TO 1 ROWS
        WHERE    clsname = is_objkey-obj_name
          AND    cmpname = 'CREATE'
          AND    cmptype = '1'
          AND    mtdtype = '0'.
      IF sy-subrc = 0.
        rv_obj_gentype = 'LCP'.
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.

* GenType LCPI: Static-typed LCP Wrapper classes CL_*_INT (BOPF)
  IF is_objkey-object = 'CLAS'.

    IF mv_lcpi_buffered <> abap_true.

      DATA:
        ls_seoclskey TYPE seoclskey,
        lt_impkeys   TYPE seor_implementing_keys,
        ls_impkeys   TYPE seor_implementing_key.

      ls_seoclskey-clsname = '/BOPF/IF_LIB_LCP_WRAPPER'.

      CALL FUNCTION 'SEO_INTERFACE_IMPLEM_GET_ALL'
        EXPORTING
          intkey  = ls_seoclskey
        IMPORTING
          impkeys = lt_impkeys
        EXCEPTIONS
          OTHERS  = 0.

      CLEAR mt_lcpi_buffer.
      LOOP AT lt_impkeys INTO ls_impkeys.
        APPEND ls_impkeys-clsname TO mt_lcpi_buffer.
      ENDLOOP.

      SORT mt_lcpi_buffer.

      mv_lcpi_buffered = abap_true.

    ENDIF.

    READ TABLE mt_lcpi_buffer TRANSPORTING NO FIELDS
               WITH KEY table_line = is_objkey-obj_name
               BINARY SEARCH.
    IF sy-subrc = 0.
      rv_obj_gentype = 'LCPI'.
      RETURN.
    ENDIF.

  ENDIF.

* GenType VIEW: Function Group generated for Table Maintenance 0*
  DATA:
    lv_tabname TYPE vim_name.                               "#EC NEEDED

  IF is_objkey-object = 'FUGR'.
    SELECT tabname
      FROM tvdir UP TO 1 ROWS                            "#EC CI_GENBUFF
      INTO lv_tabname
      WHERE area = is_objkey-obj_name.
      EXIT.
    ENDSELECT.
    IF sy-subrc = 0.
      rv_obj_gentype = 'VIEW'.
    ENDIF.
  ENDIF.

* GenType MAP: BOPF field mapping
  DATA: lo_class_descr     TYPE REF TO cl_abap_classdescr.
  DATA: lo_abap_type_descr TYPE REF TO cl_abap_typedescr.

  IF is_objkey-object = 'CLAS'.
    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = is_objkey-obj_name
      RECEIVING
        p_descr_ref    = lo_abap_type_descr
      EXCEPTIONS
        type_not_found = 1 ).

    CHECK sy-subrc IS INITIAL.

    lo_class_descr ?= lo_abap_type_descr.

    READ TABLE lo_class_descr->interfaces TRANSPORTING NO FIELDS
         WITH KEY name = '/BOPF/IF_LIB_PROXY_MAPPING'.
    IF sy-subrc = 0.
      rv_obj_gentype = 'MAP'.
    ENDIF.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_TOOL_SRC_CODE__ANALYSIS->FILL_OBJECT_INFO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_OBJKEY                      TYPE        ZCL_TOOL_SRC_CODE_ANALYZE=>TY_MS_OBJKEY
* | [--->] IV_SUBOBJ                      TYPE        CSEQUENCE
* | [<-->] CS_RESULT_COMMON               TYPE        TY_MS_RESULT_COMMON
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD FILL_OBJECT_INFO.

*  DATA:
*    ls_tadir TYPE tadir.

* INIT RESULTS
  CLEAR:
    cs_result_common-devclass,
    cs_result_common-author.

* BODY
  cs_result_common-object  = is_objkey-object.
  cs_result_common-objname = is_objkey-obj_name.
  cs_result_common-subobj  = iv_subobj.

  SELECT SINGLE devclass author srcsystem
    INTO (cs_result_common-devclass, cs_result_common-author, cs_result_common-srcsystem)
    FROM tadir
    WHERE ( pgmid    = 'R3TR'                   )
      AND ( object   = cs_result_common-object  )
      AND ( obj_name = cs_result_common-objname ).

* cs_result_common-obj_gentype = determine_gentype( is_objkey ).

  SELECT SINGLE pdevclass
    INTO cs_result_common-devlayer
    FROM tdevc
    WHERE ( devclass = cs_result_common-devclass ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TOOL_SRC_CODE__ANALYSIS->ZIF_TOOL_SRC_CODE__ANALYSIS~ANALYZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_OBJKEY                      TYPE        ZCL_TOOL_SRC_CODE_ANALYZE=>TY_MS_OBJKEY
* | [--->] IV_SUBOBJ                      TYPE        CSEQUENCE
* | [--->] IT_SOURCE                      TYPE        RSWSOURCET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_TOOL_SRC_CODE__ANALYSIS~ANALYZE.
   CALL METHOD me->me_analyze
    EXPORTING
      is_objkey = is_objkey
      iv_subobj = iv_subobj
      it_source = it_source.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TOOL_SRC_CODE__ANALYSIS->ZIF_TOOL_SRC_CODE__ANALYSIS~GET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_LIST                        TYPE        TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_TOOL_SRC_CODE__ANALYSIS~GET_RESULT.
  FIELD-SYMBOLS:
    <lt_list> TYPE standard table.

* BODY
  ASSIGN mr_list->* TO <lt_list>.
  CHECK sy-subrc = 0.
  et_list = <lt_list>.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TOOL_SRC_CODE__ANALYSIS->ZIF_TOOL_SRC_CODE__ANALYSIS~INIT
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_TOOL_SRC_CODE__ANALYSIS~INIT.
  endmethod.
ENDCLASS.