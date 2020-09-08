CLASS zcl_singleton_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    CLASS-METHODS get
      IMPORTING
        !iv_func               TYPE rs38l_fnam
      RETURNING
        VALUE(rv_curried_func) TYPE rs38l_fnam .
    CLASS-METHODS cleanup .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_curried_func,
        func_name      TYPE rs38l_fnam,
        curried_func   TYPE rs38l_fnam,
        function_group TYPE rs38l-area,
      END OF ty_curried_func .
    TYPES:
      tt_curried_func TYPE TABLE OF ty_curried_func WITH KEY func_name curried_func .
    TYPES:
      tt_fm_argument TYPE STANDARD TABLE OF fupararef WITH KEY funcname parameter .
    TYPES:
      BEGIN OF ty_fm_argument,
        func_name TYPE fupararef-funcname,
        func_arg  TYPE tt_fm_argument,
      END OF ty_fm_argument .
    TYPES:
      tt_fm_argument_detail TYPE STANDARD TABLE OF ty_fm_argument WITH KEY func_name .

    DATA mt_curried_func TYPE tt_curried_func .
    DATA mv_org_func TYPE rs38l_fnam .
    CLASS-DATA so_instance TYPE REF TO zcl_singleton_factory .
    DATA mv_curried TYPE rs38l_fnam .
    DATA mt_fm_argument TYPE tt_fm_argument_detail .

    METHODS generate_signature
      EXPORTING
        !et_import TYPE rsfb_imp
        !et_export TYPE rsfb_exp .
    METHODS adapt_source_code
      IMPORTING
        !iv_include TYPE progname .
    METHODS run
      IMPORTING
        !iv_func               TYPE rs38l_fnam
      RETURNING
        VALUE(rv_curried_func) TYPE rs38l_fnam .
    METHODS parse_argument .
    METHODS generate_singleton_fm
      RETURNING
        VALUE(rv_generated_include) TYPE progname .
    METHODS _cleanup .
ENDCLASS.



CLASS ZCL_SINGLETON_FACTORY IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SINGLETON_FACTORY->ADAPT_SOURCE_CODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INCLUDE                     TYPE        PROGNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD adapt_source_code.

    DATA:lt_codeline TYPE STANDARD TABLE OF char255,
         ls_export   TYPE fupararef,
         ls_import   TYPE fupararef.

    READ REPORT iv_include INTO lt_codeline.
    READ TABLE mt_fm_argument ASSIGNING FIELD-SYMBOL(<fm_argu>) WITH KEY func_name = mv_org_func.
    ASSERT sy-subrc = 0.
*   Jerry: for POC I only support 1 import and 1 export parameter. 1:N can easily be supported.
    READ TABLE <fm_argu>-func_arg INTO ls_import WITH KEY paramtype = 'I'.
    READ TABLE <fm_argu>-func_arg INTO ls_export WITH KEY paramtype = 'E'.
    TRANSLATE ls_import-structure TO LOWER CASE.
    TRANSLATE ls_import-parameter TO LOWER CASE.
    TRANSLATE ls_export-structure TO LOWER CASE.
    TRANSLATE ls_export-structure TO LOWER CASE.
    DELETE lt_codeline INDEX lines( lt_codeline ).
    DELETE lt_codeline WHERE table_line IS INITIAL.

    APPEND |DATA: lt_ptab TYPE abap_func_parmbind_tab.| TO lt_codeline.
    APPEND |DATA: ls_para LIKE LINE OF lt_ptab.| TO lt_codeline.
    APPEND |TYPES: BEGIN OF ty_buffer,| TO lt_codeline.
    IF ls_import-type = 'X'.
      APPEND |{ ls_import-parameter } TYPE { ls_import-structure },| TO lt_codeline.
    ELSE.
      APPEND |{ ls_import-parameter } TYPE REF TO { ls_import-structure },| TO lt_codeline.
    ENDIF.
    IF ls_export-type = 'X'.
      APPEND |{ ls_export-parameter } TYPE { ls_export-structure },| TO lt_codeline.
    ELSE.
      APPEND |{ ls_export-parameter } TYPE REF TO { ls_export-structure },| TO lt_codeline.
    ENDIF.
    APPEND |END OF ty_buffer.| TO lt_codeline.

    APPEND |TYPES: tt_buffer TYPE STANDARD TABLE OF ty_buffer WITH KEY { ls_import-parameter }.|
     TO lt_codeline.

    APPEND |STATICS: st_buffer TYPE tt_buffer.| TO lt_codeline.
    APPEND |READ TABLE st_buffer ASSIGNING FIELD-SYMBOL(<buffer>) WITH KEY|
     && | { ls_import-parameter } = { ls_import-parameter }.| TO lt_codeline.
    APPEND |IF sy-subrc = 0.| TO lt_codeline.
    APPEND |{ ls_export-parameter } = <buffer>-{ ls_export-parameter }. | TO lt_codeline.
    APPEND |RETURN.| TO lt_codeline.
    APPEND |ENDIF.| TO lt_codeline.

    APPEND | ls_para = value #( name = '{ ls_import-parameter CASE = UPPER }'| TO lt_codeline.
    APPEND |  kind  = abap_func_exporting value = REF #( { ls_import-parameter } ) ).| TO lt_codeline.
    APPEND |APPEND ls_para TO LT_PTAB.| TO lt_codeline.

    APPEND |ls_para = value #( name = '{ ls_export-parameter }'| TO lt_codeline.
    APPEND | kind  = abap_func_IMporting value = REF #( { ls_export-parameter } ) ). | TO lt_codeline.
    APPEND |APPEND ls_para TO LT_PTAB.| TO lt_codeline.

    APPEND 'TRY.' TO lt_codeline.
    APPEND |CALL FUNCTION '{ mv_org_func }' PARAMETER-TABLE lt_ptab.| TO lt_codeline.
    APPEND | CATCH cx_root INTO DATA(cx_root). | TO lt_codeline.
    APPEND |WRITE: / cx_root->get_text( ).| TO lt_codeline.
    APPEND 'ENDTRY.' TO lt_codeline.
    APPEND |APPEND INITIAL LINE TO st_buffer ASSIGNING FIELD-SYMBOL(<filled_buffer>).| TO lt_codeline.
    APPEND |<filled_buffer>-{ ls_export-parameter } = { ls_export-parameter }.| TO lt_codeline.
    APPEND |<filled_buffer>-{ ls_import-parameter } = { ls_import-parameter }.| TO lt_codeline.
    APPEND 'ENDFUNCTION.' TO lt_codeline.
    INSERT REPORT iv_include FROM lt_codeline.
    COMMIT WORK AND WAIT.
    WAIT UP TO 1 SECONDS.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SINGLETON_FACTORY=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
    CREATE OBJECT so_instance.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SINGLETON_FACTORY=>CLEANUP
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cleanup.
    so_instance->_cleanup( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SINGLETON_FACTORY->GENERATE_SIGNATURE
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_IMPORT                      TYPE        RSFB_IMP
* | [<---] ET_EXPORT                      TYPE        RSFB_EXP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_signature.

    DATA: wa_rsimp LIKE LINE OF et_import,
          wa_rsexp LIKE LINE OF et_export.

    READ TABLE mt_fm_argument ASSIGNING FIELD-SYMBOL(<fm_arg>) WITH KEY func_name = mv_org_func.
    CHECK sy-subrc = 0.

    LOOP AT <fm_arg>-func_arg ASSIGNING FIELD-SYMBOL(<arg_detail>).
      CASE <arg_detail>-paramtype.
        WHEN 'I'.
          wa_rsimp-parameter = <arg_detail>-parameter.      "#EC NOTEXT
          wa_rsimp-reference = <arg_detail>-reference.
          wa_rsimp-optional = <arg_detail>-optional.
          wa_rsimp-typ = <arg_detail>-structure.            "#EC NOTEXT
          APPEND wa_rsimp TO et_import.
        WHEN 'E'.
          wa_rsexp-parameter = <arg_detail>-parameter.      "#EC NOTEXT
          wa_rsexp-reference = <arg_detail>-reference.
          wa_rsexp-typ = |REF TO { <arg_detail>-structure }|.
          wa_rsexp-ref_class = <arg_detail>-ref_class.
          APPEND wa_rsexp TO et_export.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SINGLETON_FACTORY->GENERATE_SINGLETON_FM
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_GENERATED_INCLUDE           TYPE        PROGNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_singleton_fm.
    DATA: lv_date               TYPE sy-datum,
          lv_time               TYPE sy-uzeit,
          lv_pool_name          TYPE rs38l-area,
          lv_func_name          TYPE rs38l-name,
          l_function_include    TYPE progname,
          lt_exception_list     TYPE TABLE OF rsexc,
          lt_export_parameter   TYPE TABLE OF rsexp,
          lt_import_parameter   TYPE TABLE OF rsimp,
          lt_tables_parameter   TYPE TABLE OF rstbl,
          lt_changing_parameter TYPE TABLE OF rscha,
          lt_parameter_docu     TYPE TABLE OF rsfdo.

    lv_date = sy-datum.
    lv_time = sy-uzeit.

    CONCATENATE 'LAZY' lv_date lv_time INTO lv_pool_name.
    CONCATENATE 'Z' lv_pool_name INTO lv_func_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = lv_pool_name
        short_text              = 'Lazy Load Function demo by Jerry'       "#EC NOTEXT
        devclass                = '$TMP'                        "#EC NOTEXT
        responsible             = sy-uname
        suppress_corr_check     = space
      EXCEPTIONS
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        OTHERS                  = 12.
    IF sy-subrc <> 0.
      WRITE:/ 'Functio group was not created: ' , sy-subrc .
      RETURN.
    ENDIF.

    generate_signature( IMPORTING et_export = lt_export_parameter
                                  et_import = lt_import_parameter ).
    CALL FUNCTION 'FUNCTION_CREATE'
      EXPORTING
        funcname                = lv_func_name
        function_pool           = lv_pool_name
        short_text              = 'Lazy Load test by Jerry'                 "#EC NOTEXT
      IMPORTING
        function_include        = l_function_include
      TABLES
        exception_list          = lt_exception_list
        export_parameter        = lt_export_parameter
        import_parameter        = lt_import_parameter
        tables_parameter        = lt_tables_parameter
        changing_parameter      = lt_changing_parameter
        parameter_docu          = lt_parameter_docu
      EXCEPTIONS
        double_task             = 1
        error_message           = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        OTHERS                  = 7.

    IF sy-subrc <> 0.
      WRITE: / 'failed:', sy-subrc.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO mt_curried_func ASSIGNING FIELD-SYMBOL(<curried_fm>).

    <curried_fm>-func_name = mv_org_func.
    mv_curried = <curried_fm>-curried_func = lv_func_name.
*    <curried_fm>-curried_arg = it_parsed_argument.
    <curried_fm>-function_group = lv_pool_name.
*
    rv_generated_include = l_function_include.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SINGLETON_FACTORY=>GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FUNC                        TYPE        RS38L_FNAM
* | [<-()] RV_CURRIED_FUNC                TYPE        RS38L_FNAM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get.
    rv_curried_func = so_instance->run( iv_func = iv_func ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SINGLETON_FACTORY->PARSE_ARGUMENT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_argument.
    DATA: lt_argu    TYPE TABLE OF fupararef,
          ls_fm_argu TYPE fupararef.
    READ TABLE mt_fm_argument WITH KEY func_name = mv_org_func TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.

    SELECT * INTO TABLE lt_argu FROM fupararef WHERE funcname = mv_org_func
      AND r3state = 'A'.
    CHECK sy-subrc = 0.
    APPEND INITIAL LINE TO mt_fm_argument ASSIGNING FIELD-SYMBOL(<fm_argument>).
    <fm_argument>-func_name = mv_org_func.

    LOOP AT lt_argu ASSIGNING FIELD-SYMBOL(<form_argu>).
      MOVE-CORRESPONDING <form_argu> TO ls_fm_argu.
      APPEND ls_fm_argu TO <fm_argument>-func_arg.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SINGLETON_FACTORY->RUN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FUNC                        TYPE        RS38L_FNAM
* | [<-()] RV_CURRIED_FUNC                TYPE        RS38L_FNAM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run.
    mv_org_func = iv_func.
    parse_argument( ).
    DATA(lv_include) = generate_singleton_fm( ).
    adapt_source_code( lv_include ).
    rv_curried_func = mv_curried.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SINGLETON_FACTORY->_CLEANUP
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _cleanup.
    LOOP AT mt_curried_func ASSIGNING FIELD-SYMBOL(<curried>).

      CALL FUNCTION 'FUNCTION_DELETE'
        EXPORTING
          funcname = <curried>-curried_func.

      CALL FUNCTION 'FUNCTION_POOL_DELETE'
        EXPORTING
          pool = <curried>-function_group.
    ENDLOOP.

    CLEAR: mt_curried_func.
  ENDMETHOD.
ENDCLASS.