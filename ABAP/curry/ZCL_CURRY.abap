CLASS zcl_curry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    CLASS-METHODS curry
      IMPORTING
        !iv_func               TYPE rs38l_fnam
        !it_argument           TYPE string_table
      RETURNING
        VALUE(rv_curried_func) TYPE rs38l_fnam .
    CLASS-METHODS cleanup .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_curried_argument,
        arg_name  TYPE string,
        arg_type  TYPE rs38l_typ,
        arg_value TYPE string,
      END OF ty_curried_argument .
    TYPES:
      tt_curried_argument TYPE TABLE OF ty_curried_argument WITH KEY arg_name .
    TYPES:
      BEGIN OF ty_curried_func,
        func_name      TYPE rs38l_fnam,
        curried_func   TYPE rs38l_fnam,
        function_group TYPE rs38l-area,
        curried_arg    TYPE tt_curried_argument,
      END OF ty_curried_func .
    TYPES:
      tt_curried_func TYPE TABLE OF ty_curried_func WITH KEY func_name curried_func .

    DATA mt_curried_func TYPE tt_curried_func .
    DATA mv_org_func TYPE rs38l_fnam .
    CLASS-DATA so_instance TYPE REF TO zcl_curry .
    DATA mv_curried TYPE rs38l_fnam .

    METHODS adapt_source_code
      IMPORTING
        !iv_include TYPE progname .
    METHODS run
      IMPORTING
        !iv_func               TYPE rs38l_fnam
        !it_argument           TYPE string_table
      RETURNING
        VALUE(rv_curried_func) TYPE rs38l_fnam .
    METHODS parse_argument
      IMPORTING
        !it_argument        TYPE string_table
      EXPORTING
        !et_parsed_argument TYPE tt_curried_argument .
    METHODS generate_curried_fm
      IMPORTING
        !it_parsed_argument         TYPE tt_curried_argument
      RETURNING
        VALUE(rv_generated_include) TYPE progname .
    METHODS _cleanup .
ENDCLASS.



CLASS ZCL_CURRY IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CURRY->ADAPT_SOURCE_CODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INCLUDE                     TYPE        PROGNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD adapt_source_code.
    DATA:lt_codeline    TYPE STANDARD TABLE OF char255,
         lv_argu        TYPE string,
         lt_parsed_argu TYPE tt_curried_argument.
    READ REPORT iv_include INTO lt_codeline.

    DELETE lt_codeline INDEX lines( lt_codeline ).
    DELETE lt_codeline WHERE table_line IS INITIAL.

    APPEND |DATA: lt_ptab TYPE abap_func_parmbind_tab.| TO lt_codeline.
    APPEND |DATA: ls_para LIKE LINE OF lt_ptab.| TO lt_codeline.
    lt_parsed_argu = mt_curried_func[ func_name = mv_org_func curried_func = mv_curried ]-curried_arg.
    LOOP AT lt_parsed_argu ASSIGNING FIELD-SYMBOL(<argu>).
      APPEND | DATA:  _{ <argu>-arg_name } LIKE { <argu>-arg_name }.| TO lt_codeline.

      IF <argu>-arg_value IS NOT INITIAL.
        APPEND | _{ <argu>-arg_name } = '{ <argu>-arg_value }'. | TO lt_codeline.
      ELSE.
        APPEND | _{ <argu>-arg_name } = { <argu>-arg_name }. | TO lt_codeline.
      ENDIF.

      lv_argu = | ls_para = value #( name = '{ <argu>-arg_name }' | &&
         | kind  = abap_func_exporting value = REF #( _{ <argu>-arg_name } ) ).|.
      APPEND lv_argu TO lt_codeline.
      APPEND | APPEND ls_para TO lt_ptab. | TO lt_codeline.
    ENDLOOP.

    APPEND 'TRY.' TO lt_codeline.
    APPEND |CALL FUNCTION '{ mv_org_func }' PARAMETER-TABLE lt_ptab.| TO lt_codeline.
    APPEND | CATCH cx_root INTO DATA(cx_root). | TO lt_codeline.
    APPEND |WRITE: / cx_root->get_text( ).| TO lt_codeline.
    APPEND 'ENDTRY.' TO lt_codeline.
    APPEND 'ENDFUNCTION.' TO lt_codeline.
    INSERT REPORT iv_include FROM lt_codeline.
    COMMIT WORK AND WAIT.
    WAIT UP TO 1 SECONDS.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CURRY=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
    CREATE OBJECT so_instance.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CURRY=>CLEANUP
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cleanup.
    so_instance->_cleanup( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CURRY=>CURRY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FUNC                        TYPE        RS38L_FNAM
* | [--->] IT_ARGUMENT                    TYPE        STRING_TABLE
* | [<-()] RV_CURRIED_FUNC                TYPE        RS38L_FNAM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD curry.
    rv_curried_func = so_instance->run( iv_func = iv_func it_argument = it_argument ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CURRY->GENERATE_CURRIED_FM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PARSED_ARGUMENT             TYPE        TT_CURRIED_ARGUMENT
* | [<-()] RV_GENERATED_INCLUDE           TYPE        PROGNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_curried_fm.
    DATA: lv_date               TYPE sy-datum,
          lv_time               TYPE sy-uzeit,
          lv_pool_name          TYPE rs38l-area,
          lv_func_name          TYPE rs38l-name,
          l_function_include    TYPE progname,
          lt_exception_list     TYPE TABLE OF rsexc,
          lt_export_parameter   TYPE TABLE OF rsexp,
          lt_import_parameter   TYPE TABLE OF rsimp,
          wa_rsimp              TYPE rsimp,
          lt_tables_parameter   TYPE TABLE OF rstbl,
          lt_changing_parameter TYPE TABLE OF rscha,
          lt_parameter_docu     TYPE TABLE OF rsfdo.

    lv_date = sy-datum.
    lv_time = sy-uzeit.

    CONCATENATE 'CURRY' lv_date lv_time INTO lv_pool_name.
    CONCATENATE 'Z' lv_pool_name INTO lv_func_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = lv_pool_name
        short_text              = 'Curry test by Jerry'       "#EC NOTEXT
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

    LOOP AT it_parsed_argument ASSIGNING FIELD-SYMBOL(<argu>).
      wa_rsimp-parameter = <argu>-arg_name.                 "#EC NOTEXT
      wa_rsimp-reference = 'X'.
      wa_rsimp-optional = 'X'.                              "#EC NOTEXT
      wa_rsimp-typ       = 'STRING'.                        "#EC NOTEXT
      APPEND wa_rsimp TO lt_import_parameter.
    ENDLOOP.
    CALL FUNCTION 'FUNCTION_CREATE'
      EXPORTING
        funcname                = lv_func_name
        function_pool           = lv_pool_name
        short_text              = 'Curry test by Jerry'                 "#EC NOTEXT
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
    <curried_fm>-curried_arg = it_parsed_argument.
    <curried_fm>-function_group = lv_pool_name.

    rv_generated_include = l_function_include.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CURRY->PARSE_ARGUMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ARGUMENT                    TYPE        STRING_TABLE
* | [<---] ET_PARSED_ARGUMENT             TYPE        TT_CURRIED_ARGUMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_argument.
    DATA: lt_argu   TYPE TABLE OF fupararef,
          lt_parsed TYPE tt_curried_argument.

    SELECT * INTO TABLE lt_argu FROM fupararef WHERE funcname = mv_org_func AND paramtype = 'I'.
    CHECK sy-subrc = 0.

    LOOP AT lt_argu ASSIGNING FIELD-SYMBOL(<form_argu>).
      APPEND INITIAL LINE TO lt_parsed ASSIGNING FIELD-SYMBOL(<parsed_argu>).
      CLEAR: <parsed_argu>.
      <parsed_argu>-arg_name = <form_argu>-parameter.
      <parsed_argu>-arg_type = <form_argu>-structure.
      READ TABLE it_argument ASSIGNING FIELD-SYMBOL(<curried>) INDEX sy-tabix.
      IF sy-subrc = 0.
        <parsed_argu>-arg_value = <curried>.
      ENDIF.
    ENDLOOP.

    et_parsed_argument = lt_parsed.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CURRY->RUN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FUNC                        TYPE        RS38L_FNAM
* | [--->] IT_ARGUMENT                    TYPE        STRING_TABLE
* | [<-()] RV_CURRIED_FUNC                TYPE        RS38L_FNAM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run.
    mv_org_func = iv_func.
    parse_argument( EXPORTING it_argument = it_argument
                    IMPORTING et_parsed_argument = DATA(lt_parsed) ).
    DATA(lv_include) = generate_curried_fm( lt_parsed ).
    adapt_source_code( lv_include ).
    rv_curried_func = mv_curried.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CURRY->_CLEANUP
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