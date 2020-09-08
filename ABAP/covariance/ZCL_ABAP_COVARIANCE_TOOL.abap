CLASS zcl_abap_covariance_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS covariance_syntax_check
      IMPORTING
        !is_method_def          TYPE seocpdkey
      RETURNING
        VALUE(rt_error_message) TYPE rsfb_source .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_method_detail,
        method_type          TYPE c LENGTH 1,
        caller_variable_name TYPE string,
        method_cls_name      TYPE string,
        method_name          TYPE string,
        line                 TYPE int4,
        call_parameter_name  TYPE string, "should be a string_table in productive use
* for simpliciation reason in this POC Jerry assume each constructor / instance method
* only define ONLY ONE parameter, in productive use should use TABLE OF instead to
* support multiple parameter
        method_signature     TYPE seosubcodf,
      END OF ty_method_detail .
    TYPES:
      tt_method_detail TYPE STANDARD TABLE OF ty_method_detail .
    TYPES:
      BEGIN OF ty_container_generic_type,
        container_name TYPE string,
        type           TYPE string,
      END OF ty_container_generic_type .
    TYPES:
      tt_container_generic_type TYPE TABLE OF ty_container_generic_type .

    CONSTANTS:
      BEGIN OF cs_method_type,
        constructor TYPE c LENGTH 1 VALUE 1,
        instance    TYPE c LENGTH 1 VALUE 2,
      END OF cs_method_type .
    DATA mt_result TYPE scr_refs .
    DATA mt_method_detail TYPE tt_method_detail .
    DATA mt_source_code TYPE seop_source_string .
    DATA ms_working_method TYPE seocpdkey .
    DATA mt_container_generic_type TYPE tt_container_generic_type .
    CONSTANTS cv_covariance_inf TYPE string VALUE 'ZIF_COVARIANCE' ##NO_TEXT.
    DATA mt_error_message TYPE rsfb_source .

    METHODS is_covariance_fulfilled
      IMPORTING
        !iv_generic_type    TYPE string
        !iv_concrete_type   TYPE string
      RETURNING
        VALUE(rv_fulfilled) TYPE abap_bool .
    METHODS report_error
      IMPORTING
        !iv_method_name   TYPE string
        !iv_generic_type  TYPE string
        !iv_concrete_type TYPE string
        !iv_line          TYPE int4 .
    METHODS check_ctor_covariance
      IMPORTING
        !is_method_def TYPE ty_method_detail .
    METHODS get_container_generic_type
      IMPORTING
        !iv_container_name     TYPE string
      RETURNING
        VALUE(rv_generic_type) TYPE string .
    METHODS is_covariance_check_needed
      IMPORTING
        !iv_caller_name  TYPE string
      RETURNING
        VALUE(rv_needed) TYPE abap_bool .
    METHODS fill_caller_variable_name
      IMPORTING
        !iv_current_index TYPE int4
      CHANGING
        !cs_method_detail TYPE ty_method_detail .
    METHODS get_method_type
      IMPORTING
        !iv_raw                 TYPE string
      RETURNING
        VALUE(rs_method_detail) TYPE ty_method_detail .
    METHODS fill_call_parameter
      IMPORTING
        !iv_current_index TYPE int4
      CHANGING
        !cs_method_detail TYPE ty_method_detail .
    METHODS fill_method_source .
    METHODS get_constructor_call_parameter
      IMPORTING
        !iv_code              TYPE string
        !iv_param_name        TYPE string
      RETURNING
        VALUE(rv_param_value) TYPE string .
    METHODS get_variable_type
      IMPORTING
        !iv_variable_name       TYPE string
      RETURNING
        VALUE(rv_variable_type) TYPE string .
    METHODS check_instance_covariance
      IMPORTING
        !is_method_def TYPE ty_method_detail .
    METHODS initialize .
ENDCLASS.



CLASS ZCL_ABAP_COVARIANCE_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->CHECK_CTOR_COVARIANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_METHOD_DEF                  TYPE        TY_METHOD_DETAIL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_ctor_covariance.
    DATA(lv_generic_type) = get_container_generic_type( iv_container_name = is_method_def-method_cls_name ).
    IF is_covariance_fulfilled( iv_generic_type = lv_generic_type
                                iv_concrete_type = is_method_def-call_parameter_name ) = abap_false.
      report_error( iv_method_name   = is_method_def-method_name
                    iv_generic_type  = lv_generic_type
                    iv_concrete_type = is_method_def-call_parameter_name
                    iv_line          = is_method_def-line ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->CHECK_INSTANCE_COVARIANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_METHOD_DEF                  TYPE        TY_METHOD_DETAIL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_instance_covariance.
    DATA(lv_generic_type) = get_container_generic_type( iv_container_name = is_method_def-method_cls_name ).
    DATA(lv_concrete_type) = get_variable_type( is_method_def-call_parameter_name ).
    IF is_covariance_fulfilled( iv_generic_type = lv_generic_type
                                iv_concrete_type = lv_concrete_type ) = abap_false.
      report_error( iv_method_name   = is_method_def-method_name
                    iv_generic_type  = lv_generic_type
                    iv_concrete_type = lv_concrete_type
                    iv_line          = is_method_def-line ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_COVARIANCE_TOOL->COVARIANCE_SYNTAX_CHECK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_METHOD_DEF                  TYPE        SEOCPDKEY
* | [<-()] RT_ERROR_MESSAGE               TYPE        RSFB_SOURCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD covariance_syntax_check.
    DATA: lv_include TYPE progname,
          lv_main    TYPE progname,
          lv_index   TYPE int4 VALUE 1.
    FIELD-SYMBOLS:<method> LIKE LINE OF mt_result.

    initialize( ).
    ms_working_method = is_method_def.
    fill_method_source( ).
    lv_include = cl_oo_classname_service=>get_method_include( is_method_def ).
    lv_main = cl_oo_classname_service=>get_classpool_name( is_method_def-clsname ).

    DATA(lo_compiler) = NEW cl_abap_compiler( p_name = lv_main p_include = lv_include ).

    lo_compiler->get_all( IMPORTING p_result = mt_result ).

    LOOP AT mt_result ASSIGNING <method>.
      CASE <method>-tag.
        WHEN 'ME'.
          DATA(ls_method_detail) = get_method_type( <method>-full_name ).
          fill_caller_variable_name( EXPORTING iv_current_index = lv_index
                                     CHANGING  cs_method_detail = ls_method_detail ).
          IF ls_method_detail-method_signature IS NOT INITIAL.
            fill_call_parameter( EXPORTING iv_current_index = lv_index
                                    CHANGING cs_method_detail = ls_method_detail ).
          ENDIF.
          ls_method_detail-line = <method>-line.
          APPEND ls_method_detail TO mt_method_detail.
        WHEN OTHERS.
      ENDCASE.
      ADD 1 TO lv_index.
    ENDLOOP.

    DELETE mt_method_detail WHERE caller_variable_name IS INITIAL.
    LOOP AT mt_method_detail ASSIGNING FIELD-SYMBOL(<result>).
      CHECK is_covariance_check_needed( <result>-caller_variable_name ) = abap_true.
      CASE <result>-method_type.
        WHEN cs_method_type-constructor.
          check_ctor_covariance( <result> ).
        WHEN cs_method_type-instance.
          check_instance_covariance( <result> ).
      ENDCASE.
    ENDLOOP.
    rt_error_message = mt_error_message.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->FILL_CALLER_VARIABLE_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CURRENT_INDEX               TYPE        INT4
* | [<-->] CS_METHOD_DETAIL               TYPE        TY_METHOD_DETAIL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_caller_variable_name.
    DATA: lv_index TYPE int4.

    lv_index = iv_current_index.
    WHILE lv_index > 0.
      READ TABLE mt_result ASSIGNING FIELD-SYMBOL(<line>) INDEX lv_index.
      IF <line>-tag = 'DA'.
        cs_method_detail-caller_variable_name = <line>-name.
        RETURN.
      ENDIF.

      lv_index = lv_index - 1.
    ENDWHILE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->FILL_CALL_PARAMETER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CURRENT_INDEX               TYPE        INT4
* | [<-->] CS_METHOD_DETAIL               TYPE        TY_METHOD_DETAIL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_call_parameter.
    DATA: lv_index TYPE int4,
          lv_total TYPE int4.

    FIELD-SYMBOLS: <line> LIKE LINE OF mt_result.
    lv_total = lines( mt_result ).
    lv_index = iv_current_index.

    CASE cs_method_detail-method_type.
      WHEN cs_method_type-instance.
        WHILE lv_index < lv_total.
          READ TABLE mt_result ASSIGNING <line> INDEX lv_index.
          IF <line>-tag = 'DA'.
            cs_method_detail-call_parameter_name = <line>-name.
            RETURN.
          ENDIF.
          lv_index = lv_index + 1.
        ENDWHILE.
      WHEN cs_method_type-constructor.
        WHILE lv_index < lv_total.
          READ TABLE mt_result ASSIGNING <line> INDEX lv_index.
          IF <line>-name = cs_method_detail-method_signature-sconame.
            READ TABLE mt_source_code ASSIGNING FIELD-SYMBOL(<codeline>) INDEX <line>-line.
            cs_method_detail-call_parameter_name = get_constructor_call_parameter( iv_code = <codeline>
                           iv_param_name = <line>-name ).
            RETURN.
          ENDIF.
          lv_index = lv_index + 1.
        ENDWHILE.
    ENDCASE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->FILL_METHOD_SOURCE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_method_source.

    CALL FUNCTION 'SEO_METHOD_GET_SOURCE'
      EXPORTING
        mtdkey          = ms_working_method
      IMPORTING
        source_expanded = mt_source_code.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->GET_CONSTRUCTOR_CALL_PARAMETER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CODE                        TYPE        STRING
* | [--->] IV_PARAM_NAME                  TYPE        STRING
* | [<-()] RV_PARAM_VALUE                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_constructor_call_parameter.
    DATA(lv_source_code) = | { iv_code CASE = UPPER } |.

    DATA(reg_pattern) = |^.*{ iv_param_name }.*=(.*)$|.

    DATA(lo_regex) = NEW cl_abap_regex( pattern = reg_pattern ).
    DATA(lo_matcher) = lo_regex->create_matcher( EXPORTING text = lv_source_code ).

    CHECK lo_matcher->match( ) = abap_true.

    DATA(lt_reg_match_result) = lo_matcher->find_all( ).

    READ TABLE lt_reg_match_result ASSIGNING FIELD-SYMBOL(<reg_entry>) INDEX 1.

    CHECK sy-subrc = 0.

    READ TABLE <reg_entry>-submatches ASSIGNING FIELD-SYMBOL(<match>) INDEX 1.

    rv_param_value = lv_source_code+<match>-offset(<match>-length).

    REPLACE ALL OCCURRENCES OF REGEX `[\.()']` IN rv_param_value WITH space.

    CONDENSE rv_param_value NO-GAPS.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->GET_CONTAINER_GENERIC_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CONTAINER_NAME              TYPE        STRING
* | [<-()] RV_GENERIC_TYPE                TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_container_generic_type.

    DATA: lv_type   TYPE seoredef-attvalue,
          ls_buffer LIKE LINE OF mt_container_generic_type.

    READ TABLE mt_container_generic_type ASSIGNING FIELD-SYMBOL(<type>)
     WITH KEY container_name = iv_container_name.
    IF sy-subrc = 0.
      rv_generic_type = <type>-type.
      RETURN.
    ENDIF.

    SELECT SINGLE attvalue FROM seoredef INTO rv_generic_type
       WHERE clsname = iv_container_name AND refclsname = cv_covariance_inf.
    CHECK sy-subrc = 0.

    REPLACE ALL OCCURRENCES OF '''' IN rv_generic_type WITH space.
    CONDENSE rv_generic_type NO-GAPS.

    ls_buffer = VALUE #( container_name = iv_container_name
                         type = rv_generic_type ).
    APPEND ls_buffer TO mt_container_generic_type.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->GET_METHOD_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_RAW                         TYPE        STRING
* | [<-()] RS_METHOD_DETAIL               TYPE        TY_METHOD_DETAIL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_method_type.
    CONSTANTS : cv_instance    TYPE string VALUE '^\\TY:(.*)\\ME:(.*)$',
                cv_constructor TYPE string VALUE '^\\TY:(.*)$'.

    DATA: lo_regex            TYPE REF TO cl_abap_regex,
          lo_matcher          TYPE REF TO cl_abap_matcher,
          lt_reg_match_result TYPE match_result_tab.
    FIELD-SYMBOLS: <reg_entry> LIKE LINE OF lt_reg_match_result,
                   <match1>    LIKE LINE OF <reg_entry>-submatches,
                   <match2>    LIKE <match1>.

    lo_regex = NEW #( pattern = cv_instance ).

    lo_matcher = lo_regex->create_matcher( EXPORTING text = iv_raw ).

    IF lo_matcher->match( ) = abap_false.
      lo_regex = NEW #( pattern = cv_constructor ).
      lo_matcher = lo_regex->create_matcher( EXPORTING text = iv_raw ).
      CHECK lo_matcher->match( ) = abap_true.
      rs_method_detail-method_type = cs_method_type-constructor.
    ELSE.
      rs_method_detail-method_type = cs_method_type-instance.
    ENDIF.

    lt_reg_match_result = lo_matcher->find_all( ).
    READ TABLE lt_reg_match_result ASSIGNING <reg_entry> INDEX 1.

    READ TABLE <reg_entry>-submatches ASSIGNING <match1> INDEX 1.
    rs_method_detail-method_cls_name = iv_raw+<match1>-offset(<match1>-length).
    IF lines( <reg_entry>-submatches ) = 2.

      READ TABLE <reg_entry>-submatches ASSIGNING <match2> INDEX 2.
      rs_method_detail-method_name = iv_raw+<match2>-offset(<match2>-length).
    ELSE.
      rs_method_detail-method_name = 'CONSTRUCTOR'.
    ENDIF.
    SELECT SINGLE * INTO rs_method_detail-method_signature FROM seosubcodf
        WHERE clsname = rs_method_detail-method_cls_name AND cmpname = rs_method_detail-method_name.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->GET_VARIABLE_TYPE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VARIABLE_NAME               TYPE        STRING
* | [<-()] RV_VARIABLE_TYPE               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_variable_type.
    READ TABLE mt_method_detail ASSIGNING FIELD-SYMBOL(<ctr>) WITH KEY
      method_type = cs_method_type-constructor caller_variable_name = iv_variable_name.
    CHECK sy-subrc = 0.

    rv_variable_type = <ctr>-method_cls_name.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->INITIALIZE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD initialize.
    CLEAR: mt_result,mt_method_detail,mt_source_code,ms_working_method,
mt_container_generic_type,mt_error_message.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->IS_COVARIANCE_CHECK_NEEDED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CALLER_NAME                 TYPE        STRING
* | [<-()] RV_NEEDED                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_covariance_check_needed.

    READ TABLE mt_method_detail ASSIGNING FIELD-SYMBOL(<method>) WITH KEY
    caller_variable_name = iv_caller_name method_type = cs_method_type-constructor.
    CHECK sy-subrc = 0.
    DATA(lo_descr) = CAST cl_abap_objectdescr( cl_abap_classdescr=>describe_by_name( <method>-method_cls_name ) ).

    CHECK lo_descr->interfaces IS NOT INITIAL.
    READ TABLE lo_descr->interfaces ASSIGNING FIELD-SYMBOL(<interface>) INDEX 1.

    IF <interface> = cv_covariance_inf.
      rv_needed = abap_true.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->IS_COVARIANCE_FULFILLED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_GENERIC_TYPE                TYPE        STRING
* | [--->] IV_CONCRETE_TYPE               TYPE        STRING
* | [<-()] RV_FULFILLED                   TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_covariance_fulfilled.
    DATA: lv_super TYPE string,
          lv_child TYPE string.
    rv_fulfilled = abap_false.

    IF iv_generic_type = iv_concrete_type.
      rv_fulfilled = abap_true.
      RETURN.
    ENDIF.

    lv_child = iv_concrete_type.

    DO.
      SELECT SINGLE refclsname INTO lv_super FROM seometarel
         WHERE clsname = lv_child.
      IF sy-subrc = 4.
        RETURN.
      ELSEIF lv_super = iv_generic_type.
        rv_fulfilled = abap_true.
        RETURN.
      ELSE.
        lv_child = lv_super.
      ENDIF.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_COVARIANCE_TOOL->REPORT_ERROR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_METHOD_NAME                 TYPE        STRING
* | [--->] IV_GENERIC_TYPE                TYPE        STRING
* | [--->] IV_CONCRETE_TYPE               TYPE        STRING
* | [--->] IV_LINE                        TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD report_error.
    APPEND | Covariance violation in method: { iv_method_name } !| TO mt_error_message.
    APPEND | The container has generic type: { iv_generic_type } | TO mt_error_message.
    APPEND | However the assigned concrete type: { iv_concrete_type } in line { iv_line } is not a subclass of it! | TO mt_error_message.
  ENDMETHOD.
ENDCLASS.