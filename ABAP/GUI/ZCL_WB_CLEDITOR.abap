class ZCL_WB_CLEDITOR definition
  public
  inheriting from CL_WB_CLEDITOR
  final
  create public .

public section.

  methods CHECK_METHOD_SOURCE
    redefinition .
protected section.
private section.

  methods CUSTOM_SYNTAX_CHECK
    changing
      !CHECK_LIST_OBJECT type ref to CL_WB_CHECKLIST .
ENDCLASS.



CLASS ZCL_WB_CLEDITOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_WB_CLEDITOR->CHECK_METHOD_SOURCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] ALL_ERRORS                     TYPE        CHAR1 (default =SPACE)
* | [<---] CHECK_LIST_OBJECT              TYPE REF TO CL_WB_CHECKLIST
* | [<---] NAVIGATION_REQUEST             TYPE REF TO CL_WB_REQUEST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_method_source.

    CALL METHOD super->check_method_source
      EXPORTING
        all_errors         = all_errors
      IMPORTING
        check_list_object  = check_list_object
        navigation_request = navigation_request.

    CHECK sy-uname = 'WANGJER'.

    CHECK check_list_object IS INITIAL.

    custom_syntax_check( CHANGING check_list_object = check_list_object ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_WB_CLEDITOR->CUSTOM_SYNTAX_CHECK
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CHECK_LIST_OBJECT              TYPE REF TO CL_WB_CHECKLIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD custom_syntax_check.
    CONSTANTS: cv_threshold TYPE int4 VALUE 100.
    DATA: lt_source TYPE seop_source,
          lt_text   TYPE rsfb_source.

    CALL FUNCTION 'SEO_METHOD_GET_SOURCE'
      EXPORTING
        mtdkey = mtdkey
        state  = 'A'
      IMPORTING
        source = lt_source.

    IF lines( lt_source ) > cv_threshold.
      check_list_object = NEW #( ).
      APPEND `This warning message is raised by Jerry's custom syntax check`  TO lt_text.
      APPEND | method: { mtdkey-cpdname } has totally { lines( lt_source ) } lines of source code, please refact it to ensure   | TO lt_text.
      APPEND | no more than { cv_threshold } lines in a single method. | TO lt_text.

      check_list_object->add_error_message( p_message_text = lt_text p_message_type = 'W' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.