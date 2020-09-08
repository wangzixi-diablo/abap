CLASS zcl_wb_cleditor DEFINITION
  PUBLIC
  INHERITING FROM cl_wb_cleditor
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS check_method_source
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS covariance_syntax_check
      CHANGING
        !check_list_object TYPE REF TO cl_wb_checklist .
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

    CHECK check_list_object IS INITIAL.

    covariance_syntax_check( CHANGING check_list_object = check_list_object ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_WB_CLEDITOR->COVARIANCE_SYNTAX_CHECK
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CHECK_LIST_OBJECT              TYPE REF TO CL_WB_CHECKLIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD covariance_syntax_check.

    DATA(lo_tool) = NEW zcl_abap_covariance_tool( ).
    check_list_object = NEW #( ).

    DATA(lt_text) = lo_tool->covariance_syntax_check( mtdkey ).

    check_list_object->add_error_message( p_message_text = lt_text p_message_type = 'E' ).

  ENDMETHOD.
ENDCLASS.