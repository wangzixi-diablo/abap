CLASS zcl_abap_mockito DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    METHODS get_mocked_data
      IMPORTING
        !io_cls          TYPE REF TO object
        !iv_method_name  TYPE string
        !iv_argument     TYPE string
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS when
      IMPORTING
        !io_dummy          TYPE any OPTIONAL
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abap_mockito .
    METHODS then_return
      IMPORTING
        !iv_return TYPE string .
    CLASS-METHODS mock
      IMPORTING
        !iv_class_name     TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO object .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_method,
        name     TYPE string,
        argument TYPE string,
        result   TYPE string,
      END OF ty_method .
  types:
    tt_method TYPE STANDARD TABLE OF ty_method WITH KEY name argument .
  types:
    begin of ty_argu_info,
              import_name TYPE string,
              return_name TYPE string,
           end of ty_argu_info .
  types:
    BEGIN OF ty_mocked_data,
        cls_instance TYPE REF TO object,
        mock_methods TYPE tt_method,
      END OF ty_mocked_data .
  types:
    tt_mocked_data TYPE STANDARD TABLE OF ty_mocked_data WITH KEY cls_instance .
  types:
    BEGIN OF ty_to_be_mocked,
        cls_instance TYPE REF TO object,
        method       TYPE string,
        argument     TYPE string,
      END OF ty_to_be_mocked .

  class-data SO_INSTANCE type ref to ZCL_ABAP_MOCKITO .
  data MT_MOCKED_DATA type TT_MOCKED_DATA .
  data MS_METHOD_TO_BE_MOCKED type TY_TO_BE_MOCKED .
  class-data MT_SOURCE type SEOP_SOURCE_STRING .

  class-methods GET_ARGU_INFO
    importing
      !IV_CLASS_NAME type STRING
      !IV_METHOD_NAME type SEOCMPNAME
    returning
      value(RS_ARGU_INFO) type TY_ARGU_INFO .
  methods LOG_STUB
    importing
      !IO_CLS type ref to OBJECT
      !IV_METHOD_NAME type STRING
      !IV_ARGUMENT type STRING .
  class-methods FILL_SOURCE_CODE
    importing
      !IV_CLASS_NAME type STRING .
  class-methods GENERATE_STUB
    importing
      !IV_CLASS_NAME type STRING
    returning
      value(RO_STUB) type ref to OBJECT .
ENDCLASS.



CLASS ZCL_ABAP_MOCKITO IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_MOCKITO=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
    so_instance = NEW zcl_abap_mockito( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_MOCKITO=>FILL_SOURCE_CODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS_NAME                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_source_code.
    DATA:
      lt_public_method TYPE TABLE OF seocmpname.

    APPEND |program.class { iv_class_name }_SUB definition inheriting from { iv_class_name } |
    & | create public. public section.| TO mt_source.

    SELECT cmpname INTO TABLE lt_public_method FROM seocompo WHERE clsname = iv_class_name
     AND cmptype = 1 AND mtdtype = 0.

    LOOP AT lt_public_method ASSIGNING FIELD-SYMBOL(<method>).
      APPEND |methods { <method> } redefinition.| TO mt_source.
    ENDLOOP.

    APPEND |protected section.private section.ENDCLASS.CLASS { iv_class_name }_SUB IMPLEMENTATION.|
     TO mt_source.

    LOOP AT lt_public_method ASSIGNING FIELD-SYMBOL(<method1>).
      APPEND |method { <method1> }.| TO mt_source.
      data(ls_argu_info) = get_argu_info( iv_class_name = iv_class_name
                                          IV_METHOD_NAME = <method1> ).

      APPEND |{ ls_argu_info-return_name } = zcl_abap_mockito=>when( )->get_mocked_data(| TO mt_source.
      APPEND 'io_cls = me' TO mt_source.
      APPEND |iv_method_name = '{ <method1> }'| TO mt_source.
      APPEND | iv_argument = conv #( { ls_argu_info-import_name } )| TO mt_source.
      APPEND ').' TO mt_source.
      APPEND 'endmethod.' TO mt_source.
    ENDLOOP.
    APPEND 'ENDCLASS.' TO mt_source.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_MOCKITO=>GENERATE_STUB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS_NAME                  TYPE        STRING
* | [<-()] RO_STUB                        TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_stub.
    DATA(lv_new_cls_name) = iv_class_name && '_SUB'.

    TRANSLATE lv_new_cls_name TO UPPER CASE.
    TRY.
        GENERATE SUBROUTINE POOL mt_source NAME DATA(prog).
        DATA(class) = |\\PROGRAM={ prog }\\CLASS={ lv_new_cls_name }|.
        CREATE OBJECT ro_stub TYPE (class).
      CATCH cx_root INTO DATA(cx_root).
        WRITE: / cx_root->get_text( ).
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_MOCKITO=>GET_ARGU_INFO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS_NAME                  TYPE        STRING
* | [--->] IV_METHOD_NAME                 TYPE        SEOCMPNAME
* | [<-()] RS_ARGU_INFO                   TYPE        TY_ARGU_INFO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_ARGU_INFO.
    DATA: lt_argu TYPE TABLE OF SEOSUBCODF.

  SELECT * INTO TABLE lt_argu FROM SEOSUBCODF WHERE clsname = iv_class_name
     and cmpname = IV_METHOD_NAME.

  READ TABLE lt_argu ASSIGNING FIELD-SYMBOL(<importing>) WITH KEY pardecltyp = 0.
  CHECK sy-subrc = 0.
  rs_argu_info-import_name = <importing>-sconame.

  READ TABLE lt_argu ASSIGNING FIELD-SYMBOL(<returning>) WITH KEY pardecltyp = 3.
  CHECK sy-subrc = 0.
  rs_argu_info-return_name = <returning>-sconame.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_MOCKITO->GET_MOCKED_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CLS                         TYPE REF TO OBJECT
* | [--->] IV_METHOD_NAME                 TYPE        STRING
* | [--->] IV_ARGUMENT                    TYPE        STRING
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mocked_data.
    READ TABLE mt_mocked_data ASSIGNING FIELD-SYMBOL(<mocked>) WITH KEY cls_instance = io_cls.
    IF sy-subrc = 0.
      READ TABLE <mocked>-mock_methods ASSIGNING FIELD-SYMBOL(<method>) WITH KEY name = iv_method_name
        argument = iv_argument.
      IF sy-subrc = 0.
        rv_result = <method>-result.
        RETURN.
      ENDIF.
    ENDIF.

    log_stub( io_cls = io_cls iv_method_name = iv_method_name iv_argument = iv_argument ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_MOCKITO->LOG_STUB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CLS                         TYPE REF TO OBJECT
* | [--->] IV_METHOD_NAME                 TYPE        STRING
* | [--->] IV_ARGUMENT                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD log_stub.

    READ TABLE mt_mocked_data ASSIGNING FIELD-SYMBOL(<mocked>) WITH KEY
     cls_instance = io_cls.
    IF sy-subrc <> 0.
      DATA(ls_entry) = VALUE ty_method( name = iv_method_name argument = iv_argument ).
      DATA: lt_method TYPE tt_method.
      APPEND ls_entry TO lt_method.

      DATA(ls_mock) = VALUE ty_mocked_data( cls_instance = io_cls mock_methods = lt_method ).
      APPEND ls_mock TO mt_mocked_data.
    ELSE.
      DATA(ls_entry2) = VALUE ty_method( name = iv_method_name argument = iv_argument ).
      APPEND ls_entry2 TO <mocked>-mock_methods.
    ENDIF.

    ms_method_to_be_mocked = VALUE #( cls_instance = io_cls method = iv_method_name argument = iv_argument ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_MOCKITO=>MOCK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS_NAME                  TYPE        STRING
* | [<-()] RO_INSTANCE                    TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD mock.
    CLEAR: mt_source.
    fill_source_code( iv_class_name ).

    ro_instance = generate_stub( iv_class_name ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_MOCKITO->THEN_RETURN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_RETURN                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD then_return.
    READ TABLE mt_mocked_data ASSIGNING FIELD-SYMBOL(<class>) WITH KEY cls_instance = ms_method_to_be_mocked-cls_instance.
    ASSERT sy-subrc = 0.

    READ TABLE <class>-mock_methods ASSIGNING FIELD-SYMBOL(<method>) WITH KEY name = ms_method_to_be_mocked-method
     argument = ms_method_to_be_mocked-argument.
    ASSERT sy-subrc = 0.

    <method>-result = iv_return.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_MOCKITO=>WHEN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_DUMMY                       TYPE        ANY(optional)
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_ABAP_MOCKITO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD when.
    ro_instance = so_instance.
  ENDMETHOD.
ENDCLASS.