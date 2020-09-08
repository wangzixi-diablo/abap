CLASS zcl_summer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      tt_class_list TYPE STANDARD TABLE OF tadir-obj_name WITH KEY table_line .

    CLASS-METHODS class_constructor .
    METHODS get_bean
      IMPORTING
        !iv_bean_name  TYPE tadir-obj_name
      RETURNING
        VALUE(ro_host) TYPE REF TO object .
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_summer .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_registered_bean,
        host_name     TYPE tadir-obj_name,
        host_ref      TYPE REF TO object,
        dependent_ref TYPE REF TO object,
      END OF ty_registered_bean .
  types:
    tt_registered_bean TYPE STANDARD TABLE OF ty_registered_bean WITH KEY host_name .

  constants CV_INJECT type STRING value '@Inject' ##NO_TEXT.
  data MT_REGISTERED_BEAN type TT_REGISTERED_BEAN .
  class-data SO_INSTANCE type ref to ZCL_SUMMER .

  methods CONNECT_BEAN
    importing
      !IV_SETTER type SEOCMPNAME
      !IV_BEAN_NAME type SEOCMPNAME
      !IO_HOST type ref to OBJECT
      !IO_DEP type ref to OBJECT .
  methods GET_IMPLEMENTATION_CLS_NAME
    importing
      !IV_TYPE type RS38L_TYP
    returning
      value(RV_CLS_NAME) type TADIR-OBJ_NAME .
  methods GET_CLASS_LIST
    importing
      !IV_PACKAGE type DEVCLASS
    returning
      value(RT_CLASS_LIST) type TT_CLASS_LIST .
  methods GET_RUNNING_PACKAGE
    returning
      value(RV_PACKAGE) type DEVCLASS .
  methods SCAN_CLS_WITH_INJECT
    importing
      !IV_CLS type TADIR-OBJ_NAME
    returning
      value(RS_INJECTION) type VSEOATTRIB .
  methods GET_INITED_INSTANCE
    importing
      !IV_CLS type SEOCLSNAME
    returning
      value(RO_RESULT) type ref to OBJECT .
  methods GET_SETTER_METHOD_NAME
    importing
      !IV_ATTR_NAME type SEOCMPNAME
    returning
      value(RV_SETTER) type SEOCMPNAME .
  methods REGISTER_BEAN
    importing
      !IV_BEAN_NAME type TADIR-OBJ_NAME
      !IO_HOST type ref to OBJECT
      !IO_DEP type ref to OBJECT .
  methods INIT .
ENDCLASS.



CLASS ZCL_SUMMER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SUMMER=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
* done at 2016-10-14 12:19PM  
    so_instance = NEW zcl_summer( ).
    so_instance->init( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->CONNECT_BEAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SETTER                      TYPE        SEOCMPNAME
* | [--->] IV_BEAN_NAME                   TYPE        SEOCMPNAME
* | [--->] IO_HOST                        TYPE REF TO OBJECT
* | [--->] IO_DEP                         TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD connect_bean.
    DATA: ptab TYPE abap_parmbind_tab.

    DATA: ls_setter_info TYPE vseoparam.

    SELECT SINGLE * INTO ls_setter_info FROM vseoparam WHERE
        clsname = iv_bean_name AND cmpname = iv_setter.

    ASSERT sy-subrc = 0.

    ptab = VALUE #( ( name  = ls_setter_info-sconame
                      kind  = cl_abap_objectdescr=>exporting
                      value = REF #( io_dep ) )
                     ).
    TRY.
        CALL METHOD io_host->(iv_setter)
          PARAMETER-TABLE ptab.
      CATCH cx_root INTO DATA(exc_ref).
        WRITE: / exc_ref->get_text( ).
        "MESSAGE exc_ref->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SUMMER->GET_BEAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BEAN_NAME                   TYPE        TADIR-OBJ_NAME
* | [<-()] RO_HOST                        TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_bean.
    READ TABLE mt_registered_bean ASSIGNING FIELD-SYMBOL(<bean>) WITH KEY
         host_name = iv_bean_name.
    CHECK sy-subrc = 0.

    ro_host = <bean>-host_ref.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->GET_CLASS_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [<-()] RT_CLASS_LIST                  TYPE        TT_CLASS_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_class_list.
    SELECT obj_name INTO TABLE rt_class_list FROM tadir WHERE pgmid = 'R3TR' AND object
       = 'CLAS' AND devclass = iv_package.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->GET_IMPLEMENTATION_CLS_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TYPE                        TYPE        RS38L_TYP
* | [<-()] RV_CLS_NAME                    TYPE        TADIR-OBJ_NAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_implementation_cls_name.
* There must be multiple class implementing the given interface specified by iv_type
* for demo purpose I only use the first hit
    SELECT SINGLE clsname INTO rv_cls_name FROM vseoimplem WHERE refclsname = iv_type.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->GET_INITED_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLS                         TYPE        SEOCLSNAME
* | [<-()] RO_RESULT                      TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_inited_instance.

    CREATE OBJECT ro_result TYPE (iv_cls).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_SUMMER=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_SUMMER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.
    ro_instance = so_instance.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->GET_RUNNING_PACKAGE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_PACKAGE                     TYPE        DEVCLASS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_running_package.
    SELECT SINGLE devclass FROM tadir INTO rv_package WHERE pgmid = 'R3TR' AND object = 'PROG'
      AND obj_name = sy-cprog.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->GET_SETTER_METHOD_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ATTR_NAME                   TYPE        SEOCMPNAME
* | [<-()] RV_SETTER                      TYPE        SEOCMPNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_setter_method_name.
    SPLIT iv_attr_name AT '_' INTO TABLE DATA(lt_match).
    READ TABLE lt_match ASSIGNING FIELD-SYMBOL(<match>) INDEX 2.
    ASSERT sy-subrc = 0.
    rv_setter = 'SET_' && <match>.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->INIT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method INIT.
    DATA(lv_package) = so_instance->get_running_package( ).
    DATA(lt_cls) = so_instance->get_class_list( lv_package ).

    LOOP AT lt_cls ASSIGNING FIELD-SYMBOL(<cls>).
      DATA(ls_injection) = so_instance->scan_cls_with_inject( <cls> ).
      CHECK ls_injection IS NOT INITIAL.
      DATA(lv_cls) = so_instance->get_implementation_cls_name( ls_injection-type ).
      DATA(lo_dep) = so_instance->get_inited_instance( CONV #( lv_cls ) ).
      DATA(lo_host) = so_instance->get_inited_instance( ls_injection-clsname ).
      DATA(lv_setter) = so_instance->get_setter_method_name( ls_injection-cmpname ).

      so_instance->register_bean( iv_bean_name = CONV #( ls_injection-clsname ) io_host = lo_host io_dep = lo_dep ).
      so_instance->connect_bean( iv_setter = lv_setter iv_bean_name = ls_injection-clsname
                                 io_host = lo_host io_dep = lo_dep ).
    ENDLOOP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->REGISTER_BEAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BEAN_NAME                   TYPE        TADIR-OBJ_NAME
* | [--->] IO_HOST                        TYPE REF TO OBJECT
* | [--->] IO_DEP                         TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD register_bean.
    DATA(entry) = VALUE ty_registered_bean( host_name = iv_bean_name
               host_ref = io_host dependent_ref = io_dep ).
    APPEND entry TO mt_registered_bean.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SUMMER->SCAN_CLS_WITH_INJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLS                         TYPE        TADIR-OBJ_NAME
* | [<-()] RS_INJECTION                   TYPE        VSEOATTRIB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD scan_cls_with_inject.
    DATA: lt_vseoattrib TYPE STANDARD TABLE OF vseoattrib.

    SELECT * INTO TABLE lt_vseoattrib FROM vseoattrib WHERE clsname = iv_cls AND descript
       = cv_inject.
* For demo purpose only handle with first attribute which is annotated with @Inject
    READ TABLE lt_vseoattrib INTO rs_injection INDEX 1.

  ENDMETHOD.
ENDCLASS.