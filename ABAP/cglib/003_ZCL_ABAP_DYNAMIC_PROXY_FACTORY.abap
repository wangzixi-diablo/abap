class ZCL_ABAP_DYNAMIC_PROXY_FACTORY definition
  public
  final
  create public .

public section.

  class-methods GET_PROXY
    importing
      !IO_ORIGIN type ref to OBJECT
      !IV_NEW_CLASS_NAME type STRING
      !IV_PRE_EXIT type STRING
      !IV_POST_EXIT type STRING
    returning
      value(RO_PROXY) type ref to OBJECT .
protected section.
private section.

  class-data MS_VSEOCLASS type VSEOCLASS .
  class-data MT_ATTRIBUTE type SEOO_ATTRIBUTES_R .
  class-data MT_IMP_IF type SEOR_IMPLEMENTINGS_R .
  class-data MT_METHODS type SEOO_METHODS_R .
  class-data MT_PARAMETERS type SEOS_PARAMETERS_R .
  class-data MV_INTERFACE_NAME type STRING .
  class-data MV_METHOD_NAME type STRING .
  class-data MT_SOURCECODE type SEO_METHOD_SOURCE_TABLE .
  class-data MV_NEW_CLASS_NAME type STRING .
  class-data MO_ORIGIN type ref to OBJECT .
  class-data MV_PRE_EXIT type STRING .
  class-data MV_POST_EXIT type STRING .

  class-methods GENERATE_CLASS .
  class-methods PREPARE_ATTR_AND_SIGNATURE .
  class-methods PREPARE_SOURCE_CODE .
  class-methods EXTRACT_INTERFACE_INFO
    importing
      !IO_ORIGIN type ref to OBJECT .
  class-methods INIT
    importing
      !IV_NEW_CLASS_NAME type STRING
      !IO_ORIGIN type ref to OBJECT
      !IV_PRE_EXIT type STRING
      !IV_POST_EXIT type STRING .
  class-methods CREATE_INSTANCE
    returning
      value(RO_PROXY) type ref to OBJECT .
ENDCLASS.



CLASS ZCL_ABAP_DYNAMIC_PROXY_FACTORY IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_DYNAMIC_PROXY_FACTORY=>CREATE_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_PROXY                       TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CREATE_INSTANCE.
    TRY.

     create object ro_proxy type (mv_new_class_name)
        EXPORTING
           io_origin = mo_origin.
    CATCH cx_root INTO data(cx_root).
      WRITE:/ 'instance created failed: ', cx_root->get_text( ).
    ENDTRY.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_DYNAMIC_PROXY_FACTORY=>EXTRACT_INTERFACE_INFO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_ORIGIN                      TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method EXTRACT_INTERFACE_INFO.
     data(lo_class) = cast CL_ABAP_OBJECTDESCR( cl_abap_objectdescr=>describe_by_object_ref( io_origin ) ).

     READ TABLE lo_class->interfaces INTO mv_interface_name INDEX 1 .
     CHECK sy-subrc = 0.

     "For demo purpose, I assume only one method in one interface
     SELECT SINGLE cmpname INTO mv_method_name FROM seocompo
        WHERE clsname = mv_interface_name.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_DYNAMIC_PROXY_FACTORY=>GENERATE_CLASS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_class.
    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        devclass                   = '$TMP'
        version                    = seoc_version_active
        authority_check            = abap_true
        overwrite                  = abap_true
        suppress_method_generation = abap_false
        genflag                    = abap_false
        method_sources             = mt_sourcecode
        suppress_dialog            = abap_true
      CHANGING
        class                      = ms_vseoclass
        methods                    = mt_methods
        parameters                 = mt_parameters
        implementings              = mt_imp_if
        attributes                 = mt_attribute
      EXCEPTIONS
        existing                   = 1
        is_interface               = 2
        db_error                   = 3
        component_error            = 4
        no_access                  = 5
        other                      = 6
        OTHERS                     = 7.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_DYNAMIC_PROXY_FACTORY=>GET_PROXY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_ORIGIN                      TYPE REF TO OBJECT
* | [--->] IV_NEW_CLASS_NAME              TYPE        STRING
* | [--->] IV_PRE_EXIT                    TYPE        STRING
* | [--->] IV_POST_EXIT                   TYPE        STRING
* | [<-()] RO_PROXY                       TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_PROXY.
     init( iv_new_class_name = iv_new_class_name io_origin = io_origin
           iv_pre_exit = iv_pre_exit iv_post_exit = iv_post_exit ).
     extract_interface_info( io_origin ).
     prepare_source_code( ).
     prepare_attr_and_signature( ).
     generate_class( ).
     ro_proxy = create_instance( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_DYNAMIC_PROXY_FACTORY=>INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NEW_CLASS_NAME              TYPE        STRING
* | [--->] IO_ORIGIN                      TYPE REF TO OBJECT
* | [--->] IV_PRE_EXIT                    TYPE        STRING
* | [--->] IV_POST_EXIT                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method INIT.
    clear: mv_interface_name, mv_method_name, mt_sourcecode,mt_sourcecode,
    mt_imp_if,  ms_vseoclass, mt_attribute,mt_parameters,mt_methods.
    mv_new_class_name = iv_new_class_name.
    mo_origin = io_origin.
    mv_pre_exit = iv_pre_exit.
    mv_post_exit = iv_post_exit.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_DYNAMIC_PROXY_FACTORY=>PREPARE_ATTR_AND_SIGNATURE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD prepare_attr_and_signature.
    DATA:ls_attribute  LIKE LINE OF mt_attribute,
         ls_parameter  LIKE LINE OF mt_parameters,
         ls_method     LIKE LINE OF mt_methods.

    ls_method-clsname = mv_new_class_name.
    ls_method-cmpname = 'CONSTRUCTOR'.
    ls_method-state = 1. "implemented
    ls_method-exposure = 2. "public
    APPEND ls_method TO mt_methods.

    ls_parameter-clsname = mv_new_class_name.
    ls_parameter-cmpname = 'CONSTRUCTOR'.
    ls_parameter-version = 1.
    ls_parameter-descript = 'Constructor automatically generated by Jerry'.
    ls_parameter-type = 'OBJECT'."mv_interface_name.
    ls_parameter-sconame = 'IO_ORIGIN'.
    ls_parameter-cmptype = 1. "METHOD
    ls_parameter-mtdtype = 0. "METHOD
    ls_parameter-pardecltyp = 0. "IMPORTING
    ls_parameter-parpasstyp = 1. "pass by reference
    ls_parameter-typtype = 3. "type ref to
    APPEND ls_parameter TO mt_parameters.

    ls_attribute-clsname = mv_new_class_name.
    ls_attribute-cmpname = 'MO_ORIGIN'.
    ls_attribute-state = 1.
    ls_attribute-attdecltyp = 0.
    ls_attribute-attexpvirt = 0. "private
    ls_attribute-typtype = 3. "type ref to
    ls_attribute-type = 'OBJECT'."mv_interface_name.
    APPEND ls_attribute TO mt_attribute.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_DYNAMIC_PROXY_FACTORY=>PREPARE_SOURCE_CODE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD prepare_source_code.
    DATA: ls_method_source TYPE seo_method_source,
          ls_imp_if        TYPE seor_implementing_r,
          ls_imp_det       TYPE seoredef.

    ms_vseoclass-clsname   = mv_new_class_name.
    ms_vseoclass-state     = seoc_state_implemented.
    ms_vseoclass-exposure  = seoc_exposure_public.
    ms_vseoclass-descript  = `Dynamic proxy generated by Jerry's code`.
    ms_vseoclass-langu     = sy-langu.
    ms_vseoclass-clsccincl = abap_true.
    ms_vseoclass-unicode   = abap_true.
    ms_vseoclass-fixpt     = abap_true.
    ms_vseoclass-clsfinal  = abap_true.

    ls_imp_det = ls_imp_if-clsname       = mv_new_class_name.
    ls_imp_det = ls_imp_if-refclsname    = mv_interface_name.
    ls_imp_if-state      = seoc_state_implemented.
    APPEND ls_imp_if TO mt_imp_if.

    CLEAR: ls_method_source.
    DATA: lv_name TYPE string.
    ls_method_source-cpdname = |{ mv_interface_name }~{ mv_method_name }|.
    APPEND |{ mv_pre_exit }| TO ls_method_source-source.
    APPEND |DATA(lo) = CAST { mv_interface_name }( mo_origin ).| to ls_method_source-source.
    APPEND 'lo->print( ).'  TO ls_method_source-source.
    APPEND |{ mv_post_exit }| TO ls_method_source-source.

    APPEND ls_method_source TO mt_sourcecode.

    CLEAR: ls_method_source.
    ls_method_source-cpdname = 'CONSTRUCTOR'.
    APPEND 'mo_origin = io_origin.' TO ls_method_source-source.
    APPEND ls_method_source TO mt_sourcecode.
  ENDMETHOD.
ENDCLASS.