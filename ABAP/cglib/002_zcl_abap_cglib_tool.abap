CLASS zcl_abap_cglib_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_proxy
      IMPORTING
        !iv_class_name  TYPE string
        !io_pre_exit    TYPE REF TO if_preexit
        !io_post_exit   TYPE REF TO if_postexit
      RETURNING
        VALUE(ro_proxy) TYPE REF TO object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mv_class_name TYPE string .
    CLASS-DATA mt_source TYPE seop_source_string .
    CLASS-DATA mo_proxy TYPE REF TO object .
    CLASS-DATA mo_preexit TYPE REF TO if_preexit .
    CLASS-DATA mo_postexit TYPE REF TO if_postexit .

    CLASS-METHODS generate_proxy .
    CLASS-METHODS get_source_code .
ENDCLASS.



CLASS ZCL_ABAP_CGLIB_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_CGLIB_TOOL=>GENERATE_PROXY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD generate_proxy.
    DATA(lv_new_cls_name) = mv_class_name && '_SUB'.

    DATA(lv_inherit) = |inheriting from { mv_class_name }|.
    TRANSLATE lv_new_cls_name TO UPPER CASE.
    TRY.
        LOOP AT mt_source ASSIGNING FIELD-SYMBOL(<source1>) WHERE table_line CS mv_class_name.
          REPLACE mv_class_name IN <source1> WITH lv_new_cls_name.
        ENDLOOP.

        LOOP AT mt_source ASSIGNING FIELD-SYMBOL(<source>) WHERE table_line CS mv_class_name.
          DELETE mt_source INDEX ( sy-tabix + 1 ).
          INSERT lv_inherit INTO mt_source INDEX ( sy-tabix + 1 ).
          EXIT.
        ENDLOOP.

        GENERATE SUBROUTINE POOL mt_source NAME DATA(prog).
        WRITE: / sy-subrc.

        DATA(class) = |\\PROGRAM={ prog }\\CLASS={ lv_new_cls_name }|.

        CREATE OBJECT mo_proxy TYPE (class).

        CALL METHOD mo_proxy->('SET_PREEXIT')
          EXPORTING
            io_preexit = mo_preexit.
        CALL METHOD mo_proxy->('SET_POSTEXIT')
          EXPORTING
            io_postexit = mo_postexit.

      CATCH cx_root INTO DATA(cx_root).
        WRITE: / cx_root->get_text( ).
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_CGLIB_TOOL=>GET_PROXY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLASS_NAME                  TYPE        STRING
* | [--->] IO_PRE_EXIT                    TYPE REF TO IF_PREEXIT
* | [--->] IO_POST_EXIT                   TYPE REF TO IF_POSTEXIT
* | [<-()] RO_PROXY                       TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_proxy.
    mv_class_name = iv_class_name.
    mo_preexit = io_pre_exit.
    mo_postexit = io_post_exit.
    CLEAR: mo_proxy.

    get_source_code( ).

    generate_proxy( ).

    ro_proxy = mo_proxy.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ABAP_CGLIB_TOOL=>GET_SOURCE_CODE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_source_code.
    DATA:
      cifkey      TYPE seoclskey,
      clstype     TYPE seoclstype,
      source      TYPE seop_source_string,
      pool_source TYPE seop_source_string,
      l_string    TYPE string,
      source_line TYPE LINE OF seop_source_string,
      tabix       TYPE sytabix,
      includes    TYPE seop_methods_w_include,
      include     TYPE seop_method_w_include,
      cifref      TYPE REF TO if_oo_clif_incl_naming,
      clsref      TYPE REF TO if_oo_class_incl_naming,
      intref      TYPE REF TO if_oo_interface_incl_naming.

    cifkey-clsname = mv_class_name.

    CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
      EXPORTING
        cifkey = cifkey
      RECEIVING
        cifref = cifref
      EXCEPTIONS
        OTHERS = 1.
    ASSERT sy-subrc = 0.

    APPEND 'program.' TO mt_source.
    CHECK cifref->clstype = seoc_clstype_class.
    clsref ?= cifref.
    READ REPORT clsref->class_pool INTO pool_source.

    READ REPORT clsref->locals_old INTO source.
    LOOP AT source INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO mt_source..
      ENDIF.
    ENDLOOP.

    READ REPORT clsref->locals_def INTO source.
    LOOP AT source INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO mt_source..
      ENDIF.
    ENDLOOP.

    READ REPORT clsref->locals_imp INTO source.
    LOOP AT source INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO mt_source..
      ENDIF.
    ENDLOOP.

    READ REPORT clsref->public_section INTO source.
    LOOP AT source ASSIGNING FIELD-SYMBOL(<source_line>).
      IF <source_line> NS '*"*'.
        FIND REGEX '.*methods.*\.' IN <source_line> MATCH LENGTH DATA(lv_len).
        IF sy-subrc = 0.
          lv_len = lv_len - 1.
          <source_line> = <source_line>+0(lv_len).
          CONCATENATE <source_line> 'redefinition' '.' INTO <source_line> SEPARATED BY space.
        ENDIF.

        APPEND <source_line> TO mt_source.
      ENDIF.
    ENDLOOP.
    APPEND 'methods SET_PREEXIT importing !IO_PREEXIT type ref to IF_PREEXIT .' TO mt_source.
    APPEND 'methods SET_POSTEXIT importing !IO_POSTEXIT type ref to IF_POSTEXIT .' TO mt_source.

    READ REPORT clsref->protected_section INTO source.
    LOOP AT source INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO mt_source.
      ENDIF.
    ENDLOOP.

    READ REPORT clsref->private_section INTO source.
    LOOP AT source INTO source_line.
      IF source_line NS '*"*'.
        APPEND source_line TO mt_source.
      ENDIF.
    ENDLOOP.

    APPEND 'data MO_PREEXIT type ref to IF_PREEXIT .' TO mt_source.
    APPEND 'data MO_POSTEXIT type ref to IF_POSTEXIT .' TO mt_source.

    CONCATENATE 'CLASS' cifkey 'IMPLEMENTATION' INTO l_string SEPARATED BY space.
    LOOP AT pool_source FROM tabix INTO source_line.
      IF source_line CS 'ENDCLASS'.
        APPEND source_line TO mt_source..
      ENDIF.
      IF source_line CS l_string.
        SKIP.
        APPEND source_line TO mt_source..
        tabix = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.

    includes = clsref->get_all_method_includes( ).
    LOOP AT includes INTO include.
      READ REPORT include-incname INTO source.
      INSERT 'mo_preexit->execute( ).' INTO source INDEX 2.
      INSERT 'mo_postexit->execute( ).' INTO source INDEX ( lines( source ) ).

      LOOP AT source INTO source_line.
        APPEND source_line TO mt_source..
      ENDLOOP.
    ENDLOOP.
    APPEND 'method set_preexit.  mo_preexit = IO_PREEXIT. endmethod.' TO mt_source.
    APPEND 'method set_postexit.  mo_postexit = IO_POSTEXIT. endmethod.' TO mt_source.
    LOOP AT pool_source FROM tabix INTO source_line.
      IF source_line CS 'ENDCLASS'.
        APPEND source_line TO mt_source..
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.