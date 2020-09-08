*&---------------------------------------------------------------------*
*& Report ZCGLIB_GENERATE_PROXY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcglib_generate_proxy.

TYPE-POOLS:
  seoc,
  seop.

CLASS cl_oo_include_naming DEFINITION LOAD.

DATA:
  cifkey      TYPE seoclskey,
  clstype     TYPE seoclstype,
  source      TYPE seop_source_string,
  lt_source   LIKE source,
  pool_source TYPE seop_source_string,
  source_line TYPE LINE OF seop_source_string,
  tabix       TYPE sytabix,
  includes    TYPE seop_methods_w_include,
  include     TYPE seop_method_w_include,
  cifref      TYPE REF TO if_oo_clif_incl_naming,
  clsref      TYPE REF TO if_oo_class_incl_naming,
  intref      TYPE REF TO if_oo_interface_incl_naming.

DATA: l_string TYPE string.

START-OF-SELECTION.

  cifkey-clsname = 'ZCL_JAVA_CGLIB'.
  CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
    EXPORTING
      cifkey = cifkey
    RECEIVING
      cifref = cifref
    EXCEPTIONS
      OTHERS = 1.
  ASSERT sy-subrc = 0.

  APPEND 'program.' TO lt_source.
  CHECK cifref->clstype = seoc_clstype_class.
  clsref ?= cifref.
  READ REPORT clsref->class_pool
    INTO pool_source.
*    loop at pool_source into source_line.
*      if source_line cs 'CLASS-POOL'
*        or source_line cs 'class-pool'.
*        append source_line to lt_source..
*        tabix = sy-tabix.
*        exit.
*      endif.
*    endloop.

  READ REPORT clsref->locals_old
    INTO source.
  LOOP AT source
    INTO source_line.
    IF source_line NS '*"*'.
      APPEND source_line TO lt_source..
    ENDIF.
  ENDLOOP.

  READ REPORT clsref->locals_def
    INTO source.
  LOOP AT source
    INTO source_line.
    IF source_line NS '*"*'.
      APPEND source_line TO lt_source..
    ENDIF.
  ENDLOOP.

  READ REPORT clsref->locals_imp
    INTO source.
  LOOP AT source
    INTO source_line.
    IF source_line NS '*"*'.
      APPEND source_line TO lt_source..
    ENDIF.
  ENDLOOP.

  READ REPORT clsref->macros
    INTO source.
  LOOP AT source
    INTO source_line.
    IF source_line NS '*"*'.
      APPEND source_line TO lt_source..
    ENDIF.
  ENDLOOP.

  READ REPORT clsref->public_section
    INTO source.
  LOOP AT source ASSIGNING FIELD-SYMBOL(<source_line>).

    IF <source_line> NS '*"*'.
      FIND REGEX '.*methods.*\.' IN <source_line>
     "MATCH OFFSET moff
     MATCH LENGTH DATA(lv_len).

      IF sy-subrc = 0.

        lv_len = lv_len - 1.
        <source_line> = <source_line>+0(lv_len).

        CONCATENATE <source_line> 'redefinition' '.' INTO <source_line> SEPARATED BY space.
      ENDIF.

      APPEND <source_line> TO lt_source.
    ENDIF.

  ENDLOOP.
  APPEND 'methods SET_PREEXIT importing !IO_PREEXIT type ref to IF_PREEXIT .' TO lt_source.

  READ REPORT clsref->protected_section
    INTO source.
  LOOP AT source
    INTO source_line.
    IF source_line NS '*"*'.
      APPEND source_line TO lt_source..
    ENDIF.
  ENDLOOP.

  READ REPORT clsref->private_section
    INTO source.
  LOOP AT source
    INTO source_line.
    IF source_line NS '*"*'.
      APPEND source_line TO lt_source..
    ENDIF.
  ENDLOOP.
* insert setter

  APPEND 'data MO_PREEXIT type ref to IF_PREEXIT .' TO lt_source.

  CONCATENATE 'CLASS' cifkey 'IMPLEMENTATION' INTO l_string SEPARATED BY space.
  LOOP AT pool_source
    FROM tabix
    INTO source_line.
    IF source_line CS 'ENDCLASS'.
      APPEND source_line TO lt_source..
    ENDIF.
    IF source_line CS l_string.
      SKIP.
      APPEND source_line TO lt_source..
      tabix = sy-tabix.
      EXIT.
    ENDIF.
  ENDLOOP.
* method implementation
  includes = clsref->get_all_method_includes( ).
  LOOP AT includes
    INTO include.
    READ REPORT include-incname
      INTO source.

* injext preexit and post exit
    INSERT 'mo_preexit->execute( ).' INTO source INDEX 2.

    LOOP AT source
      INTO source_line.
      APPEND source_line TO lt_source..
    ENDLOOP.

  ENDLOOP.

* insert set_preexit

  APPEND 'method set_preexit.  mo_preexit = IO_PREEXIT. endmethod.' TO lt_source.
  LOOP AT pool_source
    FROM tabix
    INTO source_line.
    IF source_line CS 'ENDCLASS'.
      APPEND source_line TO lt_source..
    ENDIF.
  ENDLOOP.
  TRY.
      LOOP AT lt_source ASSIGNING FIELD-SYMBOL(<source1>) WHERE table_line CS 'ZCL_JAVA_CGLIB'.
        REPLACE 'ZCL_JAVA_CGLIB' IN <source1> WITH 'ZCL_JAVA_CGLIB_SUB'.
      ENDLOOP.

      LOOP AT lt_source ASSIGNING FIELD-SYMBOL(<source>) WHERE table_line CS 'ZCL_JAVA_CGLIB'.
        DELETE lt_source INDEX ( sy-tabix + 1 ).
        INSERT 'inheriting from ZCL_JAVA_CGLIB' INTO lt_source INDEX ( sy-tabix + 1 ).
        EXIT.
      ENDLOOP.

      GENERATE SUBROUTINE POOL lt_source NAME DATA(prog).
      WRITE: / sy-subrc.

      DATA(class) = `\PROGRAM=` && prog && `\CLASS=ZCL_JAVA_CGLIB_SUB`.
      DATA oref TYPE REF TO object.
      CREATE OBJECT oref TYPE (class).
      "data(lv_class_name) = 'ZCL_JAVA_CGLIB_SUB'.
      DATA(lo_class) = CAST zcl_java_cglib( oref ).

      CALL METHOD lo_class->('SET_PREEXIT')
        EXPORTING
          io_preexit = NEW zcl_jerry_preexit( ).
      lo_class->greet( ).

    CATCH cx_root INTO DATA(cx_root).
      WRITE: / cx_root->get_text( ).
  ENDTRY.