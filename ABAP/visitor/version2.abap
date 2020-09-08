
REPORT zcds_visitor2.
CLASS lcl_composite_visitor DEFINITION
    INHERITING FROM cl_ddl_parser
    CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: if_qlast_visitor.
    CONSTANTS: BEGIN OF co_parser_vers,
                 _740      TYPE i VALUE 0,
                 _750      TYPE i VALUE 1,
                 undefined TYPE i VALUE cl_abap_math=>max_int4,
               END OF co_parser_vers.

    TYPES:
      BEGIN OF ty_qlast_datasource,
        name TYPE qlast_name,
        type TYPE tabtype,
      END OF ty_qlast_datasource.
    TYPES: ty_qlast_datasources TYPE SORTED TABLE OF ty_qlast_datasource WITH UNIQUE KEY name-name.

    DATA: qlast_datasources TYPE ty_qlast_datasources READ-ONLY,
          ddl_source        TYPE ddddlsrcv READ-ONLY,
          parser_vers       TYPE i READ-ONLY.

    METHODS:
      constructor,
      set_parser_version
        IMPORTING
          i_vers TYPE i DEFAULT co_parser_vers-undefined,
      visit_table_datasource
        IMPORTING
          !object TYPE REF TO object,
      print_annotation
        IMPORTING
          !object TYPE REF TO object,
      set_ddl_source
        IMPORTING
          i_source TYPE ddddlsrcv.

ENDCLASS.

CLASS lcl_composite_visitor IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.

  METHOD set_parser_version.
    parser_vers = i_vers.
  ENDMETHOD.

  METHOD visit_table_datasource.
    DATA: table_datasource TYPE REF TO cl_qlast_table_datasource,
          qlast_datasource LIKE LINE OF qlast_datasources.
    table_datasource ?= object.
    qlast_datasource-name = table_datasource->get_name_struct( ).
    qlast_datasource-name-name = to_upper( qlast_datasource-name-name ).
    qlast_datasource-type = table_datasource->get_tabletype( ).
    READ TABLE qlast_datasources WITH KEY name-name = qlast_datasource-name-name TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      INSERT qlast_datasource INTO TABLE qlast_datasources.
    ENDIF.
  ENDMETHOD.

  METHOD print_annotation.
    DATA(lo_text_annotation) = CAST cl_qlast_text_annotation( object ).

    WRITE: / 'Text annotation:' , lo_text_annotation->get_text( ).
  ENDMETHOD.

  METHOD if_qlast_visitor~call.
    CALL METHOD me->(method) EXPORTING object = object .
  ENDMETHOD.

  METHOD if_qlast_visitor~get_mapping.
    mapping = VALUE #( ( classname = 'CL_QLAST_TABLE_DATASOURCE'   method = 'VISIT_TABLE_DATASOURCE' )
                       ( classname = 'CL_QLAST_TEXT_ANNOTATION'    method = 'PRINT_ANNOTATION' )
                     ).
  ENDMETHOD.

  METHOD if_qlast_visitor~get_descend.
    descend = if_qlast_visitor=>descend_postorder.
  ENDMETHOD.

  METHOD if_qlast_visitor~get_inheritance.
    inheritance = if_qlast_visitor=>inheritance_after_parent.
  ENDMETHOD.

  METHOD if_qlast_visitor~ignore.
    CASE parser_vers.
      WHEN co_parser_vers-_740.
        mask = if_qlast_visitor=>bitmask_ignore_none.
      WHEN OTHERS.
        mask = if_qlast_visitor=>bitmask_ignore_associations.
    ENDCASE.
  ENDMETHOD.

  METHOD set_ddl_source.
    ddl_source = i_source.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA:ddl_sources TYPE cl_ddl_parser=>ddlsources,
       ddl_source  TYPE ddddlsrcv,
       ast_tree    TYPE REF TO cl_qlast_ddlstmt,
       bitmask     TYPE i.
  DATA(ddl_parser) = NEW cl_ddl_parser( ).

  DATA(r_composite_vistor) = NEW lcl_composite_visitor( ).

  TRY.
      CALL METHOD ddl_parser->get_cds_sources
        EXPORTING
          iv_ddlname   = 'PRODUCTSHORTTEXT'
          iv_get_state = 'M'
        IMPORTING
          ddlsources   = ddl_sources.

      CALL METHOD cl_ddl_parser=>set_bitmask
        EXPORTING
          iv_semantic = abap_true
          iv_aiepp    = abap_false
          iv_extresol = abap_true
        RECEIVING
          rv_bitmask  = bitmask.

      CALL METHOD ddl_parser->parse_cds
        EXPORTING
          it_sources = ddl_sources
          iv_bitset  = bitmask
        RECEIVING
          stmt       = ast_tree.

      r_composite_vistor->set_parser_version( lcl_composite_visitor=>co_parser_vers-_750 ).

    CATCH cx_sy_dyn_call_error INTO DATA(error). "< 7.50. Old parser implementation. Extend View with associations not supported.
      WRITE: / error->get_text( ).
      RETURN.
  ENDTRY.

  ast_tree->accept( r_composite_vistor ).

  LOOP AT r_composite_vistor->qlast_datasources ASSIGNING FIELD-SYMBOL(<parsed_table>).
    WRITE: / <parsed_table>-name-name.
  ENDLOOP.