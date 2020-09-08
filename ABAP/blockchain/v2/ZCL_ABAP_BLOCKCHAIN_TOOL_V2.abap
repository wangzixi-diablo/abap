class ZCL_ABAP_BLOCKCHAIN_TOOL_V2 definition
  public
  final
  create public .

public section.

  types: "Jerry: define columns to be displayed
    BEGIN OF ty_block,
        index     TYPE string,
        data      TYPE string,
        phash     TYPE string,
        timestamp TYPE string,
        chash     TYPE string,
        nonce     TYPE string,
      END OF ty_block .
  types:
    tt_block TYPE STANDARD TABLE OF ty_block WITH KEY index .
  types: "Jerry: define columns to be displayed
    BEGIN OF ty_displayed_node,
        name TYPE char20,
        hash TYPE char40,
        phash TYPE char40,
        nonce Type int4,
        "timestamp type string,
      END OF ty_displayed_node .

  methods CONSTRUCTOR
    importing
      !IO_CHAIN type ref to ZCL_BLOCKCHAIN_V2 .
  methods GET_FIELDCAT_BY_DATA
    importing
      !IS_DATA type ANY
    returning
      value(RT_FIELDCAT) type LVC_T_FCAT .
  methods GET_TREE
    returning
      value(RO_TREE) type ref to CL_GUI_ALV_TREE .
  methods EXPAND .
  methods DISPLAY .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_tree_key,
        node_id  TYPE char18,
        hash     TYPE string,
        tree_key TYPE lvc_nkey,
      END OF ty_tree_key .
  types:
    tt_tree_key TYPE STANDARD TABLE OF ty_tree_key WITH KEY node_id .

  data MV_ROOT_KEY type LVC_NKEY .
  data MO_TREE type ref to CL_GUI_ALV_TREE .
  data MT_HIERARCHY type TT_BLOCK .
  constants CV_ROOT_ID type STRING value 'ROOT' ##NO_TEXT.

  methods GET_CONTAINER
    importing
      !IV_CONTAINER_NAME type CHAR30
    returning
      value(RO_CONTAINER) type ref to CL_GUI_CUSTOM_CONTAINER .
ENDCLASS.



CLASS ZCL_ABAP_BLOCKCHAIN_TOOL_V2 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_BLOCKCHAIN_TOOL_V2->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CHAIN                       TYPE REF TO ZCL_BLOCKCHAIN_V2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONSTRUCTOR.
    CHECK io_chain->is_valid( ) = abap_true.

    FIELD-SYMBOLS: <field> TYPE LINE OF lvc_t_fcat.

    DATA(lv_size) = io_chain->get_size( ).

    DO lv_size TIMES.
      DATA(lo_block) = io_chain->get_block_by_index( sy-index ).
      APPEND INITIAL LINE TO mt_hierarchy ASSIGNING FIELD-SYMBOL(<displayed_block>).
      <displayed_block>-index = lo_block->mv_index.
      <displayed_block>-data = lo_block->get_data( ).
      <displayed_block>-chash = lo_block->mv_hash.
      <displayed_block>-phash = lo_block->mv_phash.
      <displayed_block>-nonce = lo_block->mv_nonce.
    ENDDO.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_BLOCKCHAIN_TOOL_V2->DISPLAY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD DISPLAY.
    DATA: p_relat_key  TYPE lvc_nkey,
          lt_tree_key  TYPE tt_tree_key,
          ls_displayed TYPE ty_displayed_node.
    CALL METHOD mo_tree->add_node
      EXPORTING
        i_relat_node_key = p_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = 'Block Chain'
      IMPORTING
        e_new_node_key   = mv_root_key.

    DATA(ls_tree_key) = VALUE ty_tree_key( node_id = 'ROOT' hash = mt_hierarchy[ 1 ]-chash tree_key = mv_root_key ).
    APPEND ls_tree_key TO lt_tree_key.

    LOOP AT mt_hierarchy ASSIGNING FIELD-SYMBOL(<node>).
      CHECK <node>-index > 0.
      ls_displayed-name = <node>-data.
      ls_displayed-hash = <node>-chash.
      ls_displayed-phash = <node>-phash.
      ls_displayed-nonce = <node>-nonce.
      "ls_displayed-timestamp = <node>-timestamp.
      READ TABLE lt_tree_key ASSIGNING FIELD-SYMBOL(<parent>) WITH KEY hash = <node>-phash.
      CALL METHOD mo_tree->add_node
        EXPORTING
          i_relat_node_key = <parent>-tree_key
          i_relationship   = cl_gui_column_tree=>relat_last_child
          i_node_text      = CONV #( <node>-index )
          is_outtab_line   = ls_displayed
        IMPORTING
          e_new_node_key   = p_relat_key.

      ls_tree_key = VALUE #( node_id = <node>-index hash = <node>-chash tree_key = p_relat_key ).
      APPEND ls_tree_key TO lt_tree_key.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_BLOCKCHAIN_TOOL_V2->EXPAND
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD EXPAND.
    mo_tree->expand_node( i_node_key = mv_root_key i_expand_subtree = abap_true ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAP_BLOCKCHAIN_TOOL_V2->GET_CONTAINER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CONTAINER_NAME              TYPE        CHAR30
* | [<-()] RO_CONTAINER                   TYPE REF TO CL_GUI_CUSTOM_CONTAINER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_CONTAINER.
    CREATE OBJECT ro_container
      EXPORTING
        container_name              = iv_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'(100).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_BLOCKCHAIN_TOOL_V2->GET_FIELDCAT_BY_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        ANY
* | [<-()] RT_FIELDCAT                    TYPE        LVC_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_FIELDCAT_BY_DATA.
    DATA: lobj_stdesc TYPE REF TO cl_abap_structdescr,
          lv_stname   TYPE dd02l-tabname,
          lw_fields   TYPE LINE OF cl_abap_structdescr=>included_view,
          lw_fldcat   TYPE LINE OF lvc_t_fcat,
          lw_desc     TYPE x030l,
          lt_fields   TYPE cl_abap_structdescr=>included_view.
    lobj_stdesc ?= cl_abap_structdescr=>describe_by_data( is_data ).

    IF lobj_stdesc->is_ddic_type( ) IS NOT INITIAL.
      lv_stname = lobj_stdesc->get_relative_name( ).
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_buffer_active        = space
          i_structure_name       = lv_stname
          i_bypassing_buffer     = 'X'
        CHANGING
          ct_fieldcat            = rt_fieldcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      RETURN.
    ENDIF.

    lt_fields = lobj_stdesc->get_included_view( ).

    LOOP AT lt_fields INTO lw_fields.
      CLEAR: lw_fldcat,
             lw_desc.
      lw_fldcat-col_pos   = sy-tabix.
      lw_fldcat-fieldname = lw_fields-name.
      IF lw_fields-type->is_ddic_type( ) IS NOT INITIAL.
        lw_desc            = lw_fields-type->get_ddic_header( ).
        lw_fldcat-rollname = lw_desc-tabname.
      ELSE.
        lw_fldcat-inttype  = lw_fields-type->type_kind.
        lw_fldcat-intlen   = lw_fields-type->length.
        lw_fldcat-decimals = lw_fields-type->decimals.
      ENDIF.
      APPEND lw_fldcat TO rt_fieldcat.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAP_BLOCKCHAIN_TOOL_V2->GET_TREE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_TREE                        TYPE REF TO CL_GUI_ALV_TREE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_TREE.
    CREATE OBJECT ro_tree
      EXPORTING
        parent                      = get_container( 'CCONTAINER1' )
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = 'X'
        no_html_header              = 'X'
        no_toolbar                  = ''
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.
    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

    mo_tree = ro_tree.
  ENDMETHOD.
ENDCLASS.