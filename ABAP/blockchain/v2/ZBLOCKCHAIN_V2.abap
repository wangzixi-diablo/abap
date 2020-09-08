*&---------------------------------------------------------------------*
*& Report ZBLOCKCHAIN_V1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zblockchain_v2.

PARAMETERS: num TYPE int4 DEFAULT 1,
            dif TYPE int4 default 1.

DATA: lv_start TYPE int4,
      lv_end TYPE int4.

GET RUN TIME FIELD lv_start.
DATA(lo_chain) = NEW zcl_blockchain_v2( dif ).

DO num TIMES.
  DATA(lv_data) = | Jerry block: { sy-index } |.
  DATA(lo_block) = NEW zcl_block2( iv_index = sy-index iv_data = lv_data ).
  lo_chain->add_block( lo_block ).
ENDDO.

GET RUN TIME FIELD lv_end.

lv_end = lv_end - lv_start.

WRITE:/ |time consumed for difficulty: { dif }, { lv_end } | COLOR COL_GROUP.
WRITE:/ 'Check valid:' , lo_chain->is_valid( ).
RETURN.

*====================================================================================

DATA: g_alv_tree TYPE REF TO cl_gui_alv_tree,
      gt_data    TYPE STANDARD TABLE OF zcl_abap_blockchain_tool_v2=>ty_displayed_node,
      ok_code    LIKE sy-ucomm,
      save_ok    LIKE sy-ucomm,
      ls_data    LIKE LINE OF gt_data.

FIELD-SYMBOLS: <field> TYPE LINE OF lvc_t_fcat.

IF lo_chain->is_valid( ) = abap_false.
  WRITE:/ 'Chain is not valid, cannot display!' COLOR COL_NEGATIVE.
  RETURN.
ENDIF.

DATA(lo_tool) = NEW zcl_abap_blockchain_tool_v2( io_chain = lo_chain ).
DATA(lt_fieldcat) = lo_tool->get_fieldcat_by_data( ls_data ).
PERFORM change_label.
CALL SCREEN 100.

DEFINE define_label.

  READ TABLE lt_fieldcat ASSIGNING <field> INDEX &1.
  <field>-seltext = <field>-reptext = <field>-scrtext_m = <field>-scrtext_s = <field>-scrtext_l = &2.
  <field>-outputlen = &3.

END-OF-DEFINITION.

FORM change_label.
  define_label 1 'Block Data' 20.
  define_label 2 'Hash' 40.
  define_label 3 'PHash' 40.
  define_label 4 'Nonce' 10.

ENDFORM.

FORM init_tree.
  g_alv_tree = lo_tool->get_tree( ).
  DATA l_hierarchy_header TYPE treev_hhdr.
  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.
  CALL METHOD g_alv_tree->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = l_hierarchy_header
    CHANGING
      it_fieldcatalog     = lt_fieldcat
      it_outtab           = gt_data.
  PERFORM create_tree.
  g_alv_tree->frontend_update( ).
  lo_tool->expand( ).
ENDFORM.

FORM create_tree.
  lo_tool->display( ).
ENDFORM.

FORM build_hierarchy_header CHANGING p_hierarchy_header TYPE treev_hhdr.
  p_hierarchy_header-heading = 'BlockChain list'.
  p_hierarchy_header-width = 30.
  p_hierarchy_header-width_pix = ' '.
ENDFORM.

FORM exit_program.
  LEAVE PROGRAM.
ENDFORM.

MODULE pbo OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'MAINTITLE'.
  IF g_alv_tree IS INITIAL.
    PERFORM init_tree.
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.
    ASSERT sy-subrc = 0.
  ENDIF.
ENDMODULE.

MODULE pai INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      PERFORM exit_program.
    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.