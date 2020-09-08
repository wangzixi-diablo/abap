*&---------------------------------------------------------------------*
*& Report ZBLOCKCHAIN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zblockchain.

PARAMETERS: diffle  TYPE char5 default '00000',
            noblock TYPE i DEFAULT 2.

DATA:blockdata TYPE zcl_abap_blockchain_tool=>tt_block .

DATA:blockdataline LIKE LINE OF blockdata,
     timestamp     TYPE timestampl,
     combineddata  TYPE string,
     prevblockdata LIKE LINE OF blockdata,
     nonce         TYPE i VALUE 1,
     noncestring   TYPE string,
     flag          TYPE c,
     difflength    TYPE i,
     gethash       TYPE REF TO cl_abap_message_digest.

blockdataline-index = 0.
blockdataline-data = 'Jerry first Genesis block'.
blockdataline-phash = '000000'.
GET TIME STAMP FIELD timestamp.
blockdataline-timestamp = timestamp.
blockdataline-nonce  = 0.
CONCATENATE blockdataline-index blockdataline-data blockdataline-phash blockdataline-timestamp blockdataline-nonce INTO combineddata.
CALL METHOD cl_abap_message_digest=>calculate_hash_for_char
  EXPORTING
    if_algorithm  = 'SHA1'
    if_data       = combineddata
  IMPORTING
    ef_hashstring = blockdataline-chash.
APPEND blockdataline TO blockdata.

noblock = noblock - 1.
difflength = strlen( diffle ).

DO noblock TIMES.
  blockdataline-index = sy-tabix.
  CONCATENATE `Jerry's Block ` blockdataline-index INTO blockdataline-data SEPARATED BY '-'.
  READ TABLE blockdata INTO prevblockdata INDEX blockdataline-index.
  IF sy-subrc EQ 0.
    blockdataline-phash = prevblockdata-chash.
  ENDIF.
  GET TIME STAMP FIELD timestamp.
  blockdataline-timestamp = timestamp.

  WHILE flag EQ abap_false.
    noncestring = nonce.
    CONCATENATE blockdataline-index blockdataline-data blockdataline-phash blockdataline-timestamp noncestring INTO combineddata.
    CALL METHOD cl_abap_message_digest=>calculate_hash_for_char
      EXPORTING
        if_algorithm  = 'SHA1'
        if_data       = combineddata
      IMPORTING
        ef_hashstring = blockdataline-chash.

    IF blockdataline-chash(difflength) = diffle.
      flag = 'X'.
      blockdataline-nonce  = nonce.
      APPEND blockdataline TO blockdata.
      nonce = 1.
      CLEAR:blockdataline.
    ENDIF.
    nonce = nonce + 1.
  ENDWHILE.
  CLEAR flag.
ENDDO.

DATA: g_alv_tree TYPE REF TO cl_gui_alv_tree,
      gt_data    TYPE STANDARD TABLE OF zcl_abap_blockchain_tool=>ty_displayed_node,
      ok_code    LIKE sy-ucomm,
      save_ok    LIKE sy-ucomm,
      ls_data    LIKE LINE OF gt_data.
  FIELD-SYMBOLS: <field> TYPE LINE OF LVC_T_FCAT.

  DATA(lo_tool) = NEW zcl_abap_blockchain_tool( blockdata ).
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
  define_label 3 'Nonce' 10.
  define_label 4 'Timestamp' 20.

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
  lo_tool->render_tree( ).
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