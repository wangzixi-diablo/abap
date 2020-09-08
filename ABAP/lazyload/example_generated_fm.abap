FUNCTION zlazy20170313062751.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_NODE_NAME) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(EO_NODE) TYPE REF TO ZCL_DOM_NODE
*"--------------------------------------------------------------------
  DATA: lt_ptab TYPE abap_func_parmbind_tab.
  DATA: ls_para LIKE LINE OF lt_ptab.
  TYPES: BEGIN OF ty_buffer,
           iv_node_name TYPE string,
           eo_node      TYPE REF TO zcl_dom_node,
         END OF ty_buffer.
  TYPES: tt_buffer TYPE STANDARD TABLE OF ty_buffer WITH KEY iv_node_name.
  STATICS: st_buffer TYPE tt_buffer.
  READ TABLE st_buffer ASSIGNING FIELD-SYMBOL(<buffer>) WITH KEY iv_node_name = iv_node_name.
  IF sy-subrc = 0.
    eo_node = <buffer>-eo_node.
    RETURN.
  ENDIF.
  ls_para = VALUE #( name = 'IV_NODE_NAME'
   kind  = abap_func_exporting value = REF #( iv_node_name ) ).
  APPEND ls_para TO lt_ptab.
  ls_para = VALUE #( name = 'EO_NODE'
   kind  = abap_func_importing value = REF #( eo_node ) ).
  APPEND ls_para TO lt_ptab.
  TRY.
      CALL FUNCTION 'ZCREATE_MASK' PARAMETER-TABLE lt_ptab.
    CATCH cx_root INTO DATA(cx_root).
      WRITE: / cx_root->get_text( ).
  ENDTRY.
  APPEND INITIAL LINE TO st_buffer ASSIGNING FIELD-SYMBOL(<filled_buffer>).
  <filled_buffer>-eo_node = eo_node.
  <filled_buffer>-iv_node_name = iv_node_name.
ENDFUNCTION.