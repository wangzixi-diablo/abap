REPORT zcreate_so.

DATA: ls_header         TYPE bapisdhd1,
      ls_headerx        TYPE bapisdhd1x,
      lt_bapiret2       LIKE bapiret2   OCCURS 0 WITH HEADER LINE,
      po_order_number   TYPE bapivbeln-vbeln,
      lt_partners       TYPE TABLE OF bapiparnr,
      lt_order_items_in TYPE TABLE OF bapisditm,
      lt_order_item_x   TYPE TABLE OF bapisditmx,
      ls_order_item_x   LIKE LINE OF lt_order_item_x,
      ls_order_items_in LIKE LINE OF lt_order_items_in,
      lt_schdlin        TYPE TABLE OF bapischdl,
      ls_schdlin        LIKE LINE OF lt_schdlin,
      lt_schdlinx       TYPE TABLE OF bapischdlx,
      ls_schdlinx       LIKE LINE OF lt_schdlinx,
      ls_partners       LIKE LINE OF lt_partners,
      lt_order_text TYPE TABLE OF bapisdtext,
      ls_order_text  TYPE bapisdtext.


ls_header-serv_date = '20200101'.
ls_header-doc_type = 'TA'.

ls_header-purch_no_c = 'a'.
ls_headerx-purch_no_c = 'X'.

*ls_header-sales_org = '0001'.
*ls_header-distr_chan = '01'.
*ls_header-division = '01'.

ls_header-comp_cde_b = '0001'.
ls_headerx-comp_cde_b = 'X'.

ls_headerx-doc_type = 'X'.
ls_headerx-updateflag = 'I'.

ls_partners-partn_role = 'WE'. "ship to party
ls_partners-partn_numb = '0000000001'.
APPEND ls_partners TO lt_partners.

ls_partners-partn_role = 'AG'. " sold to party
ls_partners-partn_numb = '0000000001'.
APPEND ls_partners TO lt_partners.

ls_order_items_in-item_categ = 'TAN'.
ls_order_items_in-material = 'REDUCER_A'.
ls_order_items_in-target_qty = 999.
ls_order_items_in-itm_number = 10.

ls_order_item_x-item_categ = 'X'.
ls_order_item_x-material = 'X'.
ls_order_item_x-target_qty = 'X'.
ls_order_item_x-itm_number = 'X'.
ls_order_item_x-updateflag = 'I'.

APPEND ls_order_item_x TO lt_order_item_x.
APPEND ls_order_items_in TO lt_order_items_in.

ls_schdlin-itm_number = 10.
ls_schdlin-req_qty = 3.
ls_schdlinx-itm_number = 'X'.
ls_schdlinx-itm_number = 'X'.
ls_schdlinx-updateflag = 'X'.

APPEND ls_schdlin TO lt_schdlin.
APPEND ls_schdlinx TO lt_schdlinx.

"ls_order_text-doc_number = SALESDOCUMENT.
ls_order_text-itm_number = '000010'.
ls_order_text-text_id = '0006'.
"ls_order_text-langu = 'E'.
ls_order_text-langu_iso = 'ZH'.
ls_order_text-format_col = '*'.
ls_order_text-text_line = 'Test 2 DSFSDFDSFDS'.
ls_order_text-function = '006'.
APPEND ls_order_text TO lt_order_text.

CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
  EXPORTING
    sales_header_in       = ls_header
    sales_header_inx      = ls_headerx
    int_number_assignment = 'X'
  IMPORTING
    salesdocument_ex      = po_order_number
  TABLES
    return                = lt_bapiret2
    sales_items_in        = lt_order_items_in
    sales_items_inx       = lt_order_item_x
    sales_schedules_in    = lt_schdlin
    sales_schedules_inx   = lt_schdlinx
    sales_partners        = lt_partners
    sales_text            = lt_order_text.

LOOP AT lt_bapiret2 ASSIGNING FIELD-SYMBOL(<return>) WHERE type = 'E'.
  WRITE:/ 'Error:', <return>-message COLOR COL_NEGATIVE.
  RETURN.
ENDLOOP.


IF po_order_number IS INITIAL.
  WRITE:/ 'PO number initial'.
  RETURN.
ENDIF.

CLEAR: lt_bapiret2.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    wait   = 'X'
  IMPORTING
    return = lt_bapiret2.

LOOP AT lt_bapiret2 ASSIGNING <return>.
  WRITE:/ 'Message:', <return>-message COLOR COL_POSITIVE.
ENDLOOP.