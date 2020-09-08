class ZCL_C4C_SYNC_TOOL definition
  public
  final
  create private .

public section.

  methods HANDLE
    importing
      !IV_NOTIFICATION_JSON type STRING .
  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_C4C_SYNC_TOOL .
  methods SEND_HTTP_REQUEST
    importing
      !IV_URL type STRING
    exporting
      !EV_RESPONSE type STRING
      !EV_ERROR_MESSAGE type STRING .
  methods TRIGGER_S4_OUT_DELI
    importing
      !IO_SERVER type ref to IF_HTTP_SERVER .
  PROTECTED SECTION.
private section.

  class-data SO_INSTANCE type ref to ZCL_C4C_SYNC_TOOL .
  data:
    mt_prod_map TYPE TABLE OF crmd_prod_map .

  methods GET_MAPPED_S4_BP_ID
    importing
      !IV_ROLE_CODE type INT4
      !IT_PARTY_TAB type CRMT_PARTY_T
    returning
      value(RV_ID) type KUNWE .
  methods GET_CREATED_SO_URL
    importing
      !IV_NOTIFICATION_JSON type STRING
    returning
      value(RV_URL) type STRING .
  methods CONSTRUCTOR .
  methods CREATE_SO
    importing
      !IS_FROM_C4C type CRMT_D_SALES_ORDER
      !IT_PRODUCT type CRMT_ITEM_PRODUCT_T
      !IV_SHIPTO type KUNWE
      !IV_TEXT type STRING .
  methods GET_CREATED_PARTY
    importing
      !IV_CREATED_SO_URL type STRING
    returning
      value(RT_CREATED_PARTY) type CRMT_PARTY_T .
  methods LOG
    importing
      !IS_FROM_C4C type CRMT_D_SALES_ORDER
      !IV_CREATED_SO type VBELN_VA .
  methods GET_CREATED_ITEM_PRODUCT
    importing
      !IV_CREATED_SO_URL type STRING
    returning
      value(RT_CREATED_ITEM) type CRMT_ITEM_PRODUCT_T .
  methods GET_CREATED_TEXT
    importing
      !IV_CREATED_SO_URL type STRING
    returning
      value(RV_TEXT) type STRING .
ENDCLASS.



CLASS ZCL_C4C_SYNC_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_C4C_SYNC_TOOL=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
    so_instance = NEW zcl_c4c_sync_tool( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C4C_SYNC_TOOL->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    SELECT * INTO TABLE mt_prod_map FROM crmd_prod_map.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C4C_SYNC_TOOL->CREATE_SO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_FROM_C4C                    TYPE        CRMT_D_SALES_ORDER
* | [--->] IT_PRODUCT                     TYPE        CRMT_ITEM_PRODUCT_T
* | [--->] IV_SHIPTO                      TYPE        KUNWE
* | [--->] IV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_so.

    DATA: ls_header         TYPE bapisdhd1,
          ls_headerx        TYPE bapisdhd1x,
          lt_bapiret2       TYPE STANDARD TABLE OF bapiret2,
          ls_commit         TYPE bapiret2,
          lv_created_so     TYPE bapivbeln-vbeln,
          lt_partners       TYPE TABLE OF bapiparnr,
          lt_order_items_in TYPE TABLE OF bapisditm,
          ls_order_items_in LIKE LINE OF lt_order_items_in,
          lt_order_item_x   TYPE TABLE OF bapisditmx,
          ls_order_item_x   LIKE LINE OF lt_order_item_x,
          lt_order_text     TYPE TABLE OF bapisdtext,
          ls_order_text     TYPE bapisdtext,
          lt_schdlin        TYPE TABLE OF bapischdl,
          ls_schdlin        LIKE LINE OF lt_schdlin,
          lt_schdlinx       TYPE TABLE OF bapischdlx,
          ls_schdlinx       LIKE LINE OF lt_schdlinx,
          ls_partners       LIKE LINE OF lt_partners.

*    ls_header-serv_date = '20200101'.
    ls_header-doc_type = 'TA'.

*ls_header-sales_org = '0001'.
*ls_header-distr_chan = '01'.
*ls_header-division = '01'.

*    ls_header-comp_cde_b = '0001'.
*    ls_headerx-comp_cde_b = 'X'.

    ls_headerx-doc_type = 'X'.
    ls_headerx-updateflag = 'I'.

    ls_header-purch_no_c = is_from_c4c-d-results-id.
    ls_headerx-purch_no_c = 'X'.

* Jerry 2018-11-08: C4C does not have sold to party, use ship to party instead.
* see https://github.wdf.sap.corp/S4-SalesService-CHN/FreeDiscussion/issues/645
    ls_partners-partn_role = 'WE'. "ship to party
    ls_partners-partn_numb = iv_shipto.
    APPEND ls_partners TO lt_partners.

    ls_partners-partn_role = 'AG'. " sold to party
    ls_partners-partn_numb = iv_shipto.
    APPEND ls_partners TO lt_partners.

    LOOP AT it_product ASSIGNING FIELD-SYMBOL(<item>).
      CLEAR: ls_order_items_in, ls_order_item_x.

      ls_order_items_in-item_categ = 'TAN'.

      READ TABLE mt_prod_map WITH KEY c4c_id = <item>-productinternalid ASSIGNING FIELD-SYMBOL(<prod>).
      ASSERT sy-subrc = 0.

      ls_order_items_in-material = <prod>-s4_id.

      ls_order_items_in-itm_number = <item>-id.

      ls_order_item_x-item_categ = 'X'.
      ls_order_item_x-material = 'X'.
      ls_order_item_x-itm_number = 'X'.
      ls_order_item_x-updateflag = 'I'.

      APPEND ls_order_item_x TO lt_order_item_x.
      APPEND ls_order_items_in TO lt_order_items_in.

      ls_schdlin-itm_number = <item>-id.
      ls_schdlin-req_qty = <item>-requestedquantity.
      ls_schdlinx-itm_number = 'X'.
      ls_schdlinx-req_qty = 'X'.
      ls_schdlinx-updateflag = 'X'.

      APPEND ls_schdlin TO lt_schdlin.
      APPEND ls_schdlinx TO lt_schdlinx.

    ENDLOOP.

* Jerry 2018-11-27 4:18PM - Echo said only 1 line item is possible

    IF iv_text IS NOT INITIAL.
      ls_order_text-itm_number = '000010'.
      ls_order_text-text_id = '0006'.
      "ls_order_text-langu = 'E'.
      ls_order_text-langu_iso = 'ZH'.
      ls_order_text-format_col = '*'.
      ls_order_text-text_line = iv_text.
      ls_order_text-function = '006'.
      APPEND ls_order_text TO lt_order_text.
    ENDIF.

    CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
      EXPORTING
        sales_header_in       = ls_header
        sales_header_inx      = ls_headerx
        int_number_assignment = 'X'
      IMPORTING
        salesdocument_ex      = lv_created_so
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
      ASSERT 1 = 0. "something bad happens :(
    ENDLOOP.

    ASSERT lv_created_so IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = ls_commit.

    CALL METHOD log
      EXPORTING
        is_from_c4c   = is_from_c4c
        iv_created_so = lv_created_so.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C4C_SYNC_TOOL->GET_CREATED_ITEM_PRODUCT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CREATED_SO_URL              TYPE        STRING
* | [<-()] RT_CREATED_ITEM                TYPE        CRMT_ITEM_PRODUCT_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_created_item_product.
    CONSTANTS: cv_item TYPE string VALUE '/CustomerQuoteItem'.

    DATA: lv_item_url          TYPE string,
          lv_created_item_json TYPE string,
          ls_created_item      TYPE crmt_d_item_product.

    lv_item_url = iv_created_so_url && cv_item.

    CALL METHOD send_http_request
      EXPORTING
        iv_url      = lv_item_url
      IMPORTING
        ev_response = lv_created_item_json.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_created_item_json
      CHANGING
        data = ls_created_item.

    rt_created_item = ls_created_item-d-results.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C4C_SYNC_TOOL->GET_CREATED_PARTY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CREATED_SO_URL              TYPE        STRING
* | [<-()] RT_CREATED_PARTY               TYPE        CRMT_PARTY_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_created_party.
    CONSTANTS: cv_party TYPE string VALUE '/CustomerQuoteParty'.

    DATA: lv_party_url          TYPE string,
          lv_created_party_json TYPE string,
          ls_created_party      TYPE crmt_d_party.

    lv_party_url = iv_created_so_url && cv_party.

    CALL METHOD send_http_request
      EXPORTING
        iv_url      = lv_party_url
      IMPORTING
        ev_response = lv_created_party_json.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_created_party_json
      CHANGING
        data = ls_created_party.

    rt_created_party = ls_created_party-d-results.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C4C_SYNC_TOOL->GET_CREATED_SO_URL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NOTIFICATION_JSON           TYPE        STRING
* | [<-()] RV_URL                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_created_so_url.
    DATA: ls_noti TYPE crmt_odata_notification.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = iv_notification_json
      CHANGING
        data = ls_noti.

    CHECK ls_noti-event = 'create'.
    rv_url = ls_noti-odataserviceendpoint.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C4C_SYNC_TOOL->GET_CREATED_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CREATED_SO_URL              TYPE        STRING
* | [<-()] RV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_CREATED_TEXT.
    CONSTANTS: cv_text TYPE string VALUE '/CustomerQuoteText'.

    DATA: lv_text_url          TYPE string,
          lv_created_text_json TYPE string,
          ls_created_text      TYPE CRMT_D_TEXT.

    lv_text_url = iv_created_so_url && cv_text.

    CALL METHOD send_http_request
      EXPORTING
        iv_url      = lv_text_url
      IMPORTING
        ev_response = lv_created_text_json.

    CALL METHOD /ui2/cl_json=>deserialize
      EXPORTING
        json = lv_created_text_json
      CHANGING
        data = ls_created_text.

    read TABLE ls_created_text-d-results ASSIGNING FIELD-SYMBOL(<result>) index 1.

    check sy-subrc = 0.

    rv_text = <result>-text.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_C4C_SYNC_TOOL=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_C4C_SYNC_TOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.
    ro_instance = so_instance.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C4C_SYNC_TOOL->GET_MAPPED_S4_BP_ID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ROLE_CODE                   TYPE        INT4
* | [--->] IT_PARTY_TAB                   TYPE        CRMT_PARTY_T
* | [<-()] RV_ID                          TYPE        KUNWE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mapped_s4_bp_id.

    DATA: lv_c4c_id   TYPE char10,
          lv_rolecode TYPE crmt_party-rolecode.

    lv_rolecode = iv_role_code.

    CONDENSE lv_rolecode NO-GAPS.

    READ TABLE it_party_tab INTO DATA(ls_party)
     WITH KEY rolecode = lv_rolecode.

    ASSERT sy-subrc = 0.

    lv_c4c_id = ls_party-partyid.

* Jerry: account master data must be synchronized first
    SELECT SINGLE * INTO @DATA(ls) FROM crmd_bp_mapping
       WHERE c4c_bp = @lv_c4c_id.

    ASSERT sy-subrc = 0.

    rv_id = ls-s4_bp.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C4C_SYNC_TOOL->HANDLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NOTIFICATION_JSON           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle.
    DATA(lv_created_so_url) = get_created_so_url( iv_notification_json ).

    IF lv_created_so_url IS NOT INITIAL.
* handle create event

      DATA: lv_created_so_payload TYPE string.

      send_http_request( EXPORTING iv_url = lv_created_so_url
                         IMPORTING ev_response = lv_created_so_payload  ).

      DATA: ls_created_so TYPE crmt_d_sales_order,
            lt_name_map   TYPE /ui2/cl_json=>name_mappings,
            ls_name_map   LIKE LINE OF lt_name_map.

      ls_name_map = VALUE #( abap = 'DATE_TIME' json = 'DATETIME' ).
      INSERT ls_name_map INTO TABLE lt_name_map.

      CALL METHOD /ui2/cl_json=>deserialize
        EXPORTING
          json          = lv_created_so_payload
          name_mappings = lt_name_map
        CHANGING
          data          = ls_created_so.

      DATA(lt_created_party) = get_created_party( lv_created_so_url ).

      DATA(lv_shipto) = get_mapped_s4_bp_id( it_party_tab = lt_created_party
              iv_role_code = if_sia_constant=>cs_role_code-ship_to ).

      DATA(lt_created_item) = get_created_item_product( lv_created_so_url ).

      DATA(lv_text) = get_created_text( lv_created_so_url ).

      CALL METHOD create_so
        EXPORTING
          is_from_c4c = ls_created_so
          it_product  = lt_created_item
          iv_shipto   = lv_shipto
          iv_text     = lv_text.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_C4C_SYNC_TOOL->LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_FROM_C4C                    TYPE        CRMT_D_SALES_ORDER
* | [--->] IV_CREATED_SO                  TYPE        VBELN_VA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD log.

    DATA: ls_log TYPE crmd_created_so.

    ls_log-c4c_so_id = is_from_c4c-d-results-id.
    ls_log-created_date = sy-datum.
    ls_log-created_time = sy-timlo.
    ls_log-s4_so_id = iv_created_so.

    INSERT crmd_created_so FROM ls_log.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C4C_SYNC_TOOL->SEND_HTTP_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_URL                         TYPE        STRING
* | [<---] EV_RESPONSE                    TYPE        STRING
* | [<---] EV_ERROR_MESSAGE               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_http_request.
    DATA:lo_http_client TYPE REF TO if_http_client,
         lv_status      TYPE i,
         lv_sysubrc     TYPE sysubrc.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              =  'C4C'
      IMPORTING
        client                   =  lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6
    ).
    ASSERT sy-subrc = 0.

    cl_http_utility=>set_request_uri(
      EXPORTING
        request = lo_http_client->request
        uri     = iv_url ).

    CALL METHOD lo_http_client->request->set_method( if_http_request=>co_request_method_get ).

*Disable pop-up when request receives unauthorized error: error 401.
    "lo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

    CALL METHOD lo_http_client->request->set_header_field(
        name  = 'Accept'
        value = 'application/json' ).

*Send request.
    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    ASSERT sy-subrc = 0.

* Get response.
    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    IF sy-subrc <> 0.
      CALL METHOD lo_http_client->get_last_error
        IMPORTING
          code    = lv_sysubrc
          message = ev_error_message.
      "BREAK-POINT.
      WRITE: / 'error: ' , ev_error_message.
      RETURN.
    ENDIF.

    ev_response = lo_http_client->response->get_cdata( ).

    lo_http_client->close( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C4C_SYNC_TOOL->TRIGGER_S4_OUT_DELI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SERVER                      TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD trigger_s4_out_deli.
    DATA: lt_fields TYPE tihttpnvp.

    io_server->response->set_content_type( 'text/html' ).

    io_server->response->set_status( code = 200 reason = 'Success' ).

    io_server->request->get_form_fields( CHANGING fields = lt_fields ).

    READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<soid>) WITH KEY name = 'soid'.

    CHECK sy-subrc = 0.

    DATA:lv_ship_point        TYPE bapidlvcreateheader-ship_point VALUE '0001',
         lv_due_date          TYPE datum VALUE '20181208',
         lv_delivery          TYPE          bapishpdelivnumb-deliv_numb,
         lt_so_items          TYPE TABLE OF bapidlvreftosalesorder,
         ls_so_items          LIKE LINE OF lt_so_items,
         lt_return            TYPE TABLE OF bapiret2,
         ls_read              TYPE order_view,
         lt_item              TYPE TABLE OF bapisdit,
         lt_order_headers_out TYPE TABLE OF bapisdhd,
         lt_header            TYPE TABLE OF sales_key,
         lt_bapisdtehd        TYPE TABLE OF bapisdtehd,
         lt_bapitextli        TYPE TABLE OF bapitextli,
         lv_created_dn        TYPE string,
         lt_bapiret2          TYPE bapiret2.

    APPEND INITIAL LINE TO lt_header ASSIGNING FIELD-SYMBOL(<header>).

    ls_read-item = 'X'.

    SHIFT <soid>-value LEFT DELETING LEADING '0'.

    <header>-vbeln = <soid>-value.

    SELECT SINGLE s4_so_id FROM crmd_created_so INTO <header>-vbeln
       WHERE c4c_so_id = <soid>-value.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'BAPISDORDER_GETDETAILEDLIST'
      EXPORTING
        i_bapi_view     = ls_read
      TABLES
        sales_documents = lt_header
        order_items_out = lt_item.

    LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<item>).
      APPEND INITIAL LINE TO lt_so_items ASSIGNING FIELD-SYMBOL(<fill>).

      <fill>-ref_doc = <item>-doc_number.
      <fill>-ref_item = <item>-itm_number.
      <fill>-dlv_qty = <item>-req_qty.
      <fill>-sales_unit = 'EA'.
    ENDLOOP.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
      EXPORTING
        ship_point        = lv_ship_point
        due_date          = lv_due_date
      IMPORTING
        delivery          = lv_delivery
      TABLES
        sales_order_items = lt_so_items
        return            = lt_return.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<return>).
      "WRITE:/ | Type: { <return>-type }: { <return>-message } | COLOR COL_NEGATIVE.
      IF <return>-type = 'S' and <return>-id = 'VL' and <return>-number = '311'.
         lv_created_dn = <return>-message_v1.
         EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_created_dn IS INITIAL.
       RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = lt_bapiret2.

*    LOOP AT lt_bapiret2 ASSIGNING <return>.
*      WRITE:/ 'Message:', <return>-message COLOR COL_POSITIVE.
*    ENDLOOP.

    io_server->response->set_cdata( lv_created_dn ).
  ENDMETHOD.
ENDCLASS.