CLASS zcl_odata_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_csrf_token_and_cookie
      EXPORTING
        !et_cookies TYPE tihttpcki
        !ev_token TYPE string .
    CLASS-METHODS create_opp
      IMPORTING
        !iv_token TYPE string
        !it_cookies TYPE tihttpcki .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ODATA_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ODATA_TOOL=>CREATE_OPP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TOKEN                       TYPE        STRING
* | [--->] IT_COOKIES                     TYPE        TIHTTPCKI
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_opp.
    DEFINE insert_line.
      lv_body = lv_body && &1.
      lv_body = lv_body && cl_abap_char_utilities=>newline.
    END-OF-DEFINITION.

    DATA:lo_http_client TYPE REF TO if_http_client,
             lv_status      TYPE i,
             lt_fields      TYPE tihttpnvp,
             lv_sysubrc     TYPE sysubrc.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = 'https://qxl-cust233.dev.sapbydesign.com/sap/c4c/odata/v1/c4codata/$batch'
        proxy_host         = 'PROXY.WDF.SAP.CORP'
        proxy_service      = '8080'
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    ASSERT sy-subrc = 0.
    lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.

    CALL METHOD lo_http_client->request->set_method( if_http_request=>co_request_method_post ).

    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'multipart/mixed; boundary=batch_1' ).
    lo_http_client->request->set_header_field( name = 'x-csrf-token' value = iv_token ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = 'Basic V0FOR0pFUlJZNjI4MTg6U2FwdGVzdDE=' ).
    LOOP AT it_cookies ASSIGNING FIELD-SYMBOL(<cookie>).
      lo_http_client->request->set_cookie( name = <cookie>-name
                                           value = <cookie>-value ).
    ENDLOOP.
    DATA: lv_body TYPE string.

    insert_line '--batch_1'.
    insert_line 'Content-Type: multipart/mixed; boundary=changeset_1'.
    lv_body = lv_body && cl_abap_char_utilities=>cr_lf.
*
    insert_line '--changeset_1'.
    insert_line 'Content-Type: application/http'.
    insert_line 'Content-Transfer-Encoding: binary'.
    lv_body = lv_body && cl_abap_char_utilities=>cr_lf.

    insert_line 'POST OpportunityCollection HTTP/1.1'.
    insert_line 'Content-Length: 5000'.
    insert_line 'Accept: application/json'.
    insert_line 'Content-Type: application/json'.

    lv_body = lv_body && cl_abap_char_utilities=>cr_lf.
    insert_line '{'.
    insert_line '"AccountID": "8000018122",'.
    insert_line '"OwnerID": "8000018122",'.
    insert_line '}'.
    insert_line '--changeset_1--'.
    lv_body = lv_body && cl_abap_char_utilities=>cr_lf.
    insert_line '--batch_1--'.

    lo_http_client->request->set_cdata( data = lv_body ).
    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    ASSERT sy-subrc = 0.

    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    IF sy-subrc <> 0.
      CALL METHOD lo_http_client->get_last_error
        IMPORTING
          code    = lv_sysubrc
          message = DATA(ev_message).
      WRITE: / 'error occurred during receive data' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.

    DATA(lv_json) = lo_http_client->response->get_cdata( ).

    WRITE:/ lv_json.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ODATA_TOOL=>GET_CSRF_TOKEN_AND_COOKIE
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_COOKIES                     TYPE        TIHTTPCKI
* | [<---] EV_TOKEN                       TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_csrf_token_and_cookie.
    DATA:  lo_http_client TYPE REF TO if_http_client,
           lv_status      TYPE i,
           lt_fields      TYPE tihttpnvp,
           lv_sysubrc     TYPE sysubrc.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = 'https://qxl-cust233.dev.sapbydesign.com/sap/c4c/odata/v1/c4codata/'
        proxy_host         = 'PROXY.WDF.SAP.CORP'
        proxy_service      = '8080'
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    ASSERT sy-subrc = 0.
    lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.

    CALL METHOD lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
    lo_http_client->request->set_header_field( name = 'x-csrf-token' value = 'Fetch' ).
    lo_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ).
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = 'Basic V0FOR0pFUlJZNjI4MTg6U2FwdGVzdDE=' ).

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    ASSERT sy-subrc = 0.

    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    IF sy-subrc <> 0.
      CALL METHOD lo_http_client->get_last_error
        IMPORTING
          code    = lv_sysubrc
          message = DATA(ev_message).
      WRITE: / 'Error when getting token:', ev_message.
      RETURN.
    ENDIF.

    lo_http_client->response->get_header_fields( CHANGING fields = lt_fields ).

    READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<field>) WITH KEY name = 'x-csrf-token'.
    ev_token = <field>-value.

    lo_http_client->response->get_cookies( CHANGING cookies = et_cookies ).
    lo_http_client->close( ).

  ENDMETHOD.
ENDCLASS.