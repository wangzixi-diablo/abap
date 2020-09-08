class ZCL_C4C_CRM_INTEGRATION_TOOL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_sorted_node,
        index     TYPE string,
        attribute TYPE string,
        value     TYPE string,
      END OF ty_sorted_node .
  types:
    tt_sorted_node TYPE STANDARD TABLE OF ty_sorted_node .

  class-methods PARSE_JSON_TO_INTERNAL_TABLE
    importing
      !IV_JSON type STRING
    exporting
      !ET_NODE type TT_SORTED_NODE
      !EV_NODE_NUMBER type INT4 .
  class-methods GET_OPP_DETAIL
    importing
      !IV_URL type STRING
    returning
      value(RV_DETAIL) type STRING .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_level,
        level     TYPE i,
        indicator TYPE string,
      END OF ty_level .
  types:
    tt_level TYPE STANDARD TABLE OF ty_level WITH KEY level .
  types:
    BEGIN OF ty_node,
        node_type TYPE string,
        prefix    TYPE string,
        name      TYPE string,
        nsuri     TYPE string,
        value     TYPE string,
        value_raw TYPE xstring,
      END OF ty_node .
  types:
    tt_node TYPE TABLE OF ty_node .

  constants GC_JSON_OPEN_ELEMENT type STRING value 'open element' ##no_text .
  constants GC_JSON_ATTRIBUTE type STRING value 'attribute' ##no_text .
  constants GC_JSON_CLOSE_ELEMENT type STRING value 'close element' ##no_text .
  constants GC_JSON_VALUE type STRING value 'value' ##no_text .
  constants GC_JSON_ERROR type STRING value 'Error' ##no_text .
  class-data SV_URL type STRING .
  class-data SV_MAX_NUMBER_IN_DB type INT4 .
  class-data SV_REPO_SHORT_NAME type CHAR4 .
  class-data SV_MAX_NUMBER_REACHED type ABAP_BOOL .

  class-methods PARSE_JSON_TO_RAW_TABLE
    importing
      !IV_JSON type STRING
    exporting
      !ET_NODE type TT_NODE
    exceptions
      JSON_PARSE_ERROR .
  class-methods SORT_RAW_TABLE
    importing
      !IT_NODE type TT_NODE
    exporting
      !ET_SORTED_NODE type TT_SORTED_NODE
      !EV_NODE_NUMBER type INT4 .
ENDCLASS.



CLASS ZCL_C4C_CRM_INTEGRATION_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_C4C_CRM_INTEGRATION_TOOL=>GET_OPP_DETAIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_URL                         TYPE        STRING
* | [<-()] RV_DETAIL                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_OPP_DETAIL.
  DATA:lo_http_client TYPE REF TO if_http_client,
         lv_status      TYPE i,
         lt_fields      TYPE tihttpnvp,
         lv_sysubrc     TYPE sysubrc.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = iv_url
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

    CALL METHOD lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
    lo_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ).
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
      WRITE: / `error occurred during receive data:`, ev_message.
      RETURN.
    ENDIF.

    rv_detail = lo_http_client->response->get_cdata( ).
    lo_http_client->close( ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_C4C_CRM_INTEGRATION_TOOL=>PARSE_JSON_TO_INTERNAL_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* | [<---] ET_NODE                        TYPE        TT_SORTED_NODE
* | [<---] EV_NODE_NUMBER                 TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD PARSE_JSON_TO_INTERNAL_TABLE.
    DATA lt_raw_node TYPE tt_node.

    CALL METHOD parse_json_to_raw_table
      EXPORTING
        iv_json          = iv_json
      IMPORTING
        et_node          = lt_raw_node
      EXCEPTIONS
        json_parse_error = 1
        OTHERS           = 2.

    ASSERT sy-subrc = 0.

    CALL METHOD sort_raw_table
      EXPORTING
        it_node        = lt_raw_node
      IMPORTING
        et_sorted_node = et_node
        ev_node_number = ev_node_number.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_C4C_CRM_INTEGRATION_TOOL=>PARSE_JSON_TO_RAW_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* | [<---] ET_NODE                        TYPE        TT_NODE
* | [EXC!] JSON_PARSE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD PARSE_JSON_TO_RAW_TABLE.
    DATA:
      node_wa TYPE ty_node.

    DATA(json) = cl_abap_codepage=>convert_to( iv_json ).
    DATA(reader) = cl_sxml_string_reader=>create( json ).

    TRY.
        DO.
          CLEAR node_wa.
          DATA(node) = reader->read_next_node( ).
          IF node IS INITIAL.
            EXIT.
          ENDIF.
          CASE node->type.
            WHEN if_sxml_node=>co_nt_element_open.
              DATA(open_element) = CAST if_sxml_open_element( node ).
              node_wa-node_type = gc_json_open_element.
              node_wa-prefix    = open_element->prefix.
              node_wa-name      = open_element->qname-name.
              node_wa-nsuri     = open_element->qname-namespace.
              DATA(attributes)  = open_element->get_attributes( ).
              APPEND node_wa TO et_node.
              LOOP AT attributes INTO DATA(attribute).
                node_wa-node_type = gc_json_attribute.
                node_wa-prefix    = attribute->prefix.
                node_wa-name      = attribute->qname-name.
                node_wa-nsuri     = attribute->qname-namespace.
                IF attribute->value_type = if_sxml_value=>co_vt_text.
                  node_wa-value = attribute->get_value( ).
                ELSEIF attribute->value_type =
                                   if_sxml_value=>co_vt_raw.
                  node_wa-value_raw = attribute->get_value_raw( ).
                ENDIF.
                APPEND node_wa TO et_node.
              ENDLOOP.
              CONTINUE.
            WHEN if_sxml_node=>co_nt_element_close.
              DATA(close_element) = CAST if_sxml_close_element( node ).
              node_wa-node_type   = gc_json_close_element.
              node_wa-prefix      = close_element->prefix.
              node_wa-name        = close_element->qname-name.
              node_wa-nsuri       = close_element->qname-namespace.
              APPEND node_wa TO et_node.
              CONTINUE.
            WHEN if_sxml_node=>co_nt_value.
              DATA(value_node) = CAST if_sxml_value_node( node ).
              node_wa-node_type   = gc_json_value.
              IF value_node->value_type = if_sxml_value=>co_vt_text.
                node_wa-value = value_node->get_value( ).
              ELSEIF value_node->value_type = if_sxml_value=>co_vt_raw.
                node_wa-value_raw = value_node->get_value_raw( ).
              ENDIF.
              APPEND node_wa TO et_node.
              CONTINUE.
            WHEN OTHERS.
              node_wa-node_type   = gc_json_error.
              APPEND node_wa TO et_node.
              EXIT.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error INTO DATA(parse_error).
        RAISE json_parse_error.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_C4C_CRM_INTEGRATION_TOOL=>SORT_RAW_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_NODE                        TYPE        TT_NODE
* | [<---] ET_SORTED_NODE                 TYPE        TT_SORTED_NODE
* | [<---] EV_NODE_NUMBER                 TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SORT_RAW_TABLE.
    DATA:
      ls_node              TYPE ty_node,
      lv_level_counter     TYPE i VALUE 0,
      lv_attribute_name    TYPE string,
      lv_seperator         TYPE char1,
      ls_sorted_node       TYPE ty_sorted_node,
      lv_node_counter      TYPE i VALUE 1,
      lv_node_flag_counter TYPE i,
      lt_level_tab         TYPE tt_level,
      ls_level_tab         TYPE ty_level,
      lv_index             TYPE i,
      lv_temp_counter      TYPE i.

    FIELD-SYMBOLS <fs_level_tab> TYPE ty_level.

    LOOP AT it_node INTO ls_node.
*Check if open element, if yes increase level counter
      IF  ls_node-node_type = gc_json_open_element.
        lv_level_counter = lv_level_counter + 1.

*Check if it is new node, if yes increase node counter
        IF lv_node_flag_counter IS NOT INITIAL AND lv_level_counter = lv_node_flag_counter.
          lv_node_counter = lv_node_counter + 1.
        ENDIF.

*Add level indicator to level table in order to remember which level we are in
        CLEAR ls_level_tab.
        READ TABLE lt_level_tab INTO ls_level_tab WITH TABLE KEY level = lv_level_counter.
        IF ls_level_tab IS INITIAL.
          ls_level_tab-level = lv_level_counter.
          APPEND ls_level_tab TO lt_level_tab.
        ENDIF.
      ENDIF.

*Check if attribute
      IF  ls_node-node_type = gc_json_attribute.
*If no entry in our generated result table then me mark current level as the begining of each node
        IF et_sorted_node IS INITIAL.
          lv_node_flag_counter = lv_level_counter - 1.
        ENDIF.

        LOOP AT lt_level_tab ASSIGNING <fs_level_tab> WHERE level = lv_level_counter.
          <fs_level_tab>-indicator =  ls_node-value.
        ENDLOOP.
      ENDIF.


*Check if value
*-------------------------------------------------------------------------
*Add level indicator to level table in order to show hierachy node
*For instance if we have following node hieracy
*   -A
*     -a
*     -b
*we wil have following naming convertion in our generated table
*  A-a  &  A-b
*-------------------------------------------------------------------------
      IF  ls_node-node_type = gc_json_value.
        CLEAR lv_attribute_name.
        LOOP AT lt_level_tab ASSIGNING <fs_level_tab> FROM 0 TO lv_level_counter.
          IF <fs_level_tab>-indicator IS NOT INITIAL.
            CONCATENATE lv_attribute_name '-' <fs_level_tab>-indicator INTO lv_attribute_name.
          ENDIF.
        ENDLOOP.

        CLEAR: lv_seperator, lv_index.
        lv_seperator = lv_attribute_name+0(1).
        IF lv_seperator = '-'.
          lv_index = strlen( lv_attribute_name ) - 1.
          lv_attribute_name = lv_attribute_name+1(lv_index).
        ENDIF.

        IF lv_attribute_name IS NOT INITIAL.
          ls_sorted_node-attribute = lv_attribute_name.
          ls_sorted_node-value =  ls_node-value.
          ls_sorted_node-index = lv_node_counter.
          APPEND ls_sorted_node TO et_sorted_node.
        ENDIF.
        CLEAR: ls_sorted_node.
      ENDIF.

*Check if close element
      IF  ls_node-node_type = gc_json_close_element.
        lv_level_counter = lv_level_counter - 1.

*Remove level indicator from level table
        DESCRIBE TABLE lt_level_tab LINES lv_temp_counter.
        LOOP AT lt_level_tab ASSIGNING <fs_level_tab> FROM lv_level_counter + 1 TO lv_temp_counter.
          <fs_level_tab>-indicator = ''.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

*Return total number of nodes
    ev_node_number = lv_node_counter.
  ENDMETHOD.
ENDCLASS.