class ZCL_C4C_OPP_MONITOR definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_C4C_OPP_MONITOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C4C_OPP_MONITOR->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD if_http_extension~handle_request.

  DATA(lv_payload) = server->request->get_cdata( ).

  CALL METHOD zcl_c4c_crm_integration_tool=>parse_json_to_internal_table
    EXPORTING
      iv_json = lv_payload
    IMPORTING
      et_node = DATA(lt_node).

  READ TABLE lt_node ASSIGNING FIELD-SYMBOL(<event>) WITH KEY attribute = 'event'.

  DATA(lv_event) = <event>-value.

  READ TABLE lt_node ASSIGNING FIELD-SYMBOL(<opp>) WITH KEY attribute = 'odataServiceEndpoint'.

  DATA(lv_opp_detail) = zcl_c4c_crm_integration_tool=>get_opp_detail( <opp>-value ).
  CLEAR: lt_node.
  CALL METHOD zcl_c4c_crm_integration_tool=>parse_json_to_internal_table
    EXPORTING
      iv_json = lv_opp_detail
    IMPORTING
      et_node = lt_node.
  BREAK-POINT.
ENDMETHOD.
ENDCLASS.