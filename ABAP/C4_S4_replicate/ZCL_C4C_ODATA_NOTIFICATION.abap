class ZCL_C4C_ODATA_NOTIFICATION definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_C4C_ODATA_NOTIFICATION IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_C4C_ODATA_NOTIFICATION->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_http_extension~handle_request.

    DATA(lv_data) = server->request->get_cdata( ).

    DATA(lv_method) = server->request->get_method( ).
    DATA(lo_c4c_tool) = zcl_c4c_sync_tool=>get_instance( ).

    IF lv_method = 'POST'.
      lo_c4c_tool->handle( lv_data ).
      server->response->set_content_type( 'text/html' ).
      server->response->set_cdata( 'Hello World' ).
      server->response->set_status( code = 200 reason = 'Success' ).
    ELSE.
      lo_c4c_tool->trigger_s4_out_deli( EXPORTING io_server = server ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.