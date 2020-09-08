class ZCL_APC_WSP_EXT_ZORDER_LOG_APC definition
  public
  inheriting from CL_APC_WSP_EXT_STATELESS_BASE
  final
  create public .

public section.

  methods IF_APC_WSP_EXTENSION~ON_START
    redefinition .
  methods IF_APC_WSP_EXTENSION~ON_MESSAGE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APC_WSP_EXT_ZORDER_LOG_APC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_APC_WSP_EXT_ZORDER_LOG_APC->IF_APC_WSP_EXTENSION~ON_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_MESSAGE                      TYPE REF TO IF_APC_WSP_MESSAGE
* | [--->] I_MESSAGE_MANAGER              TYPE REF TO IF_APC_WSP_MESSAGE_MANAGER
* | [--->] I_CONTEXT                      TYPE REF TO IF_APC_WSP_SERVER_CONTEXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IF_APC_WSP_EXTENSION~ON_MESSAGE.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_APC_WSP_EXT_ZORDER_LOG_APC->IF_APC_WSP_EXTENSION~ON_START
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CONTEXT                      TYPE REF TO IF_APC_WSP_SERVER_CONTEXT
* | [--->] I_MESSAGE_MANAGER              TYPE REF TO IF_APC_WSP_MESSAGE_MANAGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_apc_wsp_extension~on_start.
    DATA: lo_request TYPE REF TO if_apc_ws_initial_request.
    DATA: lo_binding TYPE REF TO if_apc_ws_binding_manager.
    DATA: lx_error   TYPE REF TO cx_apc_error.
    DATA: lv_message TYPE string.
    TRY.
        lo_request = i_context->get_initial_request( ).
        lo_binding = i_context->get_binding_manager( ).
        lo_binding->bind_amc_message_consumer( i_application_id = 'ZORDERLOG'
                                               i_channel_id     = '/order_log' ).
      CATCH cx_apc_error INTO lx_error.
        lv_message = lx_error->get_text( ).
        MESSAGE lv_message TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.