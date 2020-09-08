FUNCTION ZJERRYGET_ATTACHMENTS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_OBJECTS) TYPE  CRMT_OBJECT_KEY_T
*"  CHANGING
*"     VALUE(CT_ATTACHMENTS) TYPE  CRMT_ODATA_TASK_ATTACHMENTT
*"----------------------------------------------------------------------

DATA(lo_tool) = new zcl_crm_attachment_tool( ).

ct_attachments = lo_tool->get_attachments_origin( it_objects ).

ENDFUNCTION.