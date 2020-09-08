class ZCL_ZSERVICE_POC_DPC_EXT definition
  public
  inheriting from ZCL_ZSERVICE_POC_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_BEGIN
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~PATCH_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZSERVICE_POC_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.
    READ TABLE it_operation_info ASSIGNING FIELD-SYMBOL(<op>) INDEX 1.
    CHECK sy-subrc = 0 AND <op>-operation_type = 'CE'.

    DATA(lo_tool) = zcl_crm_service_edit_handler=>get_instance( ).
    lo_tool->set_creation_mode( ).
  ENDMETHOD.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CHANGESET_END.
    ZCL_CRM_SERVICE_EDIT_HANDLER=>get_instance( )->edit_and_save( ).
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY.

   check iv_entity_name = 'Z_C_Service_Order_ViewType'.
   data(lo_tool) = ZCL_CRM_SERVICE_EDIT_HANDLER=>get_instance( ).

   lo_tool->create_order( EXPORTING io_data_provider = io_data_provider
                          IMPORTING er_entity = er_entity ).
  endmethod.


  METHOD /iwbep/if_mgw_appl_srv_runtime~patch_entity.
    data(lo_tool) = ZCL_CRM_SERVICE_EDIT_HANDLER=>GET_INSTANCE( ).

    lo_tool->parse_changeset( io_data_provider = io_data_provider it_key = it_key_Tab ).
  ENDMETHOD.
ENDCLASS.