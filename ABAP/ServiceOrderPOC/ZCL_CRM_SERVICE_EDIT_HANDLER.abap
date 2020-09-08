class ZCL_CRM_SERVICE_EDIT_HANDLER definition
  public
  final
  create private .

public section.

  methods EDIT_AND_SAVE .
  methods PARSE_CHANGESET
    importing
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER
      !IT_KEY type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_CRM_SERVICE_EDIT_HANDLER .
  methods CREATE_ORDER
    importing
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER
    exporting
      !ER_ENTITY type ref to DATA .
  methods SET_CREATION_MODE .
  methods IS_CREATION
    returning
      value(RV_IS_CREATION) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  types:
    begin of ty_creation,
             description TYPE CRMD_ORDERADM_H-description,
             start_date TYPE zcorderview-requested_start,
             end_Date TYPE zcorderview-requested_end,
             partner_id TYPE zcorderview-partner_id,
         end of ty_creation .

  data MV_ORDER_GUID type CRMT_OBJECT_GUID .
  data MV_POST_DATE type CRMD_ORDERADM_H-POSTING_DATE .
  data MT_ORDERADM_H type CRMT_ORDERADM_H_COMT .
  data MT_INPUT_FIELDS type CRMT_INPUT_FIELD_TAB .
  constants:
    BEGIN OF cs_name,
                 order_guid TYPE string VALUE 'order_guid',
                 POST_DATE TYPE STRING VALUE 'POSTING_DATE',
                 description TYPE string value 'DESCRIPTION',
               END OF cs_name .
  constants:
    BEGIN OF cs_start,
               appt_type TYPE string value 'SRV_CUST_BEG',
               field_name TYPE string value 'TIMESTAMP_FROM',
        end of cs_start .
  constants:
    BEGIN OF cs_end,
               appt_type TYPE string value 'SRV_CUST_END',
               field_name TYPE string value 'TIMESTAMP_FROM',
        end of cs_end .
  class-data SO_INSTANCE type ref to ZCL_CRM_SERVICE_EDIT_HANDLER .
  data MT_CHANGEABLE_FIELDS type STRING_TABLE .
  data MV_START_DATE type SCAPPTSEG-TST_FROM .
  data MV_END_DATE type SCAPPTSEG-TST_FROM .
  data MV_DESCRIPTION type CRMD_ORDERADM_H-DESCRIPTION .
  data MT_APPOINTMENT type CRMT_APPOINTMENT_COMT .
  data MV_CREATION_MODE type ABAP_BOOL .
  data MS_CREATION_DATA type TY_CREATION .
  data CV_ORDER_TYPE type CRMD_ORDERADM_H-PROCESS_TYPE value 'SRVO' ##NO_TEXT.
  data MT_PARTNER type CRMT_PARTNER_COMT .

  methods POST_SAVE
    exporting
      !ER_ENTITY type ref to DATA .
  methods PREPARE_CREATE .
  methods MAINTAIN_AND_SAVE .
  methods CONSTRUCTOR .
  methods PREPARE_CHANGE .
  methods PREPARE_DATE_FOR_CHANGE
    importing
      !IS_DATE_INFO type DATA
      !IV_DATE_VALUE type SCAPPTSEG-TST_FROM .
ENDCLASS.



CLASS ZCL_CRM_SERVICE_EDIT_HANDLER IMPLEMENTATION.


  method CONSTRUCTOR.
    append 'REQUESTED_START' TO mt_changeable_fields.
    append 'REQUESTED_END' TO mt_changeable_fields.
    append 'DESCRIPTION' TO mt_changeable_fields.
    "append '' TO mt_changeable_fields.
  endmethod.


  method CREATE_ORDER.
DATA: ls_header TYPE zcorderview.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_header ).
    ms_creation_data-description = ls_header-description.
    ms_creation_data-start_Date = ls_header-requested_start.
    ms_creation_data-end_date = ls_header-requested_end.
    ms_creation_data-partner_id = ls_header-partner_id.

    prepare_create( ).
    maintain_and_save( ).
    post_save( IMPORTING er_entity = er_entity ).


  endmethod.


  METHOD edit_and_save.
    IF mv_creation_mode = abap_false.
      prepare_change( ).
      maintain_and_save( ).
    ENDIF.
  ENDMETHOD.


  method GET_INSTANCE.
    IF so_instance IS INITIAL.
       so_instance = new ZCL_CRM_SERVICE_EDIT_HANDLER( ).
    ENDIF.

    ro_instance = so_instance.
  endmethod.


  method IS_CREATION.
     rv_is_creation = mv_creation_mode.
  endmethod.


  METHOD maintain_and_save.

    DATA: lt_objects_to_save   TYPE crmt_object_guid_tab,
          lt_saved_objects     TYPE crmt_return_objects,
          lt_objects_not_saved TYPE crmt_object_guid_tab,
          lt_exception         TYPE CRMT_EXCEPTION_T.

    CALL FUNCTION 'CRM_ORDER_MAINTAIN'
      EXPORTING
        it_appointment    = mt_appointment
        it_partner        = mt_partner
      IMPORTING
        et_exception      = lt_exception
      CHANGING
        ct_orderadm_h     = mt_orderadm_h
        ct_input_fields   = mt_input_fields
      EXCEPTIONS
        error_occurred    = 1
        document_locked   = 2
        no_change_allowed = 3
        no_authority      = 4
        OTHERS            = 5.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'CRM_ORDER_CHECK_BEFORE_SAVE_OW'
      EXPORTING
        iv_order_guid  = mv_order_guid
*       IV_LOG_HANDLE  =
      EXCEPTIONS
        save_cancelled = 1
        OTHERS         = 2.

    CHECK sy-subrc EQ 0.

    APPEND mv_order_guid TO lt_objects_to_save.
    CLEAR: lt_exception.

    CALL FUNCTION 'CRM_ORDER_SAVE'
      EXPORTING
        it_objects_to_save   = lt_objects_to_save
        iv_update_task_local = abap_true
      IMPORTING
        et_saved_objects     = lt_saved_objects
        et_objects_not_saved = lt_objects_not_saved
        et_exception         = lt_exception
      EXCEPTIONS
        document_not_saved   = 1
        OTHERS               = 2.

    CHECK sy-subrc = 0.
  ENDMETHOD.


  METHOD parse_changeset.
    DATA: ls_header TYPE zcorderview.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_header ).

    LOOP AT mt_changeable_fields ASSIGNING FIELD-SYMBOL(<field>).
      ASSIGN COMPONENT <field> OF STRUCTURE ls_header TO FIELD-SYMBOL(<value>).
      CHECK sy-subrc = 0 AND <value> IS NOT INITIAL.
      CASE <field>.
        WHEN 'DESCRIPTION'.
           mv_description = <value>.
        WHEN 'REQUESTED_START'.
           mv_start_Date = <value>.
        WHEN 'REQUESTED_END'.
           mv_end_Date = <value>.
      ENDCASE.
    ENDLOOP.
    "mv_post_date = ls_header-posting_date.

    READ TABLE it_key ASSIGNING FIELD-SYMBOL(<key>) WITH KEY name = 'order_guid'.
    CHECK sy-subrc = 0.

    mv_order_guid = <key>-value.

  ENDMETHOD.


  method POST_SAVE.
    FIELD-SYMBOLS: <any> TYPE any.

    DATA: ls_order TYPE crmd_orderadm_h.

    COMMIT WORK AND WAIT.
    DATA(ls_header) = value zcorderview( description = ms_creation_data-description
    order_guid = mv_order_guid
    requested_start = ms_creation_data-start_Date requested_end = ms_creation_data-end_date ).

    SELECT single object_id posting_Date into CORRESPONDING FIELDS OF ls_order FROM crmd_orderadm_h
       WHERE guid = mv_order_guid.
    ASSERT sy-subrc = 0.
    ls_header-object_id = ls_order-object_id.
    ls_header-posting_date = ls_order-posting_date.
    ls_header-partner_id = ms_creation_data-partner_id.
    ls_header-txt04 = 'OPEN'.

    CREATE data er_entity LIKE ls_header.
    ASSIGN er_entity->* TO <any>.
    <any> = ls_header.
  endmethod.


  METHOD prepare_change.

    IF mv_description IS NOT INITIAL.
      DATA(ls_header) = value CRMT_ORDERADM_H_COM( description = mv_description guid = mv_order_guid ).
      APPEND ls_header TO mt_orderadm_h.
      DATA(ls_input) = VALUE crmt_input_field( ref_guid = mv_order_guid objectname = 'ORDERADM_H').
      DATA(ls_name) = VALUE crmt_input_field_names( fieldname = cs_name-description ).

      INSERT ls_name INTO TABLE ls_input-field_names.
      INSERT ls_input INTO TABLE mt_input_fields.
    ENDIF.

    prepare_date_for_change( is_date_info = cs_start iv_date_value = MV_START_DATE ).

    prepare_date_for_change( is_date_info = cs_end iv_date_value = MV_end_DATE ).

  ENDMETHOD.


  METHOD prepare_create.
    DATA:
      ls_input_field      LIKE LINE OF mt_input_fields,
      ls_field_name       LIKE LINE OF ls_input_field-field_names,
      ls_orderadm_h_com   LIKE LINE OF mt_orderadm_h,
      ls_partner          LIKE LINE OF mt_partner,
      ls_partner_control  TYPE crmt_partner_control,
      ls_com_structure    TYPE string,
      lv_selection_needed TYPE  crmt_boolean,
      lv_is_buying_center TYPE  abap_bool,
      lt_attributes_com   TYPE  crmt_partner_attribute_com_tab,
      lt_save             TYPE crmt_object_guid_tab,
      lt_saved            TYPE crmt_return_objects,
      ls_saved            LIKE LINE OF lt_saved,
      lt_org_man          TYPE crmt_orgman_comt,
      ls_org_man          LIKE LINE OF lt_org_man.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = mv_order_guid.

    ls_orderadm_h_com-guid = mv_order_guid.
    ls_orderadm_h_com-description = ms_creation_data-description.
    ls_orderadm_h_com-process_type = cv_order_type.
    ls_orderadm_h_com-mode = 'A'.
    APPEND ls_orderadm_h_com TO mt_orderadm_h.

    ls_input_field-ref_guid = mv_order_guid.
    ls_input_field-ref_kind = 'A'.
    ls_input_field-objectname = 'ORDERADM_H'.

* sorted table
    ls_field_name-fieldname = 'DESCRIPTION'.
    APPEND ls_field_name TO ls_input_field-field_names.
    ls_field_name-fieldname = 'MODE'.
    APPEND ls_field_name TO ls_input_field-field_names.
    ls_field_name-fieldname = 'PROCESS_TYPE'.
    APPEND ls_field_name TO ls_input_field-field_names.

    APPEND ls_input_field TO mt_input_fields.

    IF ms_creation_data-partner_id IS NOT INITIAL.
      CLEAR: ls_input_field.
      ls_input_field-ref_guid = mv_order_guid.
      ls_input_field-ref_kind = 'A'.
      ls_input_field-objectname = 'PARTNER'.

      CLEAR: ls_field_name.
      ls_field_name-fieldname = 'DISPLAY_TYPE'.
      APPEND ls_field_name TO ls_input_field-field_names.

      ls_field_name-fieldname = 'KIND_OF_ENTRY'.
      APPEND ls_field_name TO ls_input_field-field_names.

      ls_field_name-fieldname = 'NO_TYPE'.
      APPEND ls_field_name TO ls_input_field-field_names.

      ls_field_name-fieldname = 'PARTNER_FCT'.
      APPEND ls_field_name TO ls_input_field-field_names.

      ls_field_name-fieldname = 'PARTNER_NO'.
      APPEND ls_field_name TO ls_input_field-field_names.
      INSERT ls_input_field INTO TABLE mt_input_fields.

      ls_partner-ref_guid = mv_order_guid.
      ls_partner-ref_kind = 'A'.
      ls_partner-kind_of_entry = 'C'.
      ls_partner-partner_fct = '00000001'. "sold to party
      ls_partner-partner_no = ms_creation_data-partner_id.
      ls_partner-display_type = 'BP'.
      ls_partner-no_type = 'BP'.
      APPEND ls_partner TO mt_partner.

    ENDIF.

    IF ms_creation_data-start_date IS NOT INITIAL AND ms_creation_data-end_date IS NOT INITIAL.

      prepare_date_for_change( is_date_info = cs_start iv_date_value = ms_creation_data-start_date ).

      prepare_date_for_change( is_date_info = cs_end iv_date_value = ms_creation_data-end_date ).

    ENDIF.
*
*  ls_input_field-ref_handle = '0000000000'.
*  ls_input_field-logical_key = '0000'.
*  APPEND ls_input_field TO lt_input_fields.
*
** input field for Org management
*  CLEAR: ls_input_field,ls_field_name.
*  ls_field_name-fieldname = 'DIS_CHANNEL'.
*  APPEND ls_field_name TO ls_input_field-field_names.
*
*  ls_field_name-fieldname = 'DIVISION'.
*  APPEND ls_field_name TO ls_input_field-field_names.
*
*  ls_field_name-fieldname = 'NO_DETERMINATION'.
*  APPEND ls_field_name TO ls_input_field-field_names.
*
*  ls_field_name-fieldname = 'SALES_ORGR_ORI'.
*  APPEND ls_field_name TO ls_input_field-field_names.
*
*  ls_field_name-fieldname = 'SALES_ORG_RESP'.
*  APPEND ls_field_name TO ls_input_field-field_names.
*
*  ls_input_field-ref_guid = mv_order_guid.
*  ls_input_field-ref_kind = 'A'.
*  ls_input_field-objectname = 'ORGMAN'.
*

  ENDMETHOD.


  METHOD prepare_date_for_change.

    CHECK iv_date_value IS NOT INITIAL.
    ASSIGN COMPONENT 'APPT_TYPE' OF STRUCTURE is_date_info TO FIELD-SYMBOL(<type>).
    ASSERT sy-subrc = 0.
    ASSIGN COMPONENT 'FIELD_NAME' OF STRUCTURE is_date_info TO FIELD-SYMBOL(<field>).

    DATA(ls_header) = VALUE crmt_appointment_com( ref_guid = mv_order_guid ref_kind = 'A'
     appt_type = <type> timestamp_from = iv_date_value mode = 'B' ).
    APPEND ls_header TO mt_appointment.

    DATA(ls_input) = VALUE crmt_input_field( ref_guid = mv_order_guid ref_kind = 'A' objectname = 'APPOINTMENT'
    logical_key = <type> ).
    DATA(ls_name) = VALUE crmt_input_field_names( fieldname = <field> ).

    INSERT ls_name INTO TABLE ls_input-field_names.
    INSERT ls_input INTO TABLE mt_input_fields.

  ENDMETHOD.


  method SET_CREATION_MODE.
    mv_creation_mode = abap_true.
  endmethod.
ENDCLASS.