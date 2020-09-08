REPORT zcrms4_create_orders_loop.

INCLUDE crm_direct.

TABLES              mara.
TABLES              but000.
TABLES              crmd_orderadm_h.

TYPES               BEGIN OF ty_org_assignment.
TYPES                 crm_sales_org        TYPE  crmt_sales_org.
TYPES                 s4_sales_org         TYPE  vkorg.
TYPES               END OF ty_org_assignment.

DATA                lv_char10              TYPE  char10.
DATA                lv_description         TYPE  crmt_process_description.
DATA                lv_handle              TYPE  crmt_handle.
DATA                lv_header_guid         TYPE  crmt_object_guid.
DATA                lv_item_guid           TYPE  crmt_object_guid.

DATA                gt_orderadm_h          TYPE  crmt_orderadm_h_comt.
DATA                gt_orderadm_i          TYPE  crmt_orderadm_i_comt.
DATA                gt_schedlin_i_com      TYPE  crmt_schedlin_i_comt.
DATA                gt_input_fields        TYPE  crmt_input_field_tab.
DATA                gt_partner             TYPE  crmt_partner_comt.
DATA                gt_orgman              TYPE  crmt_orgman_comt.
DATA                gt_appointment         TYPE  crmt_appointment_comt.
DATA                gt_product_i           TYPE  crmt_product_i_comt.
DATA                gt_status              TYPE  crmt_status_comt.
DATA                gt_obj_guids           TYPE  crmt_object_guid_tab.
DATA                gt_proc_type           TYPE  STANDARD TABLE OF crmc_proc_type.
DATA                gv_log_handle          TYPE  balloghndl.
DATA                gt_saved_objects       TYPE  crmt_return_objects.
DATA                gt_service_orgs        TYPE  hrtb_objkey.
DATA                gt_matnr               TYPE STANDARD TABLE OF matnr.
DATA                gt_partner_no          TYPE STANDARD TABLE OF bu_partner.
DATA                gt_tvko                TYPE STANDARD TABLE OF tvko.
DATA                gt_tvta                TYPE STANDARD TABLE OF tvta.

DATA                gt_org_assignment      TYPE STANDARD TABLE OF ty_org_assignment.
DATA                gv_date_start          TYPE sydatum.

PARAMETERS          p_title                TYPE text30.
PARAMETERS          p_number               TYPE i.
PARAMETERS          p_maxit                TYPE i DEFAULT 30.
PARAMETERS          p_maxqu                TYPE i DEFAULT 100.
PARAMETERS          p_type                 TYPE crmd_orderadm_h-process_type OBLIGATORY DEFAULT 'SRVO'.
SELECT-OPTIONS      s_matnr                FOR  mara-matnr.
SELECT-OPTIONS      s_part                 FOR  but000-partner.
SELECT-OPTIONS      s_date                 FOR  crmd_orderadm_h-posting_date.

CONSTANTS: cv_package_size TYPE int4 VALUE 100.

START-OF-SELECTION.
  cl_crm_order_timer_home=>start( ).

  DATA(go_prng_no_items) = cl_abap_random_int=>create( min = 1 max = p_maxit ).

  SELECT matnr FROM mara INTO TABLE gt_matnr WHERE matnr IN s_matnr
    ORDER BY PRIMARY KEY.

  DESCRIBE TABLE gt_matnr LINES DATA(lv_no_of_materials).

  DATA(go_prng_material) = cl_abap_random_int=>create( min = 1 max = lv_no_of_materials ).

  SELECT partner FROM but000 INTO TABLE gt_partner_no
    WHERE partner IN s_part ORDER BY PRIMARY KEY.

  DESCRIBE TABLE gt_partner_no LINES DATA(lv_no_of_partners).

  DATA(go_prng_partner) = cl_abap_random_int=>create( min = 1 max = lv_no_of_partners ).

  CALL METHOD cl_crm_org_management=>get_instance
    IMPORTING
      ev_instance = DATA(go_org_mgmt).

  SELECT * FROM tvko INTO TABLE gt_tvko ORDER BY PRIMARY KEY.
  LOOP AT gt_tvko INTO DATA(ls_tvko).
    DATA(ls_org_assignment) = VALUE ty_org_assignment( s4_sales_org = ls_tvko-vkorg ).

    CALL METHOD go_org_mgmt->get_sales_org_of_vkorg
      EXPORTING
        iv_vkorg            = ls_tvko-vkorg
      IMPORTING
        ev_sales_org        = ls_org_assignment-crm_sales_org
      EXCEPTIONS
        crm_key_not_defined = 1
        OTHERS              = 2.
    ASSERT sy-subrc EQ 0.
    INSERT ls_org_assignment INTO TABLE gt_org_assignment.
  ENDLOOP.

  SELECT * FROM tvta INTO TABLE gt_tvta ORDER BY PRIMARY KEY.
  LOOP AT gt_tvta INTO DATA(ls_tvta).
    READ TABLE gt_org_assignment TRANSPORTING NO FIELDS
      WITH KEY s4_sales_org = ls_tvta-vkorg.
    IF sy-subrc NE 0.
      DELETE gt_tvta.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE gt_tvta LINES DATA(lv_no_of_sales_areas).
  DATA(go_prng_sales_area) = cl_abap_random_int=>create( min = 1 max = lv_no_of_sales_areas ).

  CALL METHOD cl_crm_orgman_services=>list_service_orgs
    IMPORTING
      service_orgs = gt_service_orgs.
  DESCRIBE TABLE gt_service_orgs LINES DATA(lv_no_of_service_orgs).
  DATA(go_prng_service_org) = cl_abap_random_int=>create( min = 1 max = lv_no_of_service_orgs ).

  DESCRIBE TABLE s_date LINES DATA(lv_date).
  IF lv_date NE 1.
    MESSAGE i398(00) WITH 'Enter exactly one interval for the date' space space space.
    EXIT.
  ENDIF.
  READ TABLE s_date INTO DATA(ls_date) INDEX 1.
  IF ls_date-sign NE 'I' OR ls_date-option NE 'BT'.
    MESSAGE i398(00) WITH 'Enter exactly one interval for the date' space space space.
    EXIT.
  ENDIF.
  gv_date_start = ls_date-low.
  DATA(lv_no_of_days) = ls_date-high - ls_date-low.

  DATA(go_prng_date) = cl_abap_random_int=>create( min = 1 max = lv_no_of_days ).

  DATA(go_prng_quantity) = cl_abap_random_int=>create( min = 1 max = p_maxqu ).

  SELECT * FROM crmc_proc_type INTO TABLE gt_proc_type
    WHERE object_type = gc_object_type-service ORDER BY PRIMARY KEY.

  DESCRIBE TABLE gt_proc_type LINES DATA(lv_no_of_proc_types).

  DATA(lo_price) = cl_crms4_pricing_factory=>get_pricing_adapter( ).
  DATA(go_prng_proc_type) = cl_abap_random_int=>create( min = 1 max = lv_no_of_proc_types ).

  DATA(left) = p_number.

  cl_crm_org_management=>get_instance( IMPORTING ev_instance = DATA(lo_org) ).
  WHILE left > 0.
    DATA(num) = nmin( val1 = left val2 = cv_package_size ).
    PERFORM handle_order_with_number USING num.
    lo_org->reset_buffers( ).
    left = left - cv_package_size.
  ENDWHILE.

  cl_crm_order_timer_home=>stop( |Time consumed for Order creation with number: { p_number }| ).

FORM handle_order_with_number USING iv_num TYPE int4.
  DATA: lt_head_guid_4_price TYPE crmt_object_guid_tab.

  CLEAR: lv_handle, gt_orderadm_h, gt_partner, gt_orgman, gt_appointment, gt_orderadm_i,
  gt_schedlin_i_com, gt_obj_guids, gt_input_fields.
  DO iv_num TIMES.
    lv_char10 = sy-index.
    lv_description = p_title && lv_char10.
    ADD 1 TO lv_handle.
    PERFORM orderadm_h_create USING    lv_description
                              CHANGING lv_header_guid.
    APPEND lv_header_guid TO lt_head_guid_4_price.

    PERFORM partner_create    USING    lv_header_guid
                                       gc_object_kind-orderadm_h.
    PERFORM orgman_create     USING    lv_header_guid
                                       gc_object_kind-orderadm_h.
    PERFORM dates_create      USING    lv_header_guid
                                       gc_object_kind-orderadm_h.

    IF p_type = 'SRVO' OR p_type = 'SRVC'.
      DATA(lv_no_items) = go_prng_no_items->get_next( ).
      DO lv_no_items TIMES.
        PERFORM orderadm_i_create USING    lv_header_guid CHANGING lv_item_guid.
        PERFORM schedlin_create   USING    lv_item_guid.
      ENDDO.
    ENDIF.
  ENDDO.
  PERFORM create_orders.
  PERFORM save_orders.
  PERFORM order_cleanup USING lt_head_guid_4_price.

ENDFORM.

FORM order_cleanup CHANGING ct_header_guid TYPE crmt_object_guid_tab.

  "  lo_price->cleanup_buffer( ).
* suggested by Meng Ge
  LOOP AT ct_header_guid ASSIGNING FIELD-SYMBOL(<guid>).
    CALL FUNCTION 'CRM_PRIDOC_INIT_EC'
      EXPORTING
        iv_object_name   = space
        iv_event_exetime = 088
        iv_event         = space
        iv_header_guid   = <guid>.
  ENDLOOP.

  CALL FUNCTION 'CRM_ORDER_INITIALIZE'
    EXPORTING
      iv_initialize_whole_buffer = 'X'.

  CLEAR: cl_crms4_product_api=>gt_product_buffer, gt_input_fields, ct_header_guid.
ENDFORM.
FORM orderadm_h_create
  USING    iv_description       TYPE crmt_process_description
  CHANGING cv_header_guid       TYPE crmt_object_guid RAISING cx_uuid_error.

  DATA     ls_orderadm_h        TYPE  crmt_orderadm_h_com.
  DATA     ls_input_field       TYPE  crmt_input_field.
  DATA     ls_input_field_names TYPE  crmt_input_field_names.
  ls_orderadm_h-mode         = gc_mode-create.
  ls_orderadm_h-process_type = p_type.
  ls_orderadm_h-description  = iv_description.

  ls_orderadm_h-guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).

  INSERT ls_orderadm_h INTO TABLE gt_orderadm_h.

  ls_input_field-ref_guid   = ls_orderadm_h-guid.
  ls_input_field-ref_kind   = gc_object_kind-orderadm_h.
  ls_input_field-objectname = gc_object_name-orderadm_h.

  ls_input_field_names-fieldname = 'PROCESS_TYPE'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'DESCRIPTION'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.

  INSERT ls_input_field INTO TABLE gt_input_fields.
  cv_header_guid = ls_orderadm_h-guid.
ENDFORM.

FORM partner_create USING               iv_ref_guid           TYPE  crmt_object_guid
                      iv_ref_kind           TYPE  crmt_object_kind.
  DATA                lv_index              TYPE  i.
  DATA                lv_partner_no         TYPE  bu_partner.
  DATA                ls_partner_com        TYPE  crmt_partner_com.
  DATA                ls_input_field        TYPE  crmt_input_field.
  DATA                ls_input_field_names  TYPE  crmt_input_field_names.

  ls_input_field_names-fieldname = 'DISPLAY_TYPE'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'KIND_OF_ENTRY'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'NO_TYPE'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'PARTNER_FCT'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'PARTNER_NO'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.

  ls_partner_com-ref_guid           = iv_ref_guid.
  ls_partner_com-ref_kind           = iv_ref_kind.
  ls_partner_com-ref_partner_handle = 1.
  ls_partner_com-kind_of_entry      = 'C'.
  ls_partner_com-partner_fct        = '00000001'. "sold-to party
  ls_partner_com-no_type            = 'BP'.
  ls_partner_com-display_type       = 'BP'.
  lv_index = go_prng_partner->get_next( ).
  READ TABLE gt_partner_no INTO lv_partner_no INDEX lv_index.
  ls_partner_com-partner_no         = lv_partner_no.

  INSERT ls_partner_com INTO TABLE gt_partner.

  ls_input_field-ref_guid     = iv_ref_guid.
  ls_input_field-ref_kind     = iv_ref_kind.
  ls_input_field-logical_key  = ls_partner_com-ref_partner_handle.
  ls_input_field-objectname   = gc_object_name-partner.
  INSERT ls_input_field  INTO TABLE  gt_input_fields.

  ls_partner_com-ref_guid           = iv_ref_guid.
  ls_partner_com-ref_kind           = iv_ref_kind.
  ls_partner_com-ref_partner_handle = 2.
  ls_partner_com-kind_of_entry      = 'C'.
  ls_partner_com-partner_fct        = '00000014'. "employee responsible
  ls_partner_com-no_type            = 'BP'.
  ls_partner_com-display_type       = 'BP'.
  ls_partner_com-partner_no         = lv_partner_no.

  INSERT ls_partner_com INTO TABLE gt_partner.

  ls_input_field-ref_guid     = iv_ref_guid.
  ls_input_field-ref_kind     = iv_ref_kind.
  ls_input_field-logical_key  = ls_partner_com-ref_partner_handle.
  ls_input_field-objectname   = gc_object_name-partner.
  INSERT ls_input_field  INTO TABLE  gt_input_fields.
ENDFORM.

FORM dates_create
  USING  iv_ref_guid TYPE crmt_object_guid
         iv_ref_kind TYPE crmt_object_kind.

  DATA   ls_date              TYPE  crmt_appointment_com.
  DATA   ls_input_field       TYPE  crmt_input_field.
  DATA   ls_input_field_names TYPE  crmt_input_field_names.

  DATA   lv_date              TYPE  sydatum.
  DATA   lv_index             TYPE  i.

  CLEAR ls_input_field.
  CLEAR ls_date.

  ls_date-ref_guid       = iv_ref_guid.
  ls_date-ref_kind       = iv_ref_kind.
  ls_date-appt_type      = 'SRV_CUST_BEG'.
  lv_index = go_prng_date->get_next( ).
  lv_date = gv_date_start + lv_index.

  ls_date-timezone_from = sy-zonlo.
  CONVERT DATE lv_date TIME '000000' INTO TIME STAMP ls_date-timestamp_from TIME ZONE ls_date-timezone_from.

  INSERT ls_date INTO TABLE gt_appointment.

  ls_input_field-ref_guid    = iv_ref_guid.
  ls_input_field-ref_kind    = iv_ref_kind.
  ls_input_field-objectname  = gc_object_name-appointment.
  ls_input_field-logical_key = ls_date-appt_type.

  ls_input_field_names-fieldname = 'APPT_TYPE'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'TIMESTAMP_FROM'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'TIMEZONE_FROM'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  INSERT ls_input_field  INTO TABLE  gt_input_fields.

  CLEAR: ls_input_field,ls_date.

  ls_date-ref_guid       = iv_ref_guid.
  ls_date-ref_kind       = iv_ref_kind.
  ls_date-appt_type      = 'SRV_CUST_END'.
  lv_date = lv_date + 3.

  ls_date-timezone_from = sy-zonlo.
  CONVERT DATE lv_date TIME '000000' INTO TIME STAMP ls_date-timestamp_from TIME ZONE ls_date-timezone_from.

  INSERT ls_date INTO TABLE gt_appointment.

  ls_input_field-ref_guid    = iv_ref_guid.
  ls_input_field-ref_kind    = iv_ref_kind.
  ls_input_field-objectname  = gc_object_name-appointment.
  ls_input_field-logical_key = ls_date-appt_type.

  ls_input_field_names-fieldname = 'APPT_TYPE'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'TIMESTAMP_FROM'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'TIMEZONE_FROM'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  INSERT ls_input_field  INTO TABLE  gt_input_fields.
ENDFORM.

FORM orderadm_i_create
  USING    iv_header_guid TYPE crmt_object_guid
  CHANGING cv_item_guid   TYPE crmt_object_guid RAISING cx_uuid_error.

  DATA: ls_orderadm_i        TYPE  crmt_orderadm_i_com,
        ls_input_field       TYPE  crmt_input_field,
        ls_input_field_names TYPE  crmt_input_field_names.

  ls_orderadm_i-guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).

  ls_orderadm_i-mode          = gc_mode-create.
  ls_orderadm_i-header        = iv_header_guid.
  DATA(lv_index) = go_prng_material->get_next( ).
  READ TABLE gt_matnr INTO DATA(lv_matnr) INDEX lv_index.
  ls_orderadm_i-ordered_prod  = lv_matnr.

  INSERT ls_orderadm_i INTO TABLE gt_orderadm_i.

  ls_input_field-ref_guid  = ls_orderadm_i-guid.
  ls_input_field-ref_kind  = gc_object_kind-orderadm_i.
  ls_input_field-objectname  = gc_object_name-orderadm_i.
  ls_input_field_names-fieldname = 'ORDERED_PROD'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  INSERT ls_input_field  INTO TABLE  gt_input_fields.

  cv_item_guid = ls_orderadm_i-guid.
ENDFORM.

FORM schedlin_create USING  iv_item_guid TYPE crmt_object_guid.

  DATA:ls_schedlin_i_com    TYPE  crmt_schedlin_i_com,
       ls_schedlin_com      TYPE  crmt_schedlin_extd,
       ls_input_field       TYPE  crmt_input_field,
       ls_input_field_names TYPE  crmt_input_field_names.

  ls_schedlin_i_com-mode          = gc_mode-create.
  ls_schedlin_i_com-ref_guid      = iv_item_guid.
  ls_schedlin_com-logical_key      = ls_schedlin_i_com-ref_guid.
  DATA(lv_quantity) = go_prng_quantity->get_next( ).

  ls_schedlin_com-quantity         = lv_quantity.
  INSERT ls_schedlin_com INTO TABLE ls_schedlin_i_com-schedlines.

  INSERT ls_schedlin_i_com INTO TABLE gt_schedlin_i_com.

  ls_input_field_names-fieldname = 'QUANTITY'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.

  ls_input_field-ref_guid    = iv_item_guid.
  ls_input_field-ref_kind    = gc_object_ref_kind-orderadm_i.
  ls_input_field-objectname  = gc_object_name-schedlin.
  ls_input_field-logical_key = ls_schedlin_com-logical_key.
  INSERT ls_input_field  INTO TABLE  gt_input_fields.
ENDFORM.

FORM create_orders .
  CALL FUNCTION 'CRM_DIALOG_SET_NO_DIALOG'.

  CALL FUNCTION 'CRM_ORDER_MAINTAIN'
    EXPORTING
      it_schedlin_i   = gt_schedlin_i_com
      it_partner      = gt_partner
      it_orgman       = gt_orgman
      it_appointment  = gt_appointment
      it_product_i    = gt_product_i
      it_status       = gt_status
    CHANGING
      ct_orderadm_h   = gt_orderadm_h
      ct_orderadm_i   = gt_orderadm_i
      ct_input_fields = gt_input_fields
      cv_log_handle   = gv_log_handle
    EXCEPTIONS
      OTHERS          = 1.

  CALL FUNCTION 'CRM_DIALOG_SET_WITH_DIALOG'.

  REFRESH gt_obj_guids.

  LOOP AT gt_orderadm_h INTO DATA(gs_orderadm_h).
    INSERT gs_orderadm_h-guid INTO TABLE gt_obj_guids.
  ENDLOOP.
ENDFORM.

FORM save_orders .
  REFRESH gt_saved_objects.
  IF gt_obj_guids IS NOT INITIAL.
    CALL FUNCTION 'CRM_ORDER_SAVE'
      EXPORTING
        it_objects_to_save = gt_obj_guids
        "iv_update_task_local = abap_true
      IMPORTING
        et_saved_objects   = gt_saved_objects.
    COMMIT WORK.
  ENDIF.

  WRITE: / 'Orders have been created:', lines( gt_saved_objects ).
  SKIP.
ENDFORM.

FORM orgman_create USING  iv_ref_guid TYPE crmt_object_guid iv_ref_kind TYPE crmt_object_kind.

  DATA:ls_orgman_com        TYPE  crmt_orgman_com,
       ls_input_field       TYPE  crmt_input_field,
       ls_input_field_names TYPE  crmt_input_field_names.

  DATA           ls_tvta  TYPE tvta.
  DATA           ls_org_assignment TYPE ty_org_assignment.
  DATA           ls_service_org TYPE hrobject.

  DATA(lv_index_1) = go_prng_sales_area->get_next( ).
  READ TABLE gt_tvta INTO ls_tvta INDEX lv_index_1.
  READ TABLE gt_org_assignment INTO ls_org_assignment WITH KEY s4_sales_org = ls_tvta-vkorg.

  DATA(lv_index_2) = go_prng_service_org->get_next( ).
  READ TABLE gt_service_orgs INTO ls_service_org INDEX lv_index_2.

  ls_orgman_com-ref_guid          = iv_ref_guid.
  ls_orgman_com-ref_kind          = iv_ref_kind.

  ls_orgman_com-sales_org         = ls_org_assignment-crm_sales_org.
  ls_orgman_com-dis_channel       = ls_tvta-vtweg.
  ls_orgman_com-division          = ls_tvta-spart.
  ls_orgman_com-service_org       = ls_service_org+2.

  ls_orgman_com-sales_org_ori     = 'C'.
  ls_orgman_com-dis_channel_ori   = 'C'.
  ls_orgman_com-division_ori      = 'C'.
  ls_orgman_com-service_org_ori   = 'C'.

  INSERT ls_orgman_com INTO TABLE gt_orgman.

  ls_input_field-ref_guid    = iv_ref_guid.
  ls_input_field-ref_kind    = iv_ref_kind.
  ls_input_field-objectname  = gc_object_name-orgman.

  ls_input_field_names-fieldname = 'SALES_ORG'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'DIS_CHANNEL'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'DIVISION'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'SERVICE_ORG'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'SALES_ORG_ORI'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'DIS_CHANNEL_ORI'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'DIVISION_ORI'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  ls_input_field_names-fieldname = 'SERVICE_ORG_ORI'.
  INSERT ls_input_field_names INTO TABLE ls_input_field-field_names.
  INSERT ls_input_field  INTO TABLE  gt_input_fields.
ENDFORM.