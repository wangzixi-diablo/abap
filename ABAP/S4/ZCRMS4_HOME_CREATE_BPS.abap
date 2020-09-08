REPORT crms4_home_create_bps_opt.

DATA                go_prng_address        TYPE REF TO cl_abap_random_int.
DATA                ls_address_snwd        TYPE snwd_bp_crm.
DATA                ls_bapiret2            TYPE bapiret2.
DATA                ls_bp_address          TYPE bus_ei_bupa_address.
DATA                ls_bp_data             TYPE cvis_ei_extern.
DATA                ls_bp_roles            TYPE bus_ei_bupa_roles.
DATA                ls_bp_sales            TYPE cmds_ei_sales.
DATA                ls_bp_sales_functions  TYPE cmds_ei_functions.
DATA                ls_bp_tax_ind          TYPE cmds_ei_tax_ind.
DATA                ls_t001                TYPE t001.
DATA                ls_tpaer               TYPE tpaer.
DATA                ls_tskd                TYPE tskd.
DATA                ls_tstl                TYPE tstl.
DATA                ls_tvko                TYPE tvko.
DATA                ls_tvkov               TYPE tvkov.
DATA                ls_tvta                TYPE tvta.
DATA                lt_addr_hook           TYPE STANDARD TABLE OF char10.
DATA                lt_bapiret2            TYPE bapirettab.
DATA                lt_bp_data             TYPE cvis_ei_extern_t.
DATA                lt_return              TYPE bapiretm.
DATA                lt_return_map          TYPE mdg_bs_bp_msgmap_t.
DATA                lt_t001                TYPE STANDARD TABLE OF t001.
DATA                lt_tpaer               TYPE STANDARD TABLE OF tpaer.
DATA                lt_tskd                TYPE STANDARD TABLE OF tskd.
DATA                lt_tstl                TYPE STANDARD TABLE OF tstl.
DATA                lt_tvko                TYPE STANDARD TABLE OF tvko.
DATA                lt_vkorg               TYPE STANDARD TABLE OF vkorg.
DATA                lv_addr_hook           TYPE char10.
DATA                lv_error               TYPE abap_bool.
DATA                lv_no_of_addresses     TYPE i.
DATA                lv_pargr               TYPE pargr.
DATA                gt_tvkov               TYPE STANDARD TABLE OF tvkov.
DATA                gt_tvta                TYPE STANDARD TABLE OF tvta.
DATA                lv_char10              TYPE char10.
DATA                lv_index               TYPE i.
DATA                lv_index_n             TYPE n LENGTH 6.
DATA                lv_vkorg               TYPE vkorg.

PARAMETERS          p_prefix               TYPE char4.
PARAMETERS          p_number               TYPE i.
PARAMETERS          p_name                 TYPE text30.
PARAMETERS          p_group                TYPE but000-BU_GROUP.
PARAMETERS          p_custgp               TYPE t077d-KTOKD.
PARAMETERS          p_ext                  TYPE boolean AS CHECKBOX DEFAULT abap_true.

SELECT * FROM tvta INTO TABLE gt_tvta
  ORDER BY PRIMARY KEY.

SELECT * FROM tvkov INTO TABLE gt_tvkov
  ORDER BY PRIMARY KEY.
LOOP AT gt_tvkov INTO ls_tvkov.
  INSERT ls_tvkov-vkorg INTO TABLE lt_vkorg.
ENDLOOP.
SORT lt_vkorg.
DELETE ADJACENT DUPLICATES FROM lt_vkorg.
IF lt_vkorg IS NOT INITIAL.
  SELECT * FROM tvko INTO TABLE lt_tvko
    FOR ALL ENTRIES IN lt_vkorg
    WHERE vkorg = lt_vkorg-table_line
    ORDER BY PRIMARY KEY.
ENDIF.
IF lt_tvko IS NOT INITIAL.
  SELECT DISTINCT * FROM t001 INTO TABLE lt_t001
    FOR ALL ENTRIES IN lt_tvko
    WHERE bukrs = lt_tvko-bukrs
    ORDER BY PRIMARY KEY.
ENDIF.

SELECT bp_id FROM snwd_bp_crm
  INTO TABLE lt_addr_hook
  WHERE country = 'DE'.
DESCRIBE TABLE lt_addr_hook LINES lv_no_of_addresses.
go_prng_address = cl_abap_random_int=>create( min = 1 max = lv_no_of_addresses ).

DO p_number TIMES.
  lv_index_n = sy-index.
  lv_char10 = sy-index.

  CLEAR ls_bp_data.
  ls_bp_data-partner-header-object_task                                       = 'I'.      "Insert
  IF p_ext = abap_true.
    ls_bp_data-partner-header-object_instance-bpartner                          = p_prefix && lv_index_n.
  ENDIF.
  TRY.
      CALL METHOD cl_system_uuid=>if_system_uuid_static~create_uuid_c32
        RECEIVING
          uuid = ls_bp_data-partner-header-object_instance-bpartnerguid.
    CATCH cx_uuid_error .
  ENDTRY.
  ls_bp_data-partner-central_data-common-data-bp_control-category             = '2'.      "Organization
  ls_bp_data-partner-central_data-common-data-bp_control-grouping             = p_group. "0002 or BP01
  ls_bp_data-partner-central_data-common-data-bp_centraldata-searchterm1      = 'CS'.
  ls_bp_data-partner-central_data-common-data-bp_centraldata-partnertype      = '0001'.
  ls_bp_data-partner-central_data-common-data-bp_centraldata-title_key        = '0003'.   "Company.
  ls_bp_data-partner-central_data-common-data-bp_organization-name1           = p_name && lv_char10.
  ls_bp_data-partner-central_data-common-data-bp_organization-industrysector  = '0001'.
  ls_bp_data-partner-central_data-common-datax-bp_centraldata-searchterm1     = 'X'.
  ls_bp_data-partner-central_data-common-datax-bp_centraldata-partnertype     = 'X'.
  ls_bp_data-partner-central_data-common-datax-bp_centraldata-title_key       = 'X'.
  ls_bp_data-partner-central_data-common-datax-bp_organization-name1          = 'X'.
  ls_bp_data-partner-central_data-common-datax-bp_organization-industrysector = 'X'.

* Roles
  ls_bp_data-partner-central_data-role-current_state = 'X'.
  CLEAR ls_bp_roles.
  ls_bp_roles-task = 'I'.
  ls_bp_roles-data_key = 'CRM000'. "Sold-to party
  ls_bp_roles-data-rolecategory = 'CRM000'.
  ls_bp_roles-data-valid_from = sy-datum.
  ls_bp_roles-data-valid_to = '99991231'.
  ls_bp_roles-datax-valid_from = 'X'.
  ls_bp_roles-datax-valid_to = 'X'.
  ls_bp_roles-currently_valid = 'X'.
  INSERT ls_bp_roles INTO TABLE ls_bp_data-partner-central_data-role-roles.
  CLEAR ls_bp_roles.
  ls_bp_roles-task = 'I'.
  ls_bp_roles-data_key = 'FLCU00'. "FI Customer
  ls_bp_roles-data-rolecategory = 'FLCU00'.
  ls_bp_roles-data-valid_from = sy-datum.
  ls_bp_roles-data-valid_to = '99991231'.
  ls_bp_roles-datax-valid_from = 'X'.
  ls_bp_roles-datax-valid_to = 'X'.
  ls_bp_roles-currently_valid = 'X'.
  INSERT ls_bp_roles INTO TABLE ls_bp_data-partner-central_data-role-roles.

* Address
  ls_bp_data-partner-central_data-address-current_state = 'X'.
  lv_index = go_prng_address->get_next( ).
  READ TABLE lt_addr_hook
    INTO lv_addr_hook
    INDEX lv_index.
  SELECT SINGLE * FROM snwd_bp_crm
    INTO ls_address_snwd
    WHERE bp_id = lv_addr_hook.
  CLEAR ls_bp_address.
  ls_bp_address-task = 'I'.
  ls_bp_address-data-postal-data-city         = ls_address_snwd-city.
  ls_bp_address-data-postal-data-postl_cod1   = ls_address_snwd-zip.
  ls_bp_address-data-postal-data-street       = ls_address_snwd-street(60).
  ls_bp_address-data-postal-data-house_no     = ls_address_snwd-street_no.
  ls_bp_address-data-postal-data-country      = ls_address_snwd-country.
  ls_bp_address-data-postal-data-langu        = 'D'.
  ls_bp_address-data-postal-datax-city        = 'X'.
  ls_bp_address-data-postal-datax-postl_cod1  = 'X'.
  ls_bp_address-data-postal-datax-street      = 'X'.
  ls_bp_address-data-postal-datax-house_no    = 'X'.
  ls_bp_address-data-postal-datax-country     = 'X'.
  ls_bp_address-data-postal-datax-langu       = 'X'.
  ls_bp_address-currently_valid = 'X'.
  INSERT ls_bp_address INTO TABLE ls_bp_data-partner-central_data-address-addresses.

* Customer data
  ls_bp_data-customer-header-object_task = 'I'.
  ls_bp_data-customer-header-object_instance = ls_bp_data-partner-header-object_instance-bpartner.
  ls_bp_data-customer-central_data-central-data-ktokd = p_custgp. " KUNA or CUST
  ls_bp_data-customer-central_data-central-data-niels = '01'.
  ls_bp_data-customer-central_data-central-datax-ktokd = 'X'.
  ls_bp_data-customer-central_data-central-datax-niels = 'X'.

* Tax indicators
  ls_bp_data-customer-central_data-tax_ind-current_state = 'X'.
  LOOP AT lt_t001 INTO ls_t001.
    CLEAR lt_tstl.
    SELECT * FROM tstl INTO TABLE lt_tstl
      WHERE talnd = ls_t001-land1.
    CHECK lt_tstl IS NOT INITIAL.
    CLEAR lt_tskd.
    SELECT * FROM tskd INTO TABLE lt_tskd
      FOR ALL ENTRIES IN lt_tstl
      WHERE tatyp = lt_tstl-tatyp.
    LOOP AT lt_tstl INTO ls_tstl.
      CLEAR ls_bp_tax_ind.
      ls_bp_tax_ind-task           = 'I'.
      ls_bp_tax_ind-data_key-aland = ls_tstl-talnd.
      ls_bp_tax_ind-data_key-tatyp = ls_tstl-tatyp.
      READ TABLE lt_tskd
        INTO ls_tskd
        WITH KEY tatyp = ls_tstl-tatyp.
      CHECK sy-subrc EQ 0.
      ls_bp_tax_ind-data-taxkd     = ls_tskd-taxkd.
      ls_bp_tax_ind-datax-taxkd    = 'X '.
      INSERT ls_bp_tax_ind INTO TABLE ls_bp_data-customer-central_data-tax_ind-tax_ind.
    ENDLOOP.
  ENDLOOP.

  ls_bp_data-customer-sales_data-current_state = 'X'.

  CLEAR lv_pargr.
  CLEAR lt_tpaer.
  SELECT SINGLE pargr FROM tkupa
    INTO lv_pargr
    WHERE ktokd = ls_bp_data-customer-central_data-central-data-ktokd.
  IF lv_pargr IS NOT INITIAL.
    SELECT * FROM tpaer
      INTO TABLE lt_tpaer
      WHERE pargr = lv_pargr.
  ENDIF.

* Sales data
  LOOP AT gt_tvta INTO ls_tvta.
* Data cannot be maintained if a different reference distribution
* channel has been defined
    SELECT SINGLE * FROM tvkov
      INTO ls_tvkov
      WHERE vkorg = ls_tvta-vkorg
      AND   vtweg = ls_tvta-vtweg.
    IF sy-subrc EQ 0 AND
      ls_tvkov-vtwku NE ls_tvta-vtweg.
      CONTINUE.
    ENDIF.
* Now maintain the data
    CLEAR ls_bp_sales.
    ls_bp_sales-task = 'I'.
    ls_bp_sales-data_key-vkorg = ls_tvta-vkorg.
    ls_bp_sales-data_key-vtweg = ls_tvta-vtweg.
    ls_bp_sales-data_key-spart = ls_tvta-spart.
    CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
      EXPORTING
        iv_tabname = 'T151'
      IMPORTING
        ev_value   = ls_bp_sales-data-kdgrp.
*                 inco1
*                 inco2
* Get random value for the delivery priority
    CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
      EXPORTING
        iv_tabname = 'TPRIO'
      IMPORTING
        ev_value   = ls_bp_sales-data-lprio.
    CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
      EXPORTING
        iv_tabname = 'TVSB'
      IMPORTING
        ev_value   = ls_bp_sales-data-vsbed.
    ls_bp_sales-data-waers = 'EUR'.
*                 zterm
    CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
      EXPORTING
        iv_tabname = 'TVV1'
      IMPORTING
        ev_value   = ls_bp_sales-data-kvgr1.
    CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
      EXPORTING
        iv_tabname = 'TVKD'
      IMPORTING
        ev_value   = ls_bp_sales-data-kalks.
    CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
      EXPORTING
        iv_tabname = 'T171'
      IMPORTING
        ev_value   = ls_bp_sales-data-bzirk.
    CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
      EXPORTING
        iv_tabname = 'T188'
      IMPORTING
        ev_value   = ls_bp_sales-data-konda.
    ls_bp_sales-datax-kdgrp = 'X'.
    ls_bp_sales-datax-lprio = 'X'.
    ls_bp_sales-datax-vsbed = 'X'.
    ls_bp_sales-datax-waers = 'X'.
    ls_bp_sales-datax-kvgr1 = 'X'.
    ls_bp_sales-datax-kalks = 'X'.
    ls_bp_sales-datax-bzirk = 'X'.
    ls_bp_sales-datax-konda = 'X'.

    ls_bp_sales-functions-current_state = 'X'.
    LOOP AT lt_tpaer INTO ls_tpaer
      WHERE papfl = 'X'.
      CLEAR ls_bp_sales_functions.
      ls_bp_sales_functions-task = 'I'.
      ls_bp_sales_functions-data_key-parvw = ls_tpaer-parvw.
      ls_bp_sales_functions-data_key-parza = 1.
      ls_bp_sales_functions-data-partner   = ls_bp_data-customer-header-object_instance.
      ls_bp_sales_functions-datax-partner  = 'X'.
      INSERT ls_bp_sales_functions INTO TABLE ls_bp_sales-functions-functions.
    ENDLOOP.

    INSERT ls_bp_sales INTO TABLE ls_bp_data-customer-sales_data-sales.

  ENDLOOP.

  CLEAR lt_bapiret2.
  CALL METHOD cl_md_bp_maintain=>validate_single_current_state
    EXPORTING
      i_data    = ls_bp_data
    IMPORTING
      et_return = lt_bapiret2.
  LOOP AT lt_bapiret2 INTO ls_bapiret2
    WHERE type EQ 'E'
       OR type EQ 'A'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    WRITE: / 'Business partner ', ls_bp_data-partner-header-object_instance-bpartner,
             ' has error messages:'.
    LOOP AT lt_bapiret2 INTO ls_bapiret2
      WHERE type EQ 'E'
         OR type EQ 'A'.
      WRITE: / ls_bapiret2-message.
    ENDLOOP.
    CONTINUE.
  ENDIF.

  INSERT ls_bp_data INTO TABLE lt_bp_data.

ENDDO.

CALL METHOD cl_md_bp_maintain=>maintain
  EXPORTING
    i_data   = lt_bp_data
  IMPORTING
    e_return = lt_return.

LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
  LOOP AT <ls_return>-object_msg
    TRANSPORTING NO FIELDS
    WHERE type = 'E'
       OR type = 'A'
       OR type = 'X'.
    lv_error = abap_true.
    EXIT.
  ENDLOOP.
ENDLOOP.

If lv_error = abap_true.
   WRITE:/ 'BP creation failed' COLOR COL_NEGATIVE.
   RETURN.
ENDIF.

CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_false.

CLEAR lt_return.
CALL METHOD cl_md_bp_maintain=>get_ppo_messages
  IMPORTING
    e_return = lt_return.

LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<msg>).
   LOOP AT <msg>-object_msg ASSIGNING FIELD-SYMBOL(<text>).
      WRITE:/ <text>-message.
   ENDLOOP.
ENDLOOP.

WRITE:/ 'BP created successfully' COLOR COL_POSITIVE.