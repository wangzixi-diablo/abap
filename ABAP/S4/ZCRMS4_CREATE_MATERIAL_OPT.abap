
REPORT zcrms4_create_materials_loop.

DATA                ls_clientdata          TYPE bapie1mara.
DATA                ls_clientdatax         TYPE bapie1marax.
DATA                ls_headdata            TYPE bapie1matheader.
DATA                ls_materialdescription TYPE bapie1makt.
DATA                ls_return              TYPE bapiret2.
DATA                ls_salesdata           TYPE bapie1mvke.
DATA                ls_salesdatax          TYPE bapie1mvkex.
DATA                ls_t001                TYPE t001.
DATA                ls_t134                TYPE t134.
DATA                ls_taxclassifications  TYPE bapie1mlan.
DATA                ls_tskd                TYPE tskd.
DATA                ls_tstl                TYPE tstl.
DATA                ls_tvko                TYPE tvko.
DATA                ls_tvkov               TYPE tvkov.
DATA                ls_tvta                TYPE tvta.
DATA                lt_clientdata          TYPE STANDARD TABLE OF bapie1mara.
DATA                lt_clientdatax         TYPE STANDARD TABLE OF bapie1marax.
DATA                lt_headdata            TYPE STANDARD TABLE OF bapie1matheader.
DATA                lt_materialdescription TYPE STANDARD TABLE OF bapie1makt.
DATA                lt_return              TYPE STANDARD TABLE OF bapie1ret2.
DATA                lt_salesdata           TYPE STANDARD TABLE OF bapie1mvke.
DATA                lt_salesdatax          TYPE STANDARD TABLE OF bapie1mvkex.
DATA                lt_t001                TYPE STANDARD TABLE OF t001.
DATA                lt_taxclassifications  TYPE STANDARD TABLE OF bapie1mlan.
DATA                lt_tskd                TYPE STANDARD TABLE OF tskd.
DATA                lt_tstl                TYPE STANDARD TABLE OF tstl.
DATA                lt_tvko                TYPE STANDARD TABLE OF tvko.
DATA                lt_tvkov               TYPE STANDARD TABLE OF tvkov.
DATA                lt_tvta                TYPE STANDARD TABLE OF tvta.
DATA                lt_vkorg               TYPE STANDARD TABLE OF vkorg.
DATA                lo_prng_ttgr           TYPE REF TO cl_abap_random_int.
DATA                lv_char10              TYPE char10.
DATA                lv_index_n             TYPE n LENGTH 10.
DATA                lv_matl_grp_1          TYPE mvgr1.
DATA                lv_service             TYPE abap_bool.
DATA                lv_vkorg               TYPE vkorg.

PARAMETERS          p_matnr                TYPE string.
PARAMETERS          p_mattyp               TYPE mtart.
PARAMETERS          p_text                 TYPE maktx.
PARAMETERS          p_number               TYPE i.
PARAMETERS          p_test                 TYPE testrun AS CHECKBOX.

SELECT * FROM tvkov INTO TABLE lt_tvkov
  ORDER BY PRIMARY KEY.
LOOP AT lt_tvkov INTO ls_tvkov.
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

SELECT SINGLE * FROM t134
  INTO ls_t134
  WHERE mtart = p_mattyp.
IF sy-subrc NE 0.
  MESSAGE i398(00) WITH 'Material type ' p_mattyp ' does not exist' space.
  EXIT.
ENDIF.

IF ls_t134-prod_type_code EQ '2'.
  lv_service = abap_true.
ELSE.
  lv_service = abap_false.
ENDIF.

DO p_number TIMES.

  lv_index_n = sy-index.
  lv_char10 = sy-index.

  CLEAR ls_headdata.
  ls_headdata-function   = 'INS'.
  CONCATENATE p_matnr lv_index_n INTO ls_headdata-material.
  CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
    EXPORTING
      iv_tabname = 'T137'
    IMPORTING
      ev_value   = ls_headdata-ind_sector.
  ls_headdata-matl_type  = p_mattyp.
  ls_headdata-basic_view = 'X'.
  ls_headdata-sales_view = 'X'.
  INSERT ls_headdata INTO TABLE lt_headdata.

  CLEAR ls_clientdata.
  ls_clientdata-function = 'INS'.
  ls_clientdata-material = ls_headdata-material.
  ls_clientdata-base_uom = 'STD'.

  CALL METHOD cl_crms4_cust_home=>get_random_customizing_value
    EXPORTING
      iv_tabname = 'T179'
    IMPORTING
      ev_value   = ls_clientdata-prod_hier.
  IF lv_service EQ abap_false.
    ls_clientdata-item_cat = 'NORM'.
  ELSE.
    ls_clientdata-item_cat = 'SRVP'.
  ENDIF.
  INSERT ls_clientdata INTO TABLE lt_clientdata.

  CLEAR ls_materialdescription.
  ls_materialdescription-function = 'INS'.
  ls_materialdescription-material = ls_headdata-material.
  ls_materialdescription-langu    = sy-langu.
  CONCATENATE p_text lv_char10 INTO ls_materialdescription-matl_desc.
  INSERT ls_materialdescription INTO TABLE lt_materialdescription.

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
      CLEAR ls_taxclassifications.
      ls_taxclassifications-function   = 'INS'.
      ls_taxclassifications-material   = ls_headdata-material.
      ls_taxclassifications-depcountry = ls_tstl-talnd.
      ls_taxclassifications-tax_type_1 = ls_tstl-tatyp.
      READ TABLE lt_tskd
      INTO ls_tskd
      WITH KEY tatyp = ls_tstl-tatyp.
      CHECK sy-subrc EQ 0.
      ls_taxclassifications-taxclass_1 = ls_tskd-taxkd.
      INSERT ls_taxclassifications INTO TABLE lt_taxclassifications.
    ENDLOOP.
  ENDLOOP.

  LOOP AT lt_tvkov INTO ls_tvkov.
    IF ls_tvkov-vtwku NE ls_tvkov-vtweg.
      CONTINUE.
    ENDIF.
    CLEAR ls_salesdata.
    ls_salesdata-function   = 'INS'.
    ls_salesdata-material   = ls_headdata-material.
    ls_salesdata-sales_org  = ls_tvkov-vkorg.
    ls_salesdata-distr_chan = ls_tvkov-vtweg.
    ls_salesdata-item_cat   = ls_clientdata-item_cat.
    INSERT ls_salesdata INTO TABLE lt_salesdata.
    CLEAR ls_salesdatax.
    ls_salesdatax-function   = 'INS'.
    ls_salesdatax-material   = ls_headdata-material.
    ls_salesdatax-sales_org  = ls_tvkov-vkorg.
    ls_salesdatax-distr_chan = ls_tvkov-vtweg.
    ls_salesdatax-item_cat   = 'X'.
    INSERT ls_salesdatax INTO TABLE lt_salesdatax.
  ENDLOOP.

* X flags
  CLEAR ls_clientdatax.
  ls_clientdatax-function   = 'INS'.
  ls_clientdatax-material   = ls_headdata-material.
  ls_clientdatax-base_uom   = 'X'.
  ls_clientdatax-prod_hier  = 'X'.
  ls_clientdatax-item_cat   = 'X'.
  INSERT ls_clientdatax INTO TABLE lt_clientdatax.
ENDDO.

CALL FUNCTION 'BAPI_MATERIAL_SAVEREPLICA'
  EXPORTING
    noappllog           = ' '
    nochangedoc         = ' '
    testrun             = p_test
    inpfldcheck         = 'W'
  IMPORTING
    return              = ls_return
  TABLES
    headdata            = lt_headdata
    clientdata          = lt_clientdata
    clientdatax         = lt_clientdatax
    salesdata           = lt_salesdata
    salesdatax          = lt_salesdatax
    materialdescription = lt_materialdescription
    taxclassifications  = lt_taxclassifications
    returnmessages      = lt_return.

LOOP AT lt_return TRANSPORTING NO FIELDS WHERE type = 'E' OR    type = 'A' OR    type = 'X'.
  EXIT.
ENDLOOP.
IF sy-subrc NE 0 AND p_test IS INITIAL.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  WRITE:/ 'Product Created successfully.' COLOR COL_POSITIVE.
ELSE.
  WRITE:/ 'Product creation failed' COLOR COL_NEGATIVE.
ENDIF.