CLASS zcl_crm_order_statistic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_item_info,
        item_number TYPE int4,
        occurance   TYPE int4,
        detail      TYPE crmt_object_guid_tab,
      END OF ty_item_info .
    TYPES:
      tt_item_info TYPE STANDARD TABLE OF ty_item_info WITH KEY item_number .
    TYPES:
      BEGIN OF ty_item_without_detail,
        occurance   TYPE int4,
        item_number TYPE string,
      END OF ty_item_without_detail .
    TYPES:
      tt_item_without_detail TYPE TABLE OF ty_item_without_detail WITH KEY occurance .
    TYPES:
      BEGIN OF ty_saleorg_info,
        occurance     TYPE int4,
        sale_org_id   TYPE crmt_sales_org,
        sale_org_text TYPE short_d,
      END OF ty_saleorg_info .
    TYPES:
      tt_saleorg_info TYPE STANDARD TABLE OF ty_saleorg_info WITH KEY occurance .
    TYPES:
      BEGIN OF ty_serviceorg_info,
        occurance        TYPE int4,
        service_org_id   TYPE crmt_sales_org,
        service_org_text TYPE short_d,
      END OF ty_serviceorg_info .
    TYPES:
      tt_serviceorg_info TYPE STANDARD TABLE OF ty_serviceorg_info WITH KEY occurance .
    TYPES:
      BEGIN OF ty_result,
        item    TYPE tt_item_without_detail,
        sales   TYPE tt_saleorg_info,
        service TYPE tt_serviceorg_info,
      END OF ty_result .

    CLASS-METHODS count
      RETURNING
        VALUE(rs_result) TYPE ty_result .
    CLASS-METHODS get_item_json
      RETURNING
        VALUE(rv_json) TYPE string .
    CLASS-METHODS get_sales_json
      RETURNING
        VALUE(rv_json) TYPE string .
    CLASS-METHODS get_service_json
      RETURNING
        VALUE(rv_json) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mt_item_result TYPE tt_item_info .
    CLASS-DATA mt_saleorg_info TYPE tt_saleorg_info .
    CLASS-DATA mt_serviceorg_info TYPE tt_serviceorg_info .

    CLASS-METHODS get_org_text
      IMPORTING
        !iv_org_id     TYPE crmt_sales_org
      RETURNING
        VALUE(rv_text) TYPE short_d .
    CLASS-METHODS count_item .
    CLASS-METHODS count_org .
    CLASS-METHODS remove_unneeded_char
      CHANGING
        !cv_json TYPE string .
    CLASS-METHODS abap_2_json
      IMPORTING
        !it_data       TYPE ANY TABLE
      RETURNING
        VALUE(rv_json) TYPE string .
ENDCLASS.



CLASS ZCL_CRM_ORDER_STATISTIC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_ORDER_STATISTIC=>ABAP_2_JSON
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DATA                        TYPE        ANY TABLE
* | [<-()] RV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD abap_2_json.
    DATA(writer) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_normalizing ).
    writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_no_empty ).
    CALL TRANSFORMATION id SOURCE itab = it_data
                           RESULT XML writer.
    DATA(json) = writer->get_output( ).
    CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
      EXPORTING
        im_xstring = json
      IMPORTING
        ex_string  = rv_json.

    remove_unneeded_char( CHANGING cv_json = rv_json ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_ORDER_STATISTIC=>COUNT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_RESULT                      TYPE        TY_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD count.
    CLEAR: mt_item_result, mt_saleorg_info, mt_serviceorg_info.
    count_item( ).
    count_org( ).
    rs_result = VALUE #( item = CORRESPONDING #( mt_item_result ) sales = mt_saleorg_info service = mt_serviceorg_info ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_ORDER_STATISTIC=>COUNT_ITEM
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD count_item.
    SELECT * INTO TABLE @DATA(lt_header) FROM crmd_orderadm_h." UP TO 1000 ROWS.

    SELECT * INTO TABLE @DATA(lt_item) FROM crmd_orderadm_i."  UP TO 1000 ROWS.

    LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<item>) GROUP BY ( key1 = <item>-header
      size = GROUP SIZE ) INTO DATA(group).
      READ TABLE mt_item_result ASSIGNING FIELD-SYMBOL(<result>) WITH KEY item_number = group-size.
      IF sy-subrc = 0.
        ADD 1 TO <result>-occurance.
        INSERT group-key1 INTO TABLE <result>-detail.
      ELSE.
        APPEND INITIAL LINE TO mt_item_result ASSIGNING FIELD-SYMBOL(<new>).
        <new>-item_number = group-size.
        <new>-occurance = 1.
        INSERT group-key1 INTO TABLE <new>-detail.
      ENDIF.
    ENDLOOP.

    SORT mt_item_result BY occurance DESCENDING.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_ORDER_STATISTIC=>COUNT_ORG
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD count_org.
    DATA: lt_link TYPE TABLE OF crmd_link,
          lt_org  TYPE TABLE OF crmd_orgman.

    SELECT * INTO TABLE lt_link FROM crmd_link WHERE objtype_set = '21'.

    SELECT * INTO TABLE lt_org FROM crmd_orgman FOR ALL ENTRIES IN lt_link
        WHERE guid = lt_link-guid_set.

    DELETE lt_org WHERE sales_org+0(1) <> 'O'.
    LOOP AT lt_org ASSIGNING FIELD-SYMBOL(<item>) GROUP BY ( key1 = <item>-sales_org
     size = GROUP SIZE ) INTO DATA(group).

      APPEND INITIAL LINE TO mt_saleorg_info ASSIGNING FIELD-SYMBOL(<new>).
      <new>-occurance = group-size.
      <new>-sale_org_id = group-key1.
      <new>-sale_org_text = get_org_text( <new>-sale_org_id ).
    ENDLOOP.

    SORT mt_saleorg_info BY occurance DESCENDING.

    LOOP AT lt_org ASSIGNING FIELD-SYMBOL(<item2>) GROUP BY ( key1 = <item2>-service_org
      size = GROUP SIZE ) INTO DATA(group2).

      APPEND INITIAL LINE TO mt_serviceorg_info ASSIGNING FIELD-SYMBOL(<new2>).
      <new2>-occurance = group2-size.
      <new2>-service_org_id = group2-key1.
      <new2>-service_org_text = get_org_text( <new2>-service_org_id ).
    ENDLOOP.

    SORT mt_serviceorg_info BY occurance DESCENDING.
    DELETE mt_serviceorg_info WHERE service_org_id IS INITIAL.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_ORDER_STATISTIC=>GET_ITEM_JSON
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_item_json.

    DATA(lt_temp) = CORRESPONDING tt_item_info( mt_item_result ).
    IF lines( lt_temp ) > 10.
      DELETE lt_temp FROM 11.
    ENDIF.

    DATA(lt_result) = CORRESPONDING tt_item_without_detail( lt_temp ).
    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<result>).
        <result>-item_number = |Item number:{ <result>-item_number }|.
    ENDLOOP.
    rv_json = abap_2_json( CORRESPONDING tt_item_without_detail( lt_result ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_ORDER_STATISTIC=>GET_ORG_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ORG_ID                      TYPE        CRMT_SALES_ORG
* | [<-()] RV_TEXT                        TYPE        SHORT_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_org_text.
    CHECK iv_org_id IS NOT INITIAL.
    DATA(lv_len) = strlen( iv_org_id ) - 2.
    DATA(lv_org_id) = CONV hrobjid( iv_org_id+2(lv_len) ).

    SELECT * INTO TABLE @DATA(lt_org) FROM hrp1000 WHERE objid = @lv_org_id.

    CHECK lt_org IS NOT INITIAL.

    SORT lt_org BY endda DESCENDING.

    rv_text = lt_org[ 1 ]-short.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_ORDER_STATISTIC=>GET_SALES_JSON
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_sales_json.

    DATA(lt_temp) = CORRESPONDING tt_saleorg_info( mt_saleorg_info ).

    IF lines( lt_temp ) > 10.
       DELETE lt_temp FROM 11.
    ENDIF.
    rv_json = abap_2_json( CORRESPONDING tt_saleorg_info( lt_temp ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_ORDER_STATISTIC=>GET_SERVICE_JSON
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_service_json.

    rv_json = abap_2_json( CORRESPONDING tt_serviceorg_info( mt_serviceorg_info ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_ORDER_STATISTIC=>REMOVE_UNNEEDED_CHAR
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD remove_unneeded_char.

    FIND REGEX '^\{(.*)\}$' IN cv_json SUBMATCHES DATA(ma).
    CHECK sy-subrc = 0.
    REPLACE '"ITAB":' IN ma WITH space.
    cv_json = ma.
  ENDMETHOD.
ENDCLASS.