class ZCL_ADT_RES_PRODUCT definition
  public
  inheriting from CL_ADT_REST_RESOURCE
  final
  create public .

public section.

  methods GET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ADT_RES_PRODUCT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADT_RES_PRODUCT->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] REQUEST                        TYPE REF TO IF_ADT_REST_REQUEST
* | [--->] RESPONSE                       TYPE REF TO IF_ADT_REST_RESPONSE
* | [--->] CONTEXT                        TYPE REF TO IF_REST_CONTEXT(optional)
* | [!CX!] CX_ADT_REST
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get.
  DATA:     lv_product_id   TYPE comm_product-product_id,
            lv_product_type TYPE comm_product-product_type,
            lv_description  TYPE comm_prshtext-short_text,
            lv_text         TYPE comm_prshtext,
            lv_product      TYPE comm_product,
            lv_data         TYPE zcl_adt_res_pro_content_handle=>ty_product,
            content_handler TYPE REF TO if_adt_rest_content_handler.

  request->get_uri_attribute(
    EXPORTING
      name      = 'product_id'
      mandatory = abap_true
     IMPORTING
      value     = lv_product_id ).

  SELECT SINGLE * FROM comm_product INTO lv_product WHERE product_id = lv_product_id.

  IF sy-subrc = 4.
    RAISE EXCEPTION TYPE cx_adt_res_not_found.
  ELSE.

  lv_data-product_id = lv_product-product_id.
  lv_data-product_type = lv_product-product_type.

  SELECT SINGLE * INTO lv_text FROM comm_prshtext WHERE product_guid = lv_product-product_guid.
  lv_data-description = lv_text-short_text.
  CREATE OBJECT content_handler TYPE zcl_adt_res_pro_content_handle.

  response->set_body_data( content_handler = content_handler
                             data            = lv_data ).
  ENDIF.

ENDMETHOD.
ENDCLASS.