class ZCL_ORDER_CUMULATE_API definition
  public
  final
  create public .

public section.

  methods GET_ORDER_HEADER_GROSS_WEIGHT
    importing
      !IV_GUID type CRMT_OBJECT_GUID
    returning
      value(RV_GROSS_WEIGHT) type CRMT_GROSS_WEIGHT_CUM .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ORDER_CUMULATE_API IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ORDER_CUMULATE_API->GET_ORDER_HEADER_GROSS_WEIGHT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_GUID                        TYPE        CRMT_OBJECT_GUID
* | [<-()] RV_GROSS_WEIGHT                TYPE        CRMT_GROSS_WEIGHT_CUM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_order_header_gross_weight.
    DATA: lt_order_i   TYPE TABLE OF crmd_orderadm_i-guid,
          lt_product_i TYPE TABLE OF crmd_product_i.

    SELECT guid INTO TABLE lt_order_i FROM crmd_orderadm_i
       WHERE header = iv_guid.
    CHECK sy-subrc = 0.
    SELECT guid gross_weight INTO CORRESPONDING FIELDS OF TABLE lt_product_i FROM crmd_product_i
       FOR ALL ENTRIES IN lt_order_i WHERE guid = lt_order_i-table_line.

    LOOP AT lt_product_i ASSIGNING FIELD-SYMBOL(<product>).
      rv_gross_weight = rv_gross_weight + <product>-gross_weight.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.