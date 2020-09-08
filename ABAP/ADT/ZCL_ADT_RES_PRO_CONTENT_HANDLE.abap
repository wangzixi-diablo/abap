class ZCL_ADT_RES_PRO_CONTENT_HANDLE definition
  public
  inheriting from CL_ADT_REST_ST_HANDLER
  final
  create public .

public section.


  types:
    BEGIN OF ty_product,
      product_id type comm_product-product_id,
      product_type  type comm_product-product_type,
      description   type COMM_PRSHTEXT-SHORT_TEXT,
     END OF ty_product .
  types:
    tt_product TYPE STANDARD TABLE OF ty_product .

  constants CO_CONTENT_TYPE type STRING value IF_REST_MEDIA_TYPE=>GC_APPL_XML. "#EC NOTEXT

  methods CONSTRUCTOR .
protected section.
private section.

  constants CO_ST_NAME type STRING value 'ID'. "#EC NOTEXT
  constants CO_ROOT_NAME type STRING value 'PRODUCT_DATA'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_ADT_RES_PRO_CONTENT_HANDLE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ADT_RES_PRO_CONTENT_HANDLE->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONSTRUCTOR.
  super->constructor( st_name = co_st_name root_name = co_root_name content_type = co_content_type ).

endmethod.
ENDCLASS.