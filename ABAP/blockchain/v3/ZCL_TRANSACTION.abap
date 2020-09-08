class ZCL_TRANSACTION definition
  public
  final
  create public .

public section.

  data MV_FROM_ADDRESS type STRING .
  data MV_TO_ADDRESS type STRING .
  data MV_AMOUNT type INT4 .

  methods CONSTRUCTOR
    importing
      !IV_FROM type STRING
      !IV_TO type STRING
      !IV_AMOUNT type INT4 .
  methods TO_STRING
    returning
      value(RV_RESULT) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TRANSACTION IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TRANSACTION->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FROM                        TYPE        STRING
* | [--->] IV_TO                          TYPE        STRING
* | [--->] IV_AMOUNT                      TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.
    mv_from_address = iv_from.
    mv_to_address = iv_to.
    mv_amount = iv_amount.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TRANSACTION->TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method TO_STRING.
    rv_result = mv_from_address && mv_to_address && mv_amount.
  endmethod.
ENDCLASS.