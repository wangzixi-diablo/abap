class ZCL_ANIMAL_CONTAINER definition
  public
  final
  create public .

public section.

  interfaces ZIF_COVARIANCE
      data values G_TYPE = 'ZCL_DOG' .

  methods GET
    importing
      !IV_INDEX type INT4
    returning
      value(RO_RESULT) type ref to ZCL_ANIMAL .
  methods ADD
    importing
      !IO_ANIMAL type ref to ZCL_ANIMAL .
  methods SIZE
    returning
      value(RV_SIZE) type INT4 .
  methods CONSTRUCTOR
    importing
      !IV_CONCRETE_TYPE type STRING .
protected section.
private section.

  types TY_REF type ref to ZCL_ANIMAL .

  data:
    DATA type STANDARD TABLE OF ty_ref .
ENDCLASS.



CLASS ZCL_ANIMAL_CONTAINER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ANIMAL_CONTAINER->ADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_ANIMAL                      TYPE REF TO ZCL_ANIMAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ADD.
    APPEND io_animal TO data.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ANIMAL_CONTAINER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CONCRETE_TYPE               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ANIMAL_CONTAINER->GET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        INT4
* | [<-()] RO_RESULT                      TYPE REF TO ZCL_ANIMAL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET.
    READ TABLE data INTO ro_result INDEX iv_index.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ANIMAL_CONTAINER->SIZE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SIZE                        TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SIZE.
    rv_size = lines( data ).
  endmethod.
ENDCLASS.