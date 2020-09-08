CLASS zcl_block DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mv_index TYPE string .
    DATA mv_phash TYPE string .
    DATA mv_timestamp TYPE string .
    DATA mv_hash TYPE string .

    METHODS calculate_hash .
    METHODS constructor
      IMPORTING
        !iv_index TYPE int4
        !iv_data  TYPE string .
    METHODS set_data
      IMPORTING
        !iv_data TYPE string .
    METHODS get_data
      RETURNING
        VALUE(rv_data) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_data TYPE string .
ENDCLASS.



CLASS ZCL_BLOCK IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK->CALCULATE_HASH
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calculate_hash.
    DATA: lv_temp TYPE string.

    CONCATENATE mv_index mv_data mv_timestamp mv_phash  INTO lv_temp.
    CALL METHOD cl_abap_message_digest=>calculate_hash_for_char
      EXPORTING
        if_algorithm  = 'SHA1'
        if_data       = lv_temp
      IMPORTING
        ef_hashstring = mv_hash.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        INT4
* | [--->] IV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mv_index = iv_index.
    mv_data = iv_data.

    DATA:  lv_time TYPE timestamp.

    GET TIME STAMP FIELD lv_time.

    mv_timestamp = lv_time.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data.
    rv_data = mv_data.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK->SET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_data.
    mv_data = iv_data.
    calculate_hash( ).
  ENDMETHOD.
ENDCLASS.