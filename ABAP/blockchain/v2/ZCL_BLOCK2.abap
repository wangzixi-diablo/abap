class ZCL_BLOCK2 definition
  public
  final
  create public .

public section.

  data MV_INDEX type STRING .
  data MV_PHASH type STRING .
  data MV_TIMESTAMP type STRING .
  data MV_HASH type STRING .
  data MV_NONCE type INT4 value 0 ##NO_TEXT.

  methods CALCULATE_HASH .
  methods CONSTRUCTOR
    importing
      !IV_INDEX type INT4
      !IV_DATA type STRING .
  methods SET_DATA
    importing
      !IV_DATA type STRING .
  methods GET_DATA
    returning
      value(RV_DATA) type STRING .
  methods MINE
    importing
      !IV_DIFFICULTY type INT4 .
  PROTECTED SECTION.
private section.

  data MV_DATA type STRING .
ENDCLASS.



CLASS ZCL_BLOCK2 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK2->CALCULATE_HASH
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CALCULATE_HASH.
    DATA: lv_temp TYPE string,
          lv_nonce TYPE string.

    lv_nonce = mv_nonce.

    CONCATENATE mv_index mv_data mv_timestamp mv_phash lv_nonce  INTO lv_temp.
    CALL METHOD cl_abap_message_digest=>calculate_hash_for_char
      EXPORTING
        if_algorithm  = 'SHA1'
        if_data       = lv_temp
      IMPORTING
        ef_hashstring = mv_hash.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK2->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        INT4
* | [--->] IV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONSTRUCTOR.
    mv_index = iv_index.
    mv_data = iv_data.

    DATA:  lv_time TYPE timestamp.

    GET TIME STAMP FIELD lv_time.

    mv_timestamp = lv_time.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK2->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_DATA.
    rv_data = mv_data.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK2->MINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DIFFICULTY                  TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method MINE.
    DATA: lv_match TYPE string.

    DO iv_difficulty TIMES.
       lv_match = lv_match && '0'.
    ENDDO.

    DO.
      IF mv_hash+0(iv_difficulty) EQ lv_match.
         RETURN.
      ENDIF.

      mv_nonce = mv_nonce + 1.
      CALCULATE_HASH( ).
    ENDDO.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK2->SET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SET_DATA.
    mv_data = iv_data.
    calculate_hash( ).
  ENDMETHOD.
ENDCLASS.