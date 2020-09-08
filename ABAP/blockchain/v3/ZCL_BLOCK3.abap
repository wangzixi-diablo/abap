class ZCL_BLOCK3 definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_transaction,
            line TYPE REF TO zcl_transaction,
        END OF ty_transaction .
  types:
    tt_transaction TYPE TABLE OF ty_transaction .

  data MV_INDEX type STRING .
  data MV_PHASH type STRING .
  data MV_TIMESTAMP type STRING .
  data MV_HASH type STRING .
  data MV_NONCE type INT4 value 0 ##NO_TEXT.
  data MT_TRANSACTION type TT_TRANSACTION .

  methods CALCULATE_HASH .
  methods CONSTRUCTOR
    importing
      !IT_TRANSACTION type TT_TRANSACTION .
  methods MINE
    importing
      !IV_DIFFICULTY type INT4 .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_BLOCK3 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK3->CALCULATE_HASH
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD calculate_hash.
    DATA: lv_temp  TYPE string,
          lv_nonce TYPE string.

    lv_nonce = mv_nonce.

    CONCATENATE mv_timestamp mv_phash lv_nonce  INTO lv_temp.

    LOOP AT mt_transaction ASSIGNING FIELD-SYMBOL(<transaction>).
      DATA(lv_transaction) = <transaction>-line->to_string( ).
      CONCATENATE lv_transaction lv_temp INTO lv_temp.
    ENDLOOP.

    CALL METHOD cl_abap_message_digest=>calculate_hash_for_char
      EXPORTING
        if_algorithm  = 'SHA1'
        if_data       = lv_temp
      IMPORTING
        ef_hashstring = mv_hash.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK3->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TRANSACTION                 TYPE        TT_TRANSACTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONSTRUCTOR.

    DATA:  lv_time TYPE timestamp.

    GET TIME STAMP FIELD lv_time.

    mv_timestamp = lv_time.

    mt_transaction = it_transaction.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCK3->MINE
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
ENDCLASS.