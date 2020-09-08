class ZCL_BLOCKCHAIN_V3 definition
  public
  final
  create public .

public section.

  methods IS_VALID
    returning
      value(RV_VALID) type ABAP_BOOL .
  methods GET_BLOCK_BY_INDEX
    importing
      !IV_INDEX type INT4
    returning
      value(RO_BLOCK) type ref to ZCL_BLOCK3 .
  methods GET_SIZE
    returning
      value(RV_SIZE) type INT4 .
  methods CONSTRUCTOR
    importing
      !IV_DIFFICULTY type INT4 .
  methods CREATE_TRANSACTION
    importing
      !IO_TRANSACTION type ref to ZCL_TRANSACTION .
  methods MINE_PENDING_TRANS
    importing
      !IV_AWARD_ADDRESS type STRING .
  methods GET_CURRENT_COIN
    importing
      !IV_ADDRESS type STRING
    returning
      value(RV_AMOUNT) type INT4 .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_block,
        line TYPE REF TO zcl_block3,
      END OF ty_block .
  types:
    tt_block TYPE STANDARD TABLE OF ty_block .

  data MT_BLOCK type TT_BLOCK .
  data MV_DIFFICULTY type INT4 .
  data MV_MINE_REWARD type INT4 value 100 ##NO_TEXT.
  data MT_PENDING_TRANS type ZCL_BLOCK3=>TT_TRANSACTION .

  methods GET_LAST_BLOCK
    returning
      value(RO_BLOCK) type ref to ZCL_BLOCK3 .
  methods CREATE_GENESIS_BLOCK
    returning
      value(RO_BLOCK) type ref to ZCL_BLOCK3 .
ENDCLASS.



CLASS ZCL_BLOCKCHAIN_V3 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V3->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DIFFICULTY                  TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONSTRUCTOR.
    mv_difficulty = iv_difficulty.

    APPEND INITIAL LINE TO mt_block ASSIGNING FIELD-SYMBOL(<genesis>).

    <genesis>-line = create_genesis_block( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BLOCKCHAIN_V3->CREATE_GENESIS_BLOCK
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CREATE_GENESIS_BLOCK.

    DATA: lt_transaction TYPE ZCL_block3=>tt_transaction.

    APPEND INITIAL LINE TO lt_transaction ASSIGNING FIELD-SYMBOL(<genesis>).

    data(lo_tran) = new zcl_transaction( iv_from = 'Bitcoin org' iv_to = space iv_amount = 0 ).

    <genesis>-line = lo_tran.

    ro_block = NEW zcl_block3( lt_transaction ).
    ro_block->calculate_hash( ).
    ro_block->mine( mv_difficulty ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V3->CREATE_TRANSACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_TRANSACTION                 TYPE REF TO ZCL_TRANSACTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CREATE_TRANSACTION.
    DATA: ls_trans TYPE zcl_block3=>ty_transaction.

    ls_trans-line = io_transaction.

    APPEND ls_trans TO mt_pending_trans.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V3->GET_BLOCK_BY_INDEX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        INT4
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_BLOCK_BY_INDEX.
    CHECK iv_index <= lines( mt_block ).

    ro_block = mt_block[ iv_index ]-line.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V3->GET_CURRENT_COIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ADDRESS                     TYPE        STRING
* | [<-()] RV_AMOUNT                      TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_CURRENT_COIN.
    LOOP At mt_block ASSIGNING FIELD-SYMBOL(<block>).
       LOOP at <block>-line->mt_transaction ASSIGNING FIELD-SYMBOL(<trans>).
         If <trans>-line->mv_from_address EQ iv_address.
            rv_amount = rv_amount - <trans>-line->mv_amount.
         ENDIF.

         IF <trans>-line->mv_to_address EQ iv_address.
            rv_amount = rv_amount + <trans>-line->mv_amount.
         ENDIF.
       ENDLOOP.
    ENDLOOP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BLOCKCHAIN_V3->GET_LAST_BLOCK
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK3
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_LAST_BLOCK.
    CHECK mt_block IS NOT INITIAL.

    DATA(size) = lines( mt_block ).

    ro_block = mt_block[ size ]-line.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V3->GET_SIZE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SIZE                        TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_SIZE.
    rv_size = lines( mt_block ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V3->IS_VALID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_VALID                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD IS_VALID.

    DATA(lv_size) = lines( mt_block ) - 1 .

    rv_valid = abap_true.

    DO lv_size TIMES. " sy-index = 1.
      DATA(i) = sy-index.
      DATA(current) = mt_block[ i + 1 ]-line.
      DATA(previous) = mt_block[ i ]-line.

      DATA(current_phash) = current->mv_phash.
      DATA(prev_hash) = previous->mv_hash.
      IF current_phash <> prev_hash.
        WRITE:/ | Current block index: { i + 1 }, hash: { current_phash } | COLOR COL_NEGATIVE.
        WRITE:/ | Previous block index: { i }, hash: { prev_hash } not equal !!! | COLOR COL_POSITIVE.
        rv_valid = abap_false.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V3->MINE_PENDING_TRANS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_AWARD_ADDRESS               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method MINE_PENDING_TRANS.

    DATA(lo_block) = new ZCL_block3( it_transaction = mt_pending_trans ).

    lo_block->calculate_hash( ).
    lo_block->mine( mv_difficulty ).

    APPEND INITIAL LINE TO mt_block ASSIGNING FIELD-SYMBOL(<new_block>).
    <new_block>-line = lo_block.

    CLEAR: mt_pending_trans.

    data(lo_award_trans) = new zcl_transaction( iv_from = 'Bitcoin org' iv_to = iv_award_address
                                                iv_amount = MV_MINE_REWARD ).

  APPEND INITIAL LINE TO mt_pending_trans ASSIGNING FIELD-SYMBOL(<award_tran>).
    <award_tran>-line = lo_award_trans.

  endmethod.
ENDCLASS.