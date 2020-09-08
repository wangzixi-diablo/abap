class ZCL_BLOCKCHAIN_V2 definition
  public
  final
  create public .

public section.

  methods IS_VALID
    returning
      value(RV_VALID) type ABAP_BOOL .
  methods ADD_BLOCK
    importing
      !IO_BLOCK type ref to ZCL_BLOCK2 .
  methods GET_BLOCK_BY_INDEX
    importing
      !IV_INDEX type INT4
    returning
      value(RO_BLOCK) type ref to ZCL_BLOCK2 .
  methods GET_SIZE
    returning
      value(RV_SIZE) type INT4 .
  methods CONSTRUCTOR
    importing
      !IV_DIFFICULTY type INT4 .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_block,
        line TYPE REF TO zcl_block2,
      END OF ty_block .
  types:
    tt_block TYPE STANDARD TABLE OF ty_block .

  data MT_BLOCK type TT_BLOCK .
  data MV_DIFFICULTY type INT4 .

  methods GET_LAST_BLOCK
    returning
      value(RO_BLOCK) type ref to ZCL_BLOCK2 .
  methods CREATE_GENESIS_BLOCK
    returning
      value(RO_BLOCK) type ref to ZCL_BLOCK2 .
ENDCLASS.



CLASS ZCL_BLOCKCHAIN_V2 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V2->ADD_BLOCK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_BLOCK                       TYPE REF TO ZCL_BLOCK2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ADD_BLOCK.
    io_block->mv_phash = get_last_block( )->mv_hash.
    io_block->calculate_hash( ).

    io_block->mine( mv_difficulty ).
    APPEND INITIAL LINE TO mt_block ASSIGNING FIELD-SYMBOL(<newblock>).

    <newblock>-line = io_block.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V2->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DIFFICULTY                  TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    mv_difficulty = iv_difficulty.

    APPEND INITIAL LINE TO mt_block ASSIGNING FIELD-SYMBOL(<genesis>).

    <genesis>-line = create_genesis_block( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BLOCKCHAIN_V2->CREATE_GENESIS_BLOCK
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CREATE_GENESIS_BLOCK.

    ro_block = NEW zcl_block2( iv_index = 0 iv_data = 'Genesis block' ).
    ro_block->calculate_hash( ).
    ro_block->mine( mv_difficulty ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V2->GET_BLOCK_BY_INDEX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        INT4
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_BLOCK_BY_INDEX.
    CHECK iv_index <= lines( mt_block ).

    ro_block = mt_block[ iv_index ]-line.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BLOCKCHAIN_V2->GET_LAST_BLOCK
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_LAST_BLOCK.
    CHECK mt_block IS NOT INITIAL.

    DATA(size) = lines( mt_block ).

    ro_block = mt_block[ size ]-line.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V2->GET_SIZE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SIZE                        TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_SIZE.
    rv_size = lines( mt_block ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN_V2->IS_VALID
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
ENDCLASS.