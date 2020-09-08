CLASS zcl_blockchain DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS is_valid
      RETURNING
        VALUE(rv_valid) TYPE abap_bool .
    METHODS add_block
      IMPORTING
        !io_block TYPE REF TO zcl_block .
    METHODS get_block_by_index
      IMPORTING
        !iv_index       TYPE int4
      RETURNING
        VALUE(ro_block) TYPE REF TO zcl_block .
    METHODS get_size
      RETURNING
        VALUE(rv_size) TYPE int4 .
    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_block,
        line TYPE REF TO zcl_block,
      END OF ty_block .
    TYPES:
      tt_block TYPE STANDARD TABLE OF ty_block .

    DATA mt_block TYPE tt_block .

    METHODS get_last_block
      RETURNING
        VALUE(ro_block) TYPE REF TO zcl_block .
    METHODS create_genesis_block
      RETURNING
        VALUE(ro_block) TYPE REF TO zcl_block .
ENDCLASS.



CLASS ZCL_BLOCKCHAIN IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN->ADD_BLOCK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_BLOCK                       TYPE REF TO ZCL_BLOCK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_block.
    io_block->mv_phash = get_last_block( )->mv_hash.
    io_block->calculate_hash( ).

    APPEND INITIAL LINE TO mt_block ASSIGNING FIELD-SYMBOL(<newblock>).

    <newblock>-line = io_block.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    APPEND INITIAL LINE TO mt_block ASSIGNING FIELD-SYMBOL(<genesis>).

    <genesis>-line = create_genesis_block( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BLOCKCHAIN->CREATE_GENESIS_BLOCK
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_genesis_block.

    ro_block = NEW zcl_block( iv_index = 0 iv_data = 'Genesis block' ).
    ro_block->calculate_hash( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN->GET_BLOCK_BY_INDEX
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        INT4
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_block_by_index.
    CHECK iv_index <= lines( mt_block ).

    ro_block = mt_block[ iv_index ]-line.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BLOCKCHAIN->GET_LAST_BLOCK
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_BLOCK                       TYPE REF TO ZCL_BLOCK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_last_block.
    CHECK mt_block IS NOT INITIAL.

    DATA(size) = lines( mt_block ).

    ro_block = mt_block[ size ]-line.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN->GET_SIZE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SIZE                        TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_size.
    rv_size = lines( mt_block ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BLOCKCHAIN->IS_VALID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_VALID                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD is_valid.

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