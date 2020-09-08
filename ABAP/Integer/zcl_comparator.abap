CLASS zcl_comparator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS compare
      IMPORTING
        !iv_a            TYPE int4
        !iv_b            TYPE int4
      RETURNING
        VALUE(rv_result) TYPE int4 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_COMPARATOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMPARATOR=>COMPARE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_A                           TYPE        INT4
* | [--->] IV_B                           TYPE        INT4
* | [<-()] RV_RESULT                      TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD compare.

    DEFINE shift_right.
      lv_diff = a->get_raw_value( ).
      a->shift_right( &1 ).
      lo_diff = zcl_integer=>value_of( lv_diff ).
      a = lo_diff->or( a ).
    END-OF-DEFINITION.

    DATA(a) = zcl_integer=>value_of( iv_a ).
    DATA(b) = zcl_integer=>value_of( iv_b ).
    DATA: lv_diff TYPE int4,
          lo_diff TYPE REF TO zcl_integer.
    a = a->xor( b ).
    IF a->get_raw_value( ) IS INITIAL.
      rv_result = 0.
      RETURN.
    ENDIF.

    shift_right 1.
    shift_right 2.
    shift_right 4.
    shift_right 8.
    shift_right 16.

    lv_diff = a->get_raw_value( ).
    a->shift_right( 1 ).
    lo_diff = zcl_integer=>value_of( lv_diff ).
    a = lo_diff->xor( a ).

    DATA(lo_origin_a) = zcl_integer=>value_of( iv_a ).
    rv_result = zcl_integer=>value_of( lo_origin_a->and( a )->get_raw_value( ) )->get_raw_value( ).

    rv_result = COND #( WHEN rv_result IS INITIAL THEN -1 ELSE 1 ).
  ENDMETHOD.
ENDCLASS.