REPORT demo_db_locator.

DATA: lt_text TYPE string_table,
      length  TYPE i.

DATA: pattern  TYPE string VALUE 'ABAP',
      lv_index TYPE int4 VALUE 1.

zcl_abap_benchmark_tool=>start_timer( ).
SELECT text FROM zsotr_textu9 WHERE langu = @sy-langu INTO TABLE @lt_text.
LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<text>).
  length = length + strlen( <text> ).
  IF find( val = <text> sub = pattern  ) <> -1.
    lv_index = lv_index + 1.
  ENDIF.
ENDLOOP.
zcl_abap_benchmark_tool=>stop_timer( ).

WRITE: / 'total length:', length, ' matched for ABAP:', lv_index.
zcl_abap_benchmark_tool=>print_used_memory( ).
CLEAR: lt_text.
zcl_abap_benchmark_tool=>gc( ).
zcl_abap_benchmark_tool=>print_used_memory( ).