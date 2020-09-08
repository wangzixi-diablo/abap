REPORT demo_db_locator.

DATA: lv_text TYPE string,
      length  TYPE i.

DATA: pattern  TYPE string VALUE 'ABAP',
      lv_index TYPE int4 VALUE 1.

zcl_abap_benchmark_tool=>start_timer( ).
SELECT text FROM zsotr_textu6 WHERE langu = @sy-langu INTO @lv_text.
  length = length + strlen( lv_text ).
  IF find( val = lv_text sub = pattern  ) <> -1.
    lv_index = lv_index + 1.
  ENDIF.

ENDSELECT.
zcl_abap_benchmark_tool=>stop_timer( ).

WRITE: / 'total length:', length, ' matched for ABAP:', lv_index.
zcl_abap_benchmark_tool=>print_used_memory( ).
CLEAR: lv_text.
zcl_abap_benchmark_tool=>gc( ).
zcl_abap_benchmark_tool=>print_used_memory( ).