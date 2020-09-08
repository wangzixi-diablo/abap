REPORT demo_db_locator.

DATA: otr_text_locator TYPE REF TO cl_abap_db_c_locator,
      length           TYPE i.

DATA: pattern  TYPE string VALUE 'ABAP',
      lv_index TYPE int4 VALUE 1.

zcl_abap_benchmark_tool=>start_timer( ).
TRY.
    SELECT text FROM zsotr_textu9 WHERE langu = @sy-langu INTO @otr_text_locator.

      length = length + otr_text_locator->get_length( ).

      IF otr_text_locator->find( start_offset = 0
                                 pattern      = pattern ) <> -1.
        lv_index = lv_index + 1.
      ENDIF.
      otr_text_locator->close( ).
    ENDSELECT.
  CATCH cx_lob_sql_error.
    WRITE 'Exception in locator' COLOR = 6.
    RETURN.
ENDTRY.

zcl_abap_benchmark_tool=>stop_timer( ).

WRITE: / 'total length:', length, ' matched for ABAP:', lv_index.
zcl_abap_benchmark_tool=>print_used_memory( ).

CLEAR: otr_text_locator.
zcl_abap_benchmark_tool=>gc( ).
zcl_abap_benchmark_tool=>print_used_memory( ).