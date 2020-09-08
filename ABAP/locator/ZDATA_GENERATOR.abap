*&---------------------------------------------------------------------*
*& Report ZTEST_TOOL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_tool.


DATA: lt_new_table TYPE TABLE OF zsotr_textu9,
      lt_old_table TYPE TABLE OF sotr_textu.

SELECT * INTO TABLE lt_old_table FROM sotr_textu.

MOVE-CORRESPONDING lt_old_table TO lt_new_table.

LOOP AT lt_new_table ASSIGNING FIELD-SYMBOL(<new>).
  DO 9 TIMES.
    <new>-text = <new>-text && <new>-text.
  ENDDO.
ENDLOOP.

INSERT zsotr_textu9 FROM TABLE lt_new_table.