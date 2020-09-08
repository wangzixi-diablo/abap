*&---------------------------------------------------------------------*
*& Report ZLAUNCHER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

* Jerry 2017-06-04 6:28PM in Wiesloch: can execute based on a very large number of task
REPORT zlauncher.

DATA:lv_taskid        TYPE c LENGTH 8,
     lv_index         TYPE c LENGTH 4,
     lv_current_index TYPE int4,
     lt_result        TYPE string_table,
     lt_total         LIKE lt_result,
     lv_left          TYPE int4,
     lv_total         TYPE int4.

lv_left = lv_total = 300.
CL_crm_order_timer=>start( ).
DO lv_total TIMES.
  lv_taskid = 'Task' && sy-index.
  CALL FUNCTION 'ZEXAMPLE_WORKER'
    STARTING NEW TASK lv_taskid
    PERFORMING read_finished ON END OF TASK
    EXPORTING
      iv_input = sy-index.
ENDDO.

WAIT UNTIL lv_left = 0.

CL_crm_order_timer=>stop( | lines: { lines( lt_total ) } | ).

FORM read_finished USING p_task TYPE clike.
  RECEIVE RESULTS FROM FUNCTION 'ZEXAMPLE_WORKER'
    CHANGING
      ct_result              = lt_result
    EXCEPTIONS
      system_failure        = 1
      communication_failure = 2.

  lv_left = lv_left - 1.
  APPEND LINES OF lt_result TO lt_total.
ENDFORM.

FUNCTION ZEXAMPLE_WORKER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_INPUT) TYPE  STRING
*"  CHANGING
*"     VALUE(CT_RESULT) TYPE  STRING_TABLE
*"----------------------------------------------------------------------

WAIT UP TO 10 seconds.

APPEND iv_input TO ct_result.

ENDFUNCTION.