class ZCL_READER definition
  public
  final
  create public .

public section.

  methods READ_FINISHED
    importing
      !P_TASK type CLIKE .
  methods CONSTRUCTOR
    importing
      !IV_NAME type STRING
      !IV_COUNT type INT4 .
  methods READ .
  methods PRINT_LOG .
protected section.
private section.

  data MV_NAME type STRING .
  data MV_COUNT type INT4 .
  data MO_DATA type ref to ZCL_DATA .
  data MT_LOG type STRING_TABLE .
ENDCLASS.



CLASS ZCL_READER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_READER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NAME                        TYPE        STRING
* | [--->] IV_COUNT                       TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.
   mv_name = iv_name.
   mv_count = iv_count.
   mo_data = zcl_data=>get_instance( ).
   mo_data->increase_read_number( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_READER->PRINT_LOG
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method PRINT_LOG.
    loop at mt_log ASSIGNING FIELD-SYMBOL(<log>).
      WRITE:/ <log>.
    endloop.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_READER->READ
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method READ.
    data(lv_task) = conv string( mv_name ).

    CALL FUNCTION 'ZREAD' STARTING NEW TASK lv_task
    CALLING read_finished ON END OF TASK
      EXPORTING
         iv_count = mv_count
         iv_name = mv_name.


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_READER->READ_FINISHED
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_TASK                         TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_finished.

    RECEIVE RESULTS FROM FUNCTION 'ZREAD'
   CHANGING
     et_read_result              = mt_log
   EXCEPTIONS
     system_failure        = 1
     communication_failure = 2.

    mo_data->decrease_read_number( ).
  ENDMETHOD.
ENDCLASS.