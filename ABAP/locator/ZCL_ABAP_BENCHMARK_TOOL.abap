class ZCL_ABAP_BENCHMARK_TOOL definition
  public
  final
  create public .

public section.

  class-methods START_TIMER .
  class-methods STOP_TIMER .
  class-methods GC .
  class-methods PRINT_USED_MEMORY .
protected section.
private section.

  class-data MV_START type INT4 .
ENDCLASS.



CLASS ZCL_ABAP_BENCHMARK_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_BENCHMARK_TOOL=>GC
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GC.
    cl_abap_memory_utilities=>do_garbage_collection( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_BENCHMARK_TOOL=>PRINT_USED_MEMORY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method PRINT_USED_MEMORY.
cl_abap_memory_utilities=>get_total_used_size( IMPORTING size = DATA(lv_size) ).

DATA(lv_print) = | Memory used(bytes): { lv_size }.|.

WRITE: / lv_print COLOR COL_GROUP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_BENCHMARK_TOOL=>START_TIMER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method START_TIMER.
    GET RUN TIME FIELD mv_start.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ABAP_BENCHMARK_TOOL=>STOP_TIMER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method STOP_TIMER.
    GET RUN TIME FIELD data(rv_duration).
    rv_duration = rv_duration - mv_start.
    data(lv_print) = |Duration(microsecond): { rv_duration }|.

    WRITE:/ lv_print COLOR COL_KEY.
  endmethod.
ENDCLASS.