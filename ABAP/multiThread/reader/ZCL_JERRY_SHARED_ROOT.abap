class ZCL_JERRY_SHARED_ROOT definition
  public
  final
  create public
  shared memory enabled .

public section.

  interfaces IF_SHM_BUILD_INSTANCE .

  class-methods INCREASE_READ_NUMBER .
  methods INCREASE_WRITE_NUMBER .
  methods DECREASE_WRITE_NUMBER .
  class-methods DECREASE_READ_NUMBER .
  class-methods READ
    returning
      value(RV_RESULT) type STRING .
  class-methods GET_READ_NUMBER
    returning
      value(RV_NUM) type INT4 .
protected section.
private section.

  data MT_DATA type STRING_TABLE .
  data MV_SIZE type INT4 value 10 ##NO_TEXT.
  data MV_READ_THREAD_NUM type INT4 .
  data MV_WRITE_THREAD_NUM type INT4 .

  methods INIT .
  methods GET_DATA_INTERNAL
    returning
      value(RV_RESULT) type STRING .
ENDCLASS.



CLASS ZCL_JERRY_SHARED_ROOT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JERRY_SHARED_ROOT=>DECREASE_READ_NUMBER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DECREASE_READ_NUMBER.
    data: area type ref to zcl_jerry_shared_area,
      root type ref to zcl_jerry_shared_root.

try.
  area = zcl_jerry_shared_area=>attach_for_update( ).
catch cx_shm_no_active_version.
 write:/ 'no active version!'.
 return.
endtry.

root ?= area->get_root( ).
SUBTRACT 1 from root->mv_read_thread_num.
area->set_root( root ).
area->detach_commit( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JERRY_SHARED_ROOT->DECREASE_WRITE_NUMBER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DECREASE_WRITE_NUMBER.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JERRY_SHARED_ROOT->GET_DATA_INTERNAL
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_DATA_INTERNAL.
    LOOP AT mt_data ASSIGNING FIELD-SYMBOL(<data>).
       rv_result = rv_result && <data>.
    ENDLOOP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JERRY_SHARED_ROOT=>GET_READ_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_NUM                         TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_READ_NUMBER.
   data: area type ref to zcl_jerry_shared_area.
try.
  area = zcl_jerry_shared_area=>attach_for_read( ).
catch cx_shm_no_active_version.
 write:/ 'no active version!'.
 return.
endtry.

 rv_num = area->root->mv_read_thread_num.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JERRY_SHARED_ROOT=>IF_SHM_BUILD_INSTANCE~BUILD
* +-------------------------------------------------------------------------------------------------+
* | [--->] INST_NAME                      TYPE        SHM_INST_NAME (default =CL_SHM_AREA=>DEFAULT_INSTANCE)
* | [--->] INVOCATION_MODE                TYPE        SHM_CONSTR_INVOCATION_MODE (default =CL_SHM_AREA=>INVOCATION_MODE_EXPLICIT)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IF_SHM_BUILD_INSTANCE~BUILD.
    data: area type ref to zcl_jerry_shared_area,
      root type ref to zcl_jerry_shared_root.

area = zcl_jerry_shared_area=>attach_for_write( ).
create object root area handle area.

root->init( ).

area->set_root( root = root ).
area->detach_commit( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JERRY_SHARED_ROOT=>INCREASE_READ_NUMBER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method INCREASE_READ_NUMBER.
    data: area type ref to zcl_jerry_shared_area,
      root type ref to zcl_jerry_shared_root.

try.
  area = zcl_jerry_shared_area=>attach_for_update( ).
catch cx_shm_no_active_version.
 write:/ 'no active version!'.
 return.
endtry.

root ?= area->get_root( ).
ADD 1 tO root->mv_read_thread_num.
area->set_root( root ).
area->detach_commit( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_JERRY_SHARED_ROOT->INCREASE_WRITE_NUMBER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method INCREASE_WRITE_NUMBER.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_JERRY_SHARED_ROOT->INIT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method INIT.
    DO mv_size TIMES.
       append conv string( sy-index ) to mt_data.
    ENDDO.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_JERRY_SHARED_ROOT=>READ
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method READ.
    data: area type ref to zcl_jerry_shared_area.
try.
  area = zcl_jerry_shared_area=>attach_for_read( ).
catch cx_shm_no_active_version.
 write:/ 'no active version!'.
 return.
endtry.

 rv_result = area->root->get_data_internal( ).
  endmethod.
ENDCLASS.