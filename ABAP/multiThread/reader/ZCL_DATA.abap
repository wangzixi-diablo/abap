class ZCL_DATA definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR .
  methods READ
    returning
      value(RV_DATA) type STRING .
  methods WRITE
    importing
      !IV_CHAR type C .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_DATA .
  methods SHOULD_END
    returning
      value(RV_END) type ABAP_BOOL .
  methods INCREASE_READ_NUMBER .
  methods DECREASE_READ_NUMBER .
protected section.
private section.

  class-data SO_INSTANCE type ref to ZCL_DATA .
ENDCLASS.



CLASS ZCL_DATA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DATA=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CLASS_CONSTRUCTOR.
    so_instance = new zcl_data( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DATA->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.
*     DO iv_size TIMES.
*        append conv string( sy-index ) TO mt_buffer.
*     ENDDO.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DATA->DECREASE_READ_NUMBER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DECREASE_READ_NUMBER.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DATA=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_INSTANCE.
    ro_instance = so_instance.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DATA->INCREASE_READ_NUMBER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method INCREASE_READ_NUMBER.
    ZCL_JERRY_SHARED_ROOT=>increase_read_number( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DATA->READ
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_DATA                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method READ.
    rv_data = zcl_jerry_shared_root=>read( ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DATA->SHOULD_END
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_END                         TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SHOULD_END.
    rv_end = boolc( zcl_jerry_shared_root=>get_read_number( ) = 0 ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DATA->WRITE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CHAR                        TYPE        C
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method WRITE.
*    LOOP AT mt_buffer ASSIGNING FIELD-SYMBOL(<buffer>).
*       <buffer> = iv_char.
*    ENDLOOP.
  endmethod.
ENDCLASS.