class ZCL_SWITCH definition
  public
  final
  create public .

public section.

  methods SET_SWITCHABLE
    importing
      !IO_SWITCHABLE type ref to OBJECT .
  methods PUSH .
protected section.
private section.

  data ISSWITCHON type ABAP_BOOL .
  data MO_SWITCHABLE type ref to ZIF_SWITCHABLE .
ENDCLASS.



CLASS ZCL_SWITCH IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SWITCH->PUSH
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD push.
    IF isswitchon = abap_true.
      mo_switchable->off( ).
      isswitchon = abap_false.
    ELSE.
      mo_switchable->on( ).
      isswitchon = abap_true.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SWITCH->SET_SWITCHABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_SWITCHABLE                  TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SET_SWITCHABLE.
    IF io_switchable IS INSTANCE OF zif_switchable.
      mo_switchable ?= io_switchable.
    ENDIF.
  endmethod.
ENDCLASS.