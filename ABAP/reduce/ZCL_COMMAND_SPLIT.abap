class ZCL_COMMAND_SPLIT definition
  public
  final
  create public .

public section.

  interfaces ZIF_COMMAND .
protected section.
private section.

  data MV_STRING_TO_SPLIT type STRING .
  data MT_SPLITED type STRING_TABLE .
ENDCLASS.



CLASS ZCL_COMMAND_SPLIT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMAND_SPLIT->ZIF_COMMAND~DO
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_COMMAND~DO.
    SPLIT mv_string_to_split AT SPACE INTO TABLE mt_splited.
    zif_command~result_Type = 'STRING_TABLE'.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMAND_SPLIT->ZIF_COMMAND~GET_NEXT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_COMMAND~GET_NEXT.

    ZIF_COMMAND~do( ).
    ro_next = ZIF_COMMAND~next.

    CHECK ZIF_COMMAND~next IS NOT INITIAL.
    ZIF_COMMAND~next->set_task( mt_splited ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMAND_SPLIT->ZIF_COMMAND~GET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<---] RV_RESULT                      TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_COMMAND~GET_RESULT.
    rv_result = MT_SPLITED.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMAND_SPLIT->ZIF_COMMAND~SET_NEXT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_COMMAND~SET_NEXT.
    ZIF_COMMAND~next = io_next.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMAND_SPLIT->ZIF_COMMAND~SET_TASK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TASK                        TYPE        ANY
* | [<-()] RO_CMD                         TYPE REF TO ZIF_COMMAND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_COMMAND~SET_TASK.
    mv_string_to_split = iv_task.
    ro_cmd = me.
  endmethod.
ENDCLASS.