class ZCL_COMMAND_JOIN definition
  public
  final
  create public .

public section.

  interfaces ZIF_COMMAND .
protected section.
private section.

  data MT_SPLITED_TO_JOIN type STRING_TABLE .
  data MV_RESULT type STRING .
ENDCLASS.



CLASS ZCL_COMMAND_JOIN IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMAND_JOIN->ZIF_COMMAND~DO
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_COMMAND~DO.
    LOOP AT MT_SPLITED_TO_JOIN ASSIGNING FIELD-SYMBOL(<string>).
      mv_result = mv_result && '-' && <string>.
    ENDLOOP.
    CHECK sy-subrc = 0.

    data(lv_len) = strlen( mv_result ) - 1.
    mv_result = mv_result+1(lv_len).

    zif_command~result_Type = 'STRING'.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMAND_JOIN->ZIF_COMMAND~GET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<---] RV_RESULT                      TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_COMMAND~GET_RESULT.
    rv_result = mv_result.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMAND_JOIN->ZIF_COMMAND~SET_TASK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TASK                        TYPE        ANY
* | [<-()] RO_CMD                         TYPE REF TO ZIF_COMMAND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_COMMAND~SET_TASK.
    MT_SPLITED_TO_JOIN = iv_task.
    ro_cmd = me.
  endmethod.
ENDCLASS.