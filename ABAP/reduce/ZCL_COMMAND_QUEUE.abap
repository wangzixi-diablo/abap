CLASS zcl_command_queue DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_cmd_queue,
        cmd TYPE REF TO zif_command,
      END OF ty_cmd_queue .
    TYPES:
      tt_cmd_queue TYPE TABLE OF ty_cmd_queue WITH KEY cmd .

    DATA mt_queue TYPE tt_cmd_queue READ-ONLY .

    CLASS-METHODS class_constructor .
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_command_queue .
    METHODS insert
      IMPORTING
        !io_command    TYPE REF TO zif_command
      RETURNING
        VALUE(ro_this) TYPE REF TO zcl_command_queue .
    METHODS execute
      EXPORTING
        !ev_result TYPE any .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA so_instance TYPE REF TO zcl_command_queue .
    DATA mo_previous_cmd TYPE REF TO zif_command .

    METHODS get_next
      RETURNING
        VALUE(ro_cmd) TYPE REF TO zif_command .
    METHODS pop
      RETURNING
        VALUE(ro_cmd) TYPE REF TO zif_command .
ENDCLASS.



CLASS ZCL_COMMAND_QUEUE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMAND_QUEUE=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
    CREATE OBJECT so_instance.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMAND_QUEUE->EXECUTE
* +-------------------------------------------------------------------------------------------------+
* | [<---] EV_RESULT                      TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD execute.

    DATA(lv_end) = lines( me->mt_queue ).

    DATA(lr_result) = REDUCE #( INIT work = me->pop( )
                           FOR n = 1  UNTIL n = lv_end
                           NEXT
                           work = me->get_next( )  ).

    lr_result->get_result( IMPORTING rv_result = ev_result ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_COMMAND_QUEUE=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_COMMAND_QUEUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.
    ro_instance = so_instance.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMAND_QUEUE->GET_NEXT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_CMD                         TYPE REF TO ZIF_COMMAND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_next.
*    FIELD-SYMBOLS: <previous_result> TYPE any.
*
*    DATA: lo_next TYPE REF TO ZIF_COMMAND.
*
*    lo_next = pop( ).
*    CHECK MO_PREVIOUS_CMD IS NOT INITIAL AND lo_next IS NOT INITIAL.
*    MO_PREVIOUS_CMD->get_result( IMPORTING rv_result = <previous_result> ).
*
*    lo_next->set_task( <previous_result> ).
*    ro_cmd->do( ).
*    ro_cmd = lo_next.

    ro_cmd = pop( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_COMMAND_QUEUE->INSERT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_COMMAND                     TYPE REF TO ZIF_COMMAND
* | [<-()] RO_THIS                        TYPE REF TO ZCL_COMMAND_QUEUE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD insert.

    DATA(ls_queue) = VALUE ty_cmd_queue( cmd = io_command ).
    APPEND ls_queue TO mt_queue.

    ro_this = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_COMMAND_QUEUE->POP
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_CMD                         TYPE REF TO ZIF_COMMAND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD pop.

    FIELD-SYMBOLS: <result> TYPE any.
    DATA: lr_any TYPE REF TO data.
    READ TABLE mt_queue ASSIGNING FIELD-SYMBOL(<head>) INDEX 1.
    CHECK sy-subrc = 0.

    IF mo_previous_cmd IS INITIAL. "first, can work without explicit input
      <head>-cmd->do( ).
    ELSE. " second, get input from previous result
      CREATE DATA lr_any TYPE (mo_previous_cmd->result_type).
      ASSIGN lr_any->* TO <result>.
      mo_previous_cmd->get_result( IMPORTING rv_result = <result> ).
      <head>-cmd->set_task( <result> ).
      <head>-cmd->do( ).
    ENDIF.
    mo_previous_cmd = ro_cmd = <head>-cmd.
    DELETE mt_queue INDEX 1.
  ENDMETHOD.
ENDCLASS.