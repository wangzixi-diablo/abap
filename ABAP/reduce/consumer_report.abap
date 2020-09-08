REPORT zsales_read_test.

DATA: lr_result TYPE REF TO string.
DATA(lo_queue) = zcl_command_queue=>get_instance( ).

lo_queue->insert( NEW zcl_command_split( )->zif_command~set_task( 'Jerry Java Scala' ) ).
lo_queue->insert( NEW zcl_command_lower( ) ).
lo_queue->insert( NEW zcl_command_join( ) ).

CREATE DATA lr_result.
ASSIGN lr_result->* TO FIELD-SYMBOL(<result>).

lo_queue->execute( IMPORTING ev_result = <result> ).

WRITE: / <result>.