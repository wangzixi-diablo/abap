*&---------------------------------------------------------------------*
*& Report ZTHREAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTHREAD.


data(lo_data) = zcl_data=>GET_INSTANCE( ).

data(lo_reader1) = new zcl_reader( iv_name = 'reader1' iv_count = 1 ).
lo_reader1->read( ).

data(lo_reader2) = new zcl_reader( iv_name = 'reader2' iv_count = 2 ).
lo_reader2->read( ).

data(lo_reader3) = new zcl_reader( iv_name = 'reader3' iv_count = 3 ).
lo_reader3->read( ).

wait UNTIL lo_data->should_end( ) = abap_true.

lo_reader1->print_log( ).
lo_reader2->print_log( ).
lo_reader3->print_log( ).
WRITE: / 'done'.