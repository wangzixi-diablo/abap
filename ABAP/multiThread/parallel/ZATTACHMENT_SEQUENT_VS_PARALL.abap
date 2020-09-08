*&---------------------------------------------------------------------*
*& Report ZATTACHMENT_SEQUEN_VS_PARALL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZATTACHMENT_SEQUENT_VS_PARALL.

data: lv_block_Size type int4 value 30.

DATA(lo_tool) = NEW zcl_crm_attachments_tool( ).
DATA(lt_input) = lo_tool->get_testdata( ).

WRITE: / 'Block Size: ' COLOR COL_NEGATIVE, lv_block_Size.
lo_tool->start( ).
DATA(lt_para) = lo_tool->parallel_read( it_orders =  lt_input iv_block_size = lv_block_Size ).
lo_tool->stop( 'parallel read: ' ).
WRITE: / 'total numbers of attachments read: ' , lines( lt_para ) COLOR COL_GROUP.

lo_tool->start( ).
DATA(lt_sequ) = lo_tool->sequential_read( it_orders = lt_input ).
lo_tool->stop( 'sequential read: ' ).
WRITE: / 'total numbers of attachments read: ' , lines( lt_sequ ) COLOR COL_GROUP.

DATA(lv_equal) = lo_tool->compare_read_result( it_origin = lt_sequ
                                               it_jerry = lt_para ).

WRITE: / 'equal? ', lv_equal COLOR COL_NEGATIVE.