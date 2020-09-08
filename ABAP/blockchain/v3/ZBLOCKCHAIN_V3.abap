*&---------------------------------------------------------------------*
*& Report ZBLOCKCHAIN_V1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zblockchain_v3.

PARAMETERS: dif TYPE int4 DEFAULT 3.

DATA(lo_chain) = NEW zcl_blockchain_v3( IV_DIFFICULTY = dif ).

lo_chain->create_transaction( new zcl_transaction( iv_from = 'Tom' iv_to = 'Jerry' iv_amount = 100 ) ).
lo_chain->create_transaction( new zcl_transaction( iv_from = 'Jerry' iv_to = 'Tom' iv_amount = 10 ) ).

lo_chain->mine_pending_trans( 'Jerry' ).

write:/ 'Jerry Current money: ' ,  lo_chain->get_current_coin( 'Jerry' ).

lo_chain->mine_pending_trans( 'Jerry' ).

write:/ 'Jerry Current money: ' ,  lo_chain->get_current_coin( 'Jerry' ).