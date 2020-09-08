REPORT zreduce1.

DATA: lt_status TYPE TABLE OF crm_jsto.

SELECT * INTO TABLE lt_status FROM crm_jsto.

DATA(lo_tool) = NEW zcl_status_calc_tool( ).

lo_tool = REDUCE #( INIT  o = lo_tool
                          local_item = VALUE zcl_status_calc_tool=>ty_status_result( )
                     FOR GROUPS <group_key> OF <wa> IN lt_status
                      GROUP BY ( obtyp = <wa>-obtyp stsma = <wa>-stsma )
       ASCENDING NEXT local_item = VALUE #( obtyp = <group_key>-obtyp
                                             stsma = <group_key>-stsma
       count = REDUCE i( INIT sum = 0 FOR m IN GROUP <group_key>
               NEXT sum = sum + 1 ) )
       o = o->add_result( local_item ) ).

DATA(ls_result) = lo_tool->get_result( ).