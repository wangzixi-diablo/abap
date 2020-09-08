*&---------------------------------------------------------------------*
*& Report ZTOOL_CODE_LINE_COUNT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTOOL_CODE_LINE_COUNT.


*=======================================================================
* SELECTION-SCREEN
*=======================================================================
SELECTION-SCREEN BEGIN OF BLOCK obj WITH FRAME TITLE text-obj.
INCLUDE ztool_dev_obj_selscr1.
SELECTION-SCREEN END OF BLOCK obj.

SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE text-opt.
PARAMETERS:
  p_cmpr_n TYPE xfeld RADIOBUTTON GROUP cmpr MODIF ID cmp,
  p_cmpr_o TYPE xfeld RADIOBUTTON GROUP cmpr DEFAULT 'X' MODIF ID cmp,
  p_cmpr_p TYPE xfeld RADIOBUTTON GROUP cmpr MODIF ID cmp,

  p_save   TYPE xfeld USER-COMMAND dummy MODIF ID sav.
SELECTION-SCREEN END OF BLOCK opt.


*=======================================================================
TYPE-POOLS:
*=======================================================================
  abap,
  seop,
  col.

*=======================================================================
TYPES:
*=======================================================================
  ty_gt_list TYPE STANDARD TABLE OF zcl_tool_src_code_location=>ty_ms_result.

*=======================================================================
DATA:
*=======================================================================
  go_salv_table        TYPE REF TO cl_salv_table,   "only needed for eventhandler on_added_function
  gt_list              TYPE ty_gt_list,
  gt_package           TYPE TABLE OF devclass,
  gv_no_package        TYPE abap_bool,

  go_src_code_analyzer TYPE REF TO zcl_tool_src_code_analyze,
  go_src_code_analysis TYPE REF TO zif_tool_src_code__analysis,
  gt_objkey            TYPE zcl_tool_src_code_analyze=>ty_mt_objkey.



*=======================================================================
CLASS lcl_handle_events DEFINITION CREATE PRIVATE FINAL.
*=======================================================================
  PUBLIC SECTION.
    CLASS-METHODS:
      on_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function,                          "#EC NEEDED
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,                               "#EC NEEDED
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.                               "#EC NEEDED
ENDCLASS.                    "lcl_handle_events DEFINITION


*=======================================================================
AT SELECTION-SCREEN OUTPUT.
*=======================================================================
  LOOP AT SCREEN.
    IF p_seltr = abap_true.
      IF ( screen-group1 = 'SEL' ) OR
         ( screen-group1 = 'LAY' ) OR
         ( screen-group1 = 'SAV' ).
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.

    ELSEIF p_selpk = abap_true.
      IF ( screen-group1 = 'LAY' ) OR
         ( screen-group1 = 'SAV' ).
*       screen-input = '0'.
*       MODIFY SCREEN.
      ENDIF.

      IF ( screen-group1 = 'CLA' AND p_ckclas = ' ' ) OR
         ( screen-group1 = 'FUG' AND p_ckfugr = ' ' ) OR
         ( screen-group1 = 'PRO' AND p_ckprog = ' ' ).
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'CMP'.
      IF p_save = abap_true.
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

* Set default values
  IF p_save = abap_true.
    p_cmpr_n = p_cmpr_o = abap_false.
    p_cmpr_p = abap_true.
  ENDIF.
  IF p_seltr = abap_true.
    p_ckclas = p_ckfugr = p_ckprog = abap_true.
    CLEAR:
      s_swcomp[],
      s_pack[],
      s_srcsys[],
      s_author[],
      s_clas[],
      s_fugr[],
      s_prog[].
  ENDIF.

*=======================================================================
START-OF-SELECTION.
*=======================================================================
  PERFORM determine_packages.

  PERFORM select_classes.
  PERFORM select_fugr.
  PERFORM select_prog.


*=======================================================================
END-OF-SELECTION.
*=======================================================================
  PERFORM process_source.
  PERFORM compress_list.
  PERFORM list_get.
  PERFORM list_store.
  PERFORM list_display.


*=======================================================================
FORM determine_packages.
*=======================================================================

  DATA:
    lv_package    TYPE tadir-devclass.

  IF p_selpk = abap_true.
    CALL METHOD zcl_tool_rs_service=>get_package_list
      EXPORTING
        it_so_pack            = s_pack[]
        it_so_swcomp          = s_swcomp[]
        iv_include_subpackage = p_subpk
      IMPORTING
        et_package            = gt_package.
  ELSEIF p_seltr = abap_true.
    CALL METHOD zcl_tool_rs_service=>get_package_list
      EXPORTING
        it_so_devlayer = s_devlay[]
      IMPORTING
        et_package     = gt_package.
  ELSE.
    RETURN.
  ENDIF.

  IF gt_package IS INITIAL.
    gv_no_package = abap_true.
  ENDIF.

ENDFORM.                    "determine_packages


*=======================================================================
FORM list_get.
*=======================================================================

* set color
  DATA:
    lt_color TYPE lvc_t_scol,
    ls_color TYPE lvc_s_scol.

  FIELD-SYMBOLS:
    <ls_list> LIKE LINE OF gt_list.

  LOOP AT gt_list ASSIGNING <ls_list>.
    CLEAR lt_color.

    IF <ls_list>-obj_gentype IS NOT INITIAL.
*     generated object: gray
      CLEAR ls_color.
      ls_color-color-col = col_normal.
      ls_color-color-int = 0.
      ls_color-color-inv = 1.
      APPEND ls_color TO lt_color.
    ENDIF.

    <ls_list>-t_color = lt_color.
  ENDLOOP.

ENDFORM.                    "list_get

*=======================================================================
FORM list_store.
*=======================================================================

  DATA:
    lo_exc     TYPE REF TO cx_uuid_error,
    lt_db_list TYPE STANDARD TABLE OF ztool_metrics_lo,
    ls_db_list LIKE LINE OF lt_db_list,
    lv_uuid    TYPE raw16,"crmt_object_guid,
    lv_msg     TYPE string.

  FIELD-SYMBOLS:
    <ls_list> LIKE LINE OF gt_list.

* PRECONDITIONS
  CHECK p_save = abap_true.

* BODY
* Fill time and date before the loop (should be the same for all entries)
  ls_db_list-cr_name       = sy-uname.
  ls_db_list-cr_time       = sy-uzeit.
  ls_db_list-cr_date       = sy-datum.

  LOOP AT gt_list ASSIGNING <ls_list>.
    MOVE-CORRESPONDING <ls_list> TO ls_db_list.
    TRY.
        ls_db_list-id  = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
      CATCH cx_uuid_error INTO lo_exc.
        lv_msg = lo_exc->get_text( ).
        MESSAGE lv_msg TYPE 'X'.
    ENDTRY.
    ls_db_list-gentype       = <ls_list>-obj_gentype.
    ls_db_list-tot_lines     = <ls_list>-num_lines_total.
    ls_db_list-code_lines    = <ls_list>-num_lines_code.
    ls_db_list-comment_lines = <ls_list>-num_lines_comment.
    ls_db_list-blank_lines   = <ls_list>-num_lines_blank.
    APPEND ls_db_list TO lt_db_list.
  ENDLOOP.

  INSERT ztool_metrics_lo FROM TABLE lt_db_list.
  IF sy-subrc <> 0.
    lv_msg = text-001.
    MESSAGE lv_msg TYPE 'E'.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.


ENDFORM.
*=======================================================================
FORM list_display.
*=======================================================================

  DATA:
    lo_salv_table TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = sy-batch
        IMPORTING
          r_salv_table = lo_salv_table
        CHANGING
          t_table      = gt_list
      ).
    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

* activate ALV generic Functions
  DATA:
    lo_functions TYPE REF TO cl_salv_functions_list.

  lo_functions = lo_salv_table->get_functions( ).
  lo_functions->set_all( abap_true ).

* activate saving of display variants
  DATA:
    lo_layout     TYPE REF TO cl_salv_layout,
    ls_layout_key TYPE salv_s_layout_key.

  lo_layout = lo_salv_table->get_layout( ).

  ls_layout_key-report = sy-repid.
  lo_layout->set_key( ls_layout_key ).

  lo_layout->set_default( abap_false ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

* adapt field catalog
  DATA:
    lo_columns    TYPE REF TO cl_salv_columns_table,
    lo_column     TYPE REF TO cl_salv_column_table,
    lt_column_ref TYPE salv_t_column_ref,
    ls_column_ref TYPE salv_s_column_ref.

  lo_columns    = lo_salv_table->get_columns( ).
  lt_column_ref = lo_columns->get( ).

* set visibility
  LOOP AT lt_column_ref INTO ls_column_ref.
    CASE ls_column_ref-columnname.

      WHEN 'INCLNAME'.
        ls_column_ref-r_column->set_visible( if_salv_c_bool_sap=>false ).

    ENDCASE.
  ENDLOOP.

  LOOP AT lt_column_ref INTO ls_column_ref.
    lo_column ?= ls_column_ref-r_column.
    CASE ls_column_ref-columnname.

      WHEN 'NUM_LINES_TOTAL'.
        lo_column->set_short_text( space ).
        lo_column->set_medium_text( space ).
        lo_column->set_long_text( text-c00 ).
        lo_column->set_tooltip( text-c00 ).

      WHEN 'NUM_LINES_CODE'.
        lo_column->set_short_text( space ).
        lo_column->set_medium_text( space ).
        lo_column->set_long_text( text-c01 ).
        lo_column->set_tooltip( text-c01 ).

      WHEN 'NUM_LINES_COMMENT'.
        lo_column->set_short_text( space ).
        lo_column->set_medium_text( space ).
        lo_column->set_long_text( text-c02 ).
        lo_column->set_tooltip( text-c02 ).

      WHEN 'NUM_LINES_BLANK'.
        lo_column->set_short_text( space ).
        lo_column->set_medium_text( space ).
        lo_column->set_long_text( text-c03 ).
        lo_column->set_tooltip( text-c03 ).

      WHEN 'SUBOBJ'.
        lo_column->set_short_text( space ).
        lo_column->set_medium_text( space ).
        lo_column->set_long_text( text-c04 ).
        lo_column->set_tooltip( text-c04 ).

    ENDCASE.
  ENDLOOP.

  TRY.
      lo_columns->set_count_column( 'COUNTER' ).
      lo_columns->set_color_column( 'T_COLOR' ).
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
  ENDTRY.

  lo_columns->set_key_fixation( abap_true ).
  lo_columns->set_optimize( abap_true ).

* set sorted columns
  DATA:
    lo_sort TYPE REF TO cl_salv_sorts.

  lo_sort = lo_salv_table->get_sorts( ).

  TRY.
      lo_sort->add_sort( columnname = 'OBJ_GENTYPE' subtotal = abap_true ).
      lo_sort->add_sort( columnname = 'DEVCLASS'    subtotal = abap_true ).
      lo_sort->add_sort( columnname = 'OBJECT' ).
      lo_sort->add_sort( columnname = 'OBJNAME' ).
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
    CATCH cx_salv_existing.                             "#EC NO_HANDLER
  ENDTRY.

* set aggregation
  DATA:
    lo_aggregations TYPE REF TO cl_salv_aggregations.

  lo_aggregations = lo_salv_table->get_aggregations( ).

  TRY.
      lo_aggregations->add_aggregation(
          columnname  = 'NUM_LINES_TOTAL'
          aggregation = if_salv_c_aggregation=>total
      ).
      lo_aggregations->add_aggregation(
          columnname  = 'NUM_LINES_CODE'
          aggregation = if_salv_c_aggregation=>total
      ).
      lo_aggregations->add_aggregation(
          columnname  = 'NUM_LINES_COMMENT'
          aggregation = if_salv_c_aggregation=>total
      ).
      lo_aggregations->add_aggregation(
          columnname  = 'NUM_LINES_BLANK'
          aggregation = if_salv_c_aggregation=>total
      ).
    CATCH cx_salv_data_error.                           "#EC NO_HANDLER
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
    CATCH cx_salv_existing.                             "#EC NO_HANDLER
  ENDTRY.

* register to the events of cl_salv_table
  DATA:
    lo_events TYPE REF TO cl_salv_events_table.

  lo_events = lo_salv_table->get_event( ).

  SET HANDLER lcl_handle_events=>on_added_function FOR lo_events.
  SET HANDLER lcl_handle_events=>on_double_click   FOR lo_events.
  SET HANDLER lcl_handle_events=>on_link_click     FOR lo_events.

  go_salv_table = lo_salv_table.

* display the table
  lo_salv_table->display( ).

ENDFORM.                    "list_display


*=======================================================================
FORM select_classes.
*=======================================================================

  DATA:
    ls_objkey LIKE LINE OF gt_objkey.

  DATA:
    BEGIN OF ls_include,
      inclname TYPE progname,
      alias    TYPE string,
    END OF ls_include ,
    lt_include      LIKE STANDARD TABLE OF ls_include,
    lt_meth_include TYPE seop_methods_w_include,
    lo_naming       TYPE REF TO if_oo_clif_incl_naming,
    lo_class_naming TYPE REF TO if_oo_class_incl_naming,
    lt_tadir_obj    TYPE TABLE OF seoclsname,
    lt_clif         TYPE TABLE OF seoclsname,
    lt_clifdf       TYPE TABLE OF seoclassdf,
    ls_clifdf       TYPE seoclassdf,
    lv_tadir_obj    LIKE LINE OF lt_tadir_obj,
    lt_source       TYPE zcl_tool_rs_service=>ty_mtx_source.


  FIELD-SYMBOLS:
    <ls_meth_include> LIKE LINE OF lt_meth_include,
    <ls_source>       LIKE LINE OF lt_source.

* PRECONDITION
  CHECK p_ckclas = abap_true.
  CHECK gv_no_package = abap_false.

* BODY
* get objects
  SELECT     pgmid object obj_name
    APPENDING TABLE gt_objkey
    FROM     tadir
    FOR ALL ENTRIES IN gt_package
    WHERE    ( pgmid    =  'R3TR' )
      AND    ( object   =  'CLAS' )
      AND    ( obj_name IN s_clas )                     "#EC CI_GENBUFF
      AND    ( devclass = gt_package-table_line )     "#EC CI_SGLSELECT
      AND    ( author   IN s_author ).

ENDFORM.                    "process_classes


*=======================================================================
FORM select_fugr.
*=======================================================================
  DATA:
    lt_source    TYPE zcl_tool_rs_service=>ty_mtx_source,
    lt_inclname  TYPE prognames,
    lt_tadir_obj TYPE TABLE OF tlibg-area,
    lt_area      TYPE TABLE OF tlibg-area,
    lv_area      TYPE tlibg-area,
    lv_prog      TYPE progname,
    lv_inclname  TYPE progname,
    lv_count     TYPE i,
    lv_tabix     TYPE i.

  FIELD-SYMBOLS:
    <ls_source>   LIKE LINE OF lt_source.

* PRECONDITION
  CHECK p_ckfugr = abap_true.
  CHECK gv_no_package = abap_false.

* BODY
* get objects
*   consider package select-options
  SELECT     pgmid object obj_name
    APPENDING TABLE gt_objkey
    FROM     tadir
    FOR ALL ENTRIES IN gt_package
    WHERE    ( pgmid    =  'R3TR' )
      AND    ( object   =  'FUGR' )
      AND    ( obj_name IN s_fugr )                     "#EC CI_GENBUFF
      AND    ( devclass =  gt_package-table_line )    "#EC CI_SGLSELECT
      AND    ( author   IN s_author ).

ENDFORM.                    "process_fugr


*=======================================================================
FORM select_prog.
*=======================================================================
  DATA:
    lt_source    TYPE zcl_tool_rs_service=>ty_mtx_source,
    lt_tadir_obj TYPE TABLE OF tadir-obj_name,
    lt_prog      TYPE TABLE OF progname,
    lv_prog      TYPE progname,
    lv_count     TYPE i,
    lv_tabix     TYPE i.

  FIELD-SYMBOLS:
    <ls_source>   LIKE LINE OF lt_source.

* PRECONDITION
  CHECK p_ckprog = abap_true.
  CHECK gv_no_package = abap_false.

* BODY
* get objects
*   consider package select-options
  SELECT     pgmid object obj_name
    APPENDING TABLE gt_objkey
    FROM     tadir
    FOR ALL ENTRIES IN gt_package
    WHERE    ( pgmid    =  'R3TR' )
      AND    ( object   =  'PROG' )
      AND    ( obj_name IN s_prog )                     "#EC CI_GENBUFF
      AND    ( devclass =  gt_package-table_line )    "#EC CI_SGLSELECT
      AND    ( author   IN s_author ).


ENDFORM.                    "process_prog


*=======================================================================
FORM process_source.
*=======================================================================

  CREATE OBJECT go_src_code_analysis TYPE ('ZCL_TOOL_SRC_CODE_LOCATION').

  CREATE OBJECT go_src_code_analyzer
    EXPORTING
      io_analysis = go_src_code_analysis
      it_objkey   = gt_objkey.

  CALL METHOD go_src_code_analyzer->get_result
    IMPORTING
      et_table = gt_list.


ENDFORM.                    "process_source


*=======================================================================
FORM compress_list.
*=======================================================================

  DATA:
    lt_sum_list TYPE ty_gt_list,
    ls_sum_list LIKE LINE OF lt_sum_list,
    ls_list     LIKE LINE OF gt_list,
    ld_index    TYPE sy-tabix.


  FIELD-SYMBOLS:
    <ls_list> LIKE LINE OF gt_list.

* proceed only if compression is requested.
  CHECK p_cmpr_n = abap_false.

  SORT gt_list BY obj_gentype devclass object objname.

  READ TABLE gt_list INTO ls_sum_list INDEX 1.
  CLEAR:
    ls_sum_list-num_lines_total,
    ls_sum_list-num_lines_comment,
    ls_sum_list-num_lines_blank,
    ls_sum_list-num_lines_code.

  LOOP AT gt_list INTO ls_list.
    ADD:
      ls_list-num_lines_total   TO ls_sum_list-num_lines_total,
      ls_list-num_lines_comment TO ls_sum_list-num_lines_comment,
      ls_list-num_lines_blank   TO ls_sum_list-num_lines_blank,
      ls_list-num_lines_code    TO ls_sum_list-num_lines_code.

    IF p_cmpr_o = abap_true.
      AT END OF  objname.
        ld_index = sy-tabix + 1.
        CLEAR ls_sum_list-subobj.
        APPEND ls_sum_list TO lt_sum_list.
        CLEAR ls_sum_list.
        READ TABLE gt_list INTO ls_sum_list INDEX ld_index.
        CLEAR:
          ls_sum_list-num_lines_total,
          ls_sum_list-num_lines_comment,
          ls_sum_list-num_lines_blank,
          ls_sum_list-num_lines_code.
      ENDAT.
    ELSEIF p_cmpr_p = abap_true.
      AT END OF devclass.
        ld_index = sy-tabix + 1.
        CLEAR:
          ls_sum_list-objname,
          ls_sum_list-object,
          ls_sum_list-subobj,
          ls_sum_list-author.
        APPEND ls_sum_list TO lt_sum_list.
        CLEAR ls_sum_list.
        READ TABLE gt_list INTO ls_sum_list INDEX ld_index.
        CLEAR:
          ls_sum_list-num_lines_total,
          ls_sum_list-num_lines_comment,
          ls_sum_list-num_lines_blank,
          ls_sum_list-num_lines_code.
      ENDAT.
    ENDIF.
  ENDLOOP.

  gt_list[] = lt_sum_list[].

ENDFORM.                    "compress_list
*=======================================================================
CLASS lcl_handle_events IMPLEMENTATION.
*=======================================================================
  METHOD on_added_function.

*   not needed yet

  ENDMETHOD.                    "on_added_function

  METHOD on_double_click.

    DATA:
      ls_list        LIKE LINE OF gt_list,
      lv_object_name TYPE tadir-obj_name,
      lv_object_type TYPE tadir-object.

*   get select object
    READ TABLE gt_list INDEX row INTO ls_list.
    CHECK sy-subrc = 0.

    lv_object_name = ls_list-objname.
    lv_object_type = ls_list-object.

*   display object with appropriate tool
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_name = lv_object_name
        object_type = lv_object_type
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              DISPLAY LIKE sy-msgty
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "on_double_click

  METHOD on_link_click.

    on_double_click(
        row    = row
        column = column
    ).

  ENDMETHOD.                    "on_link_click

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION