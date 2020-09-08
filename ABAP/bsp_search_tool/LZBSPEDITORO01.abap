*----------------------------------------------------------------------*
***INCLUDE LZBSPEDITORO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  DATA: ls_source      TYPE o2pageline_table,
        ls_pagecon_key TYPE o2pconkey.

  IF go_page_container IS INITIAL.

    CREATE OBJECT go_page_container
      EXPORTING
        container_name = 'EDITOR'.

    CREATE OBJECT go_editor
      EXPORTING
        parent           = go_page_container
        max_number_chars = '255'.

    go_editor->set_source_type( type   = 'BSP' ).

    go_editor->set_tabbar_mode( tabbar_mode = cl_gui_abapedit=>false ).
    go_editor->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).

    go_editor->create_document( ).

    go_editor->set_first_visible_line( line = 0 ).

    go_editor->set_readonly_mode( readonly_mode = 1 ).

  ENDIF.

  ls_source = cl_bsp_code_tool=>get_source( iv_comp = com_value iv_view = view_value ).

  go_editor->set_text( table = ls_source  ).

  "SET PF-STATUS 'ZO2_PAGE'.

  go_editor->set_selection_pos_in_line( line  = gv_startline pos = 0 ).

ENDMODULE.