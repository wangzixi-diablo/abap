REPORT tool_bsp_code_search.

DATA: lv_page TYPE o2pagdir-applname,
      lt_page TYPE cl_bsp_code_tool=>tt_comp.
PARAMETERS: key TYPE string OBLIGATORY DEFAULT 'ActiveXObject' LOWER CASE.
SELECT-OPTIONS: comp  FOR lv_page OBLIGATORY DEFAULT 'CRM_OI_TEMPL_RT'.

START-OF-SELECTION.
  LOOP AT comp ASSIGNING FIELD-SYMBOL(<comp>).
    APPEND <comp>-low TO lt_page.
  ENDLOOP.

  EXPORT name = sy-repid TO MEMORY ID 'Name'.

  cl_bsp_code_tool=>search( it_comp = lt_page iv_key = key ).

FORM alv_user_command USING iv_ucomm     LIKE sy-ucomm
                             cs_selfield  TYPE slis_selfield.
  IF iv_ucomm CS 'E'.
    LEAVE TO SCREEN 0.
  ENDIF.

  cl_bsp_code_tool=>on_double_click( iv_index = cs_selfield-tabindex ).

ENDFORM.