FUNCTION ZCALL_EDITOR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(COMPONENT) TYPE  O2PAGDIR-APPLNAME
*"     REFERENCE(VIEW) TYPE  O2PAGDIR-PAGEKEY
*"     REFERENCE(STARTLINE) TYPE  INT4 DEFAULT 1
*"----------------------------------------------------------------------

com_value = component.
view_value = view.
gv_startline = startline.
CALL SCREEN 0101.

ENDFUNCTION.