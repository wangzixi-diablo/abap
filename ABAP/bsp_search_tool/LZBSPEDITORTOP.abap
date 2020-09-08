FUNCTION-POOL zbspeditor.                   "MESSAGE-ID ..

* INCLUDE LZBSPEDITORD...                    " Local class definition

DATA: go_page_container TYPE REF TO cl_gui_custom_container,
      go_editor         TYPE REF TO cl_gui_abapedit,
      gv_startline      TYPE int4,
      com_value         TYPE o2pagdir-applname,
      view_value        TYPE o2pagdir-pagekey.