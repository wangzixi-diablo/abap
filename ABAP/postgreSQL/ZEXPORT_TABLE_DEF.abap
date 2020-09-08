REPORT z.

PARAMETERS: name TYPE crmt_object_name OBLIGATORY DEFAULT 'COMM_PRODUCT'.
TYPES:
  BEGIN OF ty_clipdata,
    data TYPE c LENGTH 100,
  END   OF ty_clipdata .
TYPES:
  tt_formatted TYPE STANDARD TABLE OF ty_clipdata .


DATA: lt_export   TYPE tt_formatted,
      lv_line     TYPE ty_clipdata-data,
      lt_keyfield TYPE crmt_object_name_tab,
      ls_line     TYPE ty_clipdata,
      lv_ret      TYPE int4,
      lt_table    TYPE dd03ptab.

DEFINE append_line.
  ls_line = VALUE #( data = &1 ).
  APPEND ls_line TO lt_export.
END-OF-DEFINITION.

CALL FUNCTION 'DDIF_TABL_GET'
  EXPORTING
    name      = name
  TABLES
    dd03p_tab = lt_table.

DATA(lv_text) = |CREATE TABLE public.{ name } (|.
append_line lv_text.

LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<comp>).
  CHECK <comp>-fieldname <> '.INCLUDE'.
  IF <comp>-keyflag = 'X'.
    APPEND <comp>-fieldname TO lt_keyfield.
  ENDIF.
  CASE <comp>-inttype.
    WHEN 'C'.
      lv_text = |{ <comp>-fieldname } character varying( { <comp>-intlen / 2 }) | &
      | COLLATE pg_catalog."default" NOT NULL,|.
      append_line lv_text.
    WHEN 'P'.
      IF find( val = <comp>-domname sub = 'TSTMP' case = abap_true ) <> -1.
        lv_text = |"{ <comp>-fieldname }" timestamp(0) without time zone NOT NULL,|.
      ENDIF.
      append_line lv_text.
    WHEN 'X'.
      DATA(lv_new_length) = <comp>-intlen * 2.
      lv_text = |{ <comp>-fieldname } character varying( { lv_new_length }) | &
      | COLLATE pg_catalog."default" NOT NULL,|.
      append_line lv_text.
  ENDCASE.
ENDLOOP.
lv_text = concat_lines_of( table = lt_keyfield sep = ',' ).
lv_text = |CONSTRAINT { name }_pkey PRIMARY KEY({ lv_text })|.
append_line lv_text.
append_line ')'.
append_line 'WITH ('.
append_line 'OIDS = FALSE'.
append_line ')'.
append_line 'TABLESPACE pg_default;'.
lv_text = |ALTER TABLE public.{ name } OWNER to postgres;|.
append_line lv_text.
lv_text = |GRANT ALL ON TABLE public.{ name } TO postgres;|.
append_line lv_text.
lv_text = |COMMENT ON TABLE public.{ name } IS 'Copied from CRM';|.
append_line lv_text.

cl_gui_frontend_services=>clipboard_export(
  EXPORTING
      no_auth_check        = abap_true
      IMPORTING
        data                 = lt_export
      CHANGING
        rc                   = lv_ret
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
    ).

WRITE: / 'export to clipboard ok:', sy-subrc.