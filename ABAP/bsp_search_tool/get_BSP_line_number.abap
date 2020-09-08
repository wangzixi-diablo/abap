*&---------------------------------------------------------------------*
*& Report ZCONTEXT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCONTEXT.
 TYPES:
      BEGIN OF ty_view_source,
        applname TYPE o2pagdir-applname,
        pagekey  TYPE o2pagdir-pagekey,
        source   TYPE o2pageline_table,
      END OF ty_view_source .
    TYPES:
      tt_view_source TYPE STANDARD TABLE OF ty_view_source WITH KEY
      applname pagekey .

    DATA: ls_pagecon_key TYPE o2pconkey,
      st_view_source TYPE tt_view_source,
      lv_total TYPE i value 0,
      lv_each type i,

START-OF-SELECTION.
  PERFORM MAIN.
FORM MAIN.


    SELECT applname pagekey FROM o2pagdir INTO CORRESPONDING FIELDS OF TABLE
       st_view_source WHERE applname = 'MYMAP'.

    ls_pagecon_key-objtype = 'PD'.
    ls_pagecon_key-version = 'A'.

    LOOP AT st_view_source ASSIGNING FIELD-SYMBOL(<line>).
      ls_pagecon_key-applname = <line>-applname.
      ls_pagecon_key-pagekey = <line>-pagekey.

      WRITE: / <LINE>-pagekey.
      IMPORT content    TO  <line>-source
         FROM DATABASE o2pagcon(tr) ID ls_pagecon_key
         ACCEPTING PADDING IGNORING CONVERSION ERRORS.

    PERFORM GET_CHAR_NUMBER USING <line>-source CHANGING lv_each.
    lv_total = lv_total + lv_each.


    ENDLOOP.

    WRITE: / 'total: ' , lv_total.
    ENDFORM.

    FORM GET_CHAR_NUMBER USING it_source type o2pageline_table CHANGING cv_size type i.
      CLEAR: cv_size.
      LOOP AT it_source ASSIGNING FIELD-SYMBOL(<source>).
         cv_size = cv_size + strlen( <source> ).
      ENDLOOP.

    endform.