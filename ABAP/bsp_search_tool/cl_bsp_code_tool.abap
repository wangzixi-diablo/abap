CLASS cl_bsp_code_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_view_source,
        applname TYPE o2pagdir-applname,
        pagekey  TYPE o2pagdir-pagekey,
        source   TYPE o2pageline_table,
      END OF ty_view_source .
    TYPES:
      tt_view_source TYPE STANDARD TABLE OF ty_view_source WITH KEY
      applname pagekey .
    TYPES:
      tt_comp TYPE STANDARD TABLE OF o2pagdir-applname .

    CLASS-METHODS get_source
      IMPORTING
        !iv_comp      TYPE ty_view_source-applname
        !iv_view      TYPE ty_view_source-pagekey
      RETURNING
        VALUE(result) TYPE ty_view_source-source .
    CLASS-METHODS search
      IMPORTING
        !it_comp TYPE tt_comp
        !iv_key  TYPE string .
    CLASS-METHODS on_double_click
      IMPORTING
        !iv_index TYPE int4 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_hit,
        applname TYPE o2pagdir-applname,
        pagekey  TYPE o2pagdir-pagekey,
        line     TYPE int4,
      END OF ty_hit .
    TYPES:
      tt_hit TYPE STANDARD TABLE OF ty_hit WITH KEY  applname pagekey .

    CLASS-DATA st_view_source TYPE tt_view_source .
    CLASS-DATA st_hit TYPE tt_hit .
    CLASS-DATA sv_key TYPE string .
    CLASS-DATA st_fieldcat TYPE slis_t_fieldcat_alv .

    CLASS-METHODS init
      IMPORTING
        !it_comp TYPE tt_comp
        !iv_key  TYPE string .
    CLASS-METHODS search_internal .
    CLASS-METHODS display_hit .
ENDCLASS.



CLASS CL_BSP_CODE_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_BSP_CODE_TOOL=>DISPLAY_HIT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD: display_hit.
    DATA lv_col_pos  TYPE i.
    DATA ls_fieldcat TYPE slis_fieldcat_alv.

    lv_col_pos = 1.

    ls_fieldcat-col_pos      = lv_col_pos.
    ls_fieldcat-fieldname    = 'APPLNAME'.
    ls_fieldcat-key          = 'X'.
    APPEND ls_fieldcat TO st_fieldcat.
    ADD 1 TO lv_col_pos.

    CLEAR ls_fieldcat.
    ls_fieldcat-col_pos      = lv_col_pos.
    ls_fieldcat-fieldname    = 'PAGEKEY'.
    APPEND ls_fieldcat TO st_fieldcat.
    ADD 1 TO lv_col_pos.

    CLEAR ls_fieldcat.
    ls_fieldcat-col_pos      = lv_col_pos.
    ls_fieldcat-fieldname    = 'LINE'.
    ls_fieldcat-rollname     = 'INT4'.
    ls_fieldcat-reptext_ddic = 'Line number'.
    APPEND ls_fieldcat TO st_fieldcat.

    DATA: ls_layout TYPE slis_layout_alv.

    ls_layout-colwidth_optimize = 'X'.
    ls_layout-zebra             = 'X'.

    DATA: lt_sort TYPE slis_t_sortinfo_alv,
          lv_name TYPE sy-repid.

    DATA(ls_sort) = VALUE slis_sortinfo_alv( fieldname = 'APPLNAME' up = 'X' ).
    APPEND ls_sort TO lt_sort.

    IMPORT name = lv_name FROM MEMORY ID 'Name'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = lv_name
        i_callback_pf_status_set = 'ALV_SET_STATUS'
        i_callback_user_command  = 'ALV_USER_COMMAND'
        is_layout                = ls_layout
        it_fieldcat              = st_fieldcat
        it_sort                  = lt_sort
        i_grid_title             = 'BSP Source code search tool'
      TABLES
        t_outtab                 = st_hit
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_BSP_CODE_TOOL=>GET_SOURCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_COMP                        TYPE        TY_VIEW_SOURCE-APPLNAME
* | [--->] IV_VIEW                        TYPE        TY_VIEW_SOURCE-PAGEKEY
* | [<-()] RESULT                         TYPE        TY_VIEW_SOURCE-SOURCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD: get_source.
    READ TABLE st_view_source ASSIGNING FIELD-SYMBOL(<result>)
      WITH KEY applname = iv_comp pagekey = iv_view.
    CHECK sy-subrc = 0.

    result = <result>-source.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_BSP_CODE_TOOL=>INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_COMP                        TYPE        TT_COMP
* | [--->] IV_KEY                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD: init.
    DATA: ls_pagecon_key TYPE o2pconkey.
    SELECT applname pagekey FROM o2pagdir INTO CORRESPONDING FIELDS OF TABLE
       st_view_source FOR ALL ENTRIES IN it_comp WHERE applname = it_comp-table_line
         AND pagetype = 'V'.

    ls_pagecon_key-objtype = 'PD'.
    ls_pagecon_key-version = 'A'.

    LOOP AT st_view_source ASSIGNING FIELD-SYMBOL(<line>).
      ls_pagecon_key-applname = <line>-applname.
      ls_pagecon_key-pagekey = <line>-pagekey.
      IMPORT content    TO  <line>-source
         FROM DATABASE o2pagcon(tr) ID ls_pagecon_key
         ACCEPTING PADDING IGNORING CONVERSION ERRORS.
    ENDLOOP.

    sv_key = iv_key.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_BSP_CODE_TOOL=>ON_DOUBLE_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INDEX                       TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD: on_double_click.
    READ TABLE st_hit ASSIGNING FIELD-SYMBOL(<result>) INDEX iv_index.
    CHECK sy-subrc = 0.

    CALL FUNCTION 'ZCALL_EDITOR'
      EXPORTING
        component = <result>-applname
        view      = <result>-pagekey
        startline = <result>-line.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_BSP_CODE_TOOL=>SEARCH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_COMP                        TYPE        TT_COMP
* | [--->] IV_KEY                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD: search.
    init( it_comp = it_comp iv_key = iv_key ).
    search_internal( ).
    display_hit( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_BSP_CODE_TOOL=>SEARCH_INTERNAL
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD: search_internal.
    DATA: ls_hit  TYPE ty_hit,
          lv_line TYPE line.
    LOOP AT st_view_source ASSIGNING FIELD-SYMBOL(<comp>).
      lv_line = 1.
      LOOP AT <comp>-source ASSIGNING FIELD-SYMBOL(<line>).
        FIND FIRST OCCURRENCE OF sv_key IN <line>-line IGNORING CASE.
        IF sy-subrc = 0.
          ls_hit = VALUE #( applname = <comp>-applname
                            pagekey = <comp>-pagekey
                            line = lv_line ).
          APPEND ls_hit TO st_hit.
        ENDIF.
        ADD 1 TO lv_line.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.