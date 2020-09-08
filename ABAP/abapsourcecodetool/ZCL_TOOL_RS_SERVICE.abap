class ZCL_TOOL_RS_SERVICE definition
  public
  final
  create private .

public section.
  type-pools ABAP .
  type-pools SEOP .

  types:
    ty_mt_so_devlayer TYPE RANGE OF tdevc-pdevclass .
  types:
    ty_mt_so_author TYPE RANGE OF tadir-author .
  types:
    ty_mt_so_srcsys TYPE RANGE OF tadir-srcsystem .
  types:
    ty_mt_so_clsname TYPE RANGE OF seoclassdf-clsname .
  types:
    ty_mt_tadir  TYPE STANDARD TABLE OF tadir .
  types:
    ty_mt_so_fugrname TYPE RANGE OF tlibg-area .
  types:
    ty_mt_so_progname TYPE RANGE OF trdir-name .
  types:
    BEGIN OF ty_msx_source,
        subobj TYPE tadir-obj_name,
        t_src  TYPE rswsourcet,
      END OF ty_msx_source .
  types:
    ty_mtx_source TYPE STANDARD TABLE OF ty_msx_source .
  types:
    ty_mt_so_pack  TYPE RANGE OF tadir-devclass .
  types:
    ty_mt_so_swcomp TYPE RANGE OF tdevc-dlvunit .
  types:
    ty_mt_pack   TYPE STANDARD TABLE OF tadir-devclass .

  class-methods GET_FUGR_INCLUDES
    importing
      !IV_OBJNAME type CSEQUENCE
    exporting
      !ET_INCLUDE type PROGNAMES .
  class-methods GET_PACKAGE_LIST
    importing
      !IT_SO_PACK type TY_MT_SO_PACK optional
      !IT_SO_SWCOMP type TY_MT_SO_SWCOMP optional
      !IT_SO_DEVLAYER type TY_MT_SO_DEVLAYER optional
      !IV_INCLUDE_SUBPACKAGE type ABAP_BOOL default ABAP_FALSE
    exporting
      !ET_PACKAGE type TY_MT_PACK .
  class-methods GET_SOURCE
    importing
      !IV_OBJTYPE type CSEQUENCE
      !IV_OBJNAME type CSEQUENCE
    exporting
      !ET_SOURCE type TY_MTX_SOURCE .
  class-methods GET_TADIR_LIST
    importing
      !IV_SELECT_CLAS type ABAP_BOOL default ABAP_FALSE
      !IT_SO_CLSNAME type TY_MT_SO_CLSNAME optional
      !IV_SELECT_FUGR type ABAP_BOOL default ABAP_FALSE
      !IT_SO_FUGRNAME type TY_MT_SO_FUGRNAME optional
      !IV_SELECT_PROG type ABAP_BOOL default ABAP_FALSE
      !IT_SO_PROGNAME type TY_MT_SO_PROGNAME optional
      !IT_SO_PACK type TY_MT_SO_PACK optional
      !IT_SO_AUTHOR type TY_MT_SO_AUTHOR optional
      !IT_SO_SRCSYSTEM type TY_MT_SO_SRCSYS optional
    exporting
      !ET_TADIR type TY_MT_TADIR .
*"* protected components of class ZCL_TOOL_RS_SERVICES
*"* do not include other source files here!!!
protected section.
private section.

  class-methods GET_SOURCE_CLIF
    importing
      !IV_OBJNAME type CSEQUENCE
    exporting
      !ET_SOURCE type TY_MTX_SOURCE .
  class-methods GET_SOURCE_FUGR
    importing
      !IV_OBJNAME type CSEQUENCE
    exporting
      !ET_SOURCE type TY_MTX_SOURCE .
  class-methods GET_SOURCE_PROG
    importing
      !IV_OBJNAME type CSEQUENCE
    exporting
      !ET_SOURCE type TY_MTX_SOURCE .
ENDCLASS.



CLASS ZCL_TOOL_RS_SERVICE IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TOOL_RS_SERVICE=>GET_FUGR_INCLUDES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNAME                     TYPE        CSEQUENCE
* | [<---] ET_INCLUDE                     TYPE        PROGNAMES
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_FUGR_INCLUDES.

  DATA:
    lt_so_prog      TYPE RANGE OF d010sinf-prog,
    ls_so_prog      LIKE LINE OF lt_so_prog,
    lv_namespace    TYPE string,
    lv_objname      TYPE string.

* INIT RESULTS
  CLEAR et_include[].

* PRECONDITIONS
  CHECK iv_objname IS NOT INITIAL.

* BODY
* Determine namespace
  IF iv_objname(1) = '/'.
    lv_namespace = substring_to( val   = iv_objname
                                 regex = '/'
                                 occ   = 2           ).
    lv_objname   = substring_after( val   = iv_objname
                                    regex = '/'
                                    occ   = 2           ).
  ELSE.
    lv_objname = iv_objname.
    CLEAR lv_namespace.
  ENDIF.



* Include "....Xxx" files
  ls_so_prog-sign   = 'I'.
  ls_so_prog-option = 'CP'.
  CONCATENATE lv_namespace 'L' lv_objname '+++'
              INTO ls_so_prog-low.
  APPEND ls_so_prog TO lt_so_prog.

* Exclude "....$xx" files
  ls_so_prog-sign   = 'E'.
  ls_so_prog-option = 'CP'.
  CONCATENATE lv_namespace 'L' lv_objname '$++'
              INTO ls_so_prog-low.
  APPEND ls_so_prog TO lt_so_prog.

  SELECT prog
    FROM d010sinf
    INTO TABLE et_include
    WHERE prog    IN lt_so_prog
      AND r3state  = 'A'.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TOOL_RS_SERVICE=>GET_PACKAGE_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SO_PACK                     TYPE        TY_MT_SO_PACK(optional)
* | [--->] IT_SO_SWCOMP                   TYPE        TY_MT_SO_SWCOMP(optional)
* | [--->] IT_SO_DEVLAYER                 TYPE        TY_MT_SO_DEVLAYER(optional)
* | [--->] IV_INCLUDE_SUBPACKAGE          TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<---] ET_PACKAGE                     TYPE        TY_MT_PACK
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_PACKAGE_LIST.

  DATA:
    lt_package     TYPE STANDARD TABLE OF devclass,
    lv_package     TYPE devclass,
    lt_so_pack     TYPE RANGE OF devclass,
    lt_so_pack_aux TYPE RANGE OF devclass,
    ls_so_pack     LIKE LINE OF lt_so_pack.


* INIT RESULTS
  CLEAR et_package.

  IF ( it_so_pack     IS NOT INITIAL ) OR
     ( it_so_swcomp   IS NOT INITIAL ) OR
     ( it_so_devlayer IS NOT INITIAL ).
    SELECT     devclass                                  "#EC CI_GENBUFF
      INTO     TABLE et_package
      FROM     tdevc
      WHERE    devclass  IN it_so_pack
        AND    dlvunit   IN it_so_swcomp
        AND    pdevclass IN it_so_devlayer.
  ENDIF.

  IF et_package IS INITIAL.
    RETURN.
  ENDIF.

* Get embedded packages recursively
  IF iv_include_subpackage = abap_true.
    IF lt_package IS NOT INITIAL.
      CLEAR lt_so_pack.
      LOOP AT lt_package INTO lv_package.
        ls_so_pack-sign   = 'I'.
        ls_so_pack-option = 'EQ'.
        ls_so_pack-low    = lv_package.
        ls_so_pack-high   = ' '.
        APPEND ls_so_pack TO lt_so_pack.
      ENDLOOP.
    ENDIF.

    IF lt_so_pack[] IS NOT INITIAL.
      lt_so_pack_aux[] = lt_so_pack[].
      WHILE lt_so_pack_aux[] IS NOT INITIAL.

        CLEAR lt_package.
        SELECT devclass                                  "#EC CI_GENBUFF
          INTO TABLE lt_package
          FROM tdevc
          WHERE parentcl IN lt_so_pack_aux.

        CLEAR lt_so_pack_aux[].
        LOOP AT lt_package INTO lv_package.
          ls_so_pack-sign   = 'I'.
          ls_so_pack-option = 'EQ'.
          ls_so_pack-low    = lv_package.
          ls_so_pack-high   = ' '.
          APPEND ls_so_pack TO lt_so_pack.
          APPEND ls_so_pack TO lt_so_pack_aux.
        ENDLOOP.
        APPEND LINES OF lt_package TO et_package.
      ENDWHILE.
    ENDIF.
  ENDIF.

* SET RESULTS
  SORT et_package.
  DELETE ADJACENT DUPLICATES FROM et_package.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TOOL_RS_SERVICE=>GET_SOURCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJTYPE                     TYPE        CSEQUENCE
* | [--->] IV_OBJNAME                     TYPE        CSEQUENCE
* | [<---] ET_SOURCE                      TYPE        TY_MTX_SOURCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_SOURCE.

* INIT RESULTS
  CLEAR et_source.

* BODY
  CASE iv_objtype.
    WHEN 'CLAS'.
*     class
      CALL METHOD get_source_clif
        EXPORTING
          iv_objname = iv_objname
        IMPORTING
          et_source  = et_source.

    WHEN 'INTF'.
*     interface
      CALL METHOD get_source_clif
        EXPORTING
          iv_objname = iv_objname
        IMPORTING
          et_source  = et_source.

    WHEN 'FUGR'.
*     function group
      CALL METHOD get_source_fugr
        EXPORTING
          iv_objname = iv_objname
        IMPORTING
          et_source  = et_source.

    WHEN 'PROG'.
*     program
      CALL METHOD get_source_prog
        EXPORTING
          iv_objname = iv_objname
        IMPORTING
          et_source  = et_source.

    WHEN OTHERS.
      ASSERT 1 = 0.

  ENDCASE.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_TOOL_RS_SERVICE=>GET_SOURCE_CLIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNAME                     TYPE        CSEQUENCE
* | [<---] ET_SOURCE                      TYPE        TY_MTX_SOURCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_SOURCE_CLIF.

  DATA:
    BEGIN OF ls_include,
      inclname TYPE progname,
      alias    TYPE string,
    END OF ls_include ,
    lt_include     LIKE STANDARD TABLE OF ls_include,
    lt_meth_include TYPE seop_methods_w_include,
    lo_naming      TYPE REF TO if_oo_clif_incl_naming,
    lo_class_naming TYPE REF TO if_oo_class_incl_naming,
    lt_source      TYPE ty_mtx_source,
    ls_clifdf      TYPE seoclassdf.

  FIELD-SYMBOLS:
    <ls_meth_include> LIKE LINE OF lt_meth_include.


* BODY
* get object details. Only active classes
  SELECT SINGLE *
    INTO     ls_clifdf
    FROM     seoclassdf
    WHERE    clsname = iv_objname
      AND    version = '1'.
  CHECK sy-subrc = 0.

* get class-pool name
  CALL METHOD cl_oo_include_naming=>get_instance_by_name
    EXPORTING
      name   = ls_clifdf-clsname
    RECEIVING
      cifref = lo_naming
    EXCEPTIONS
      OTHERS = 0.

* process object
  TRY.
      lo_class_naming ?= lo_naming.
    CATCH cx_sy_move_cast_error.
      RETURN.
  ENDTRY.
  CLEAR lt_include[].
  ls_include-inclname = lo_class_naming->class_pool.
  ls_include-alias    = lo_class_naming->class_pool.
  APPEND ls_include TO lt_include.
  ls_include-inclname = lo_class_naming->public_section.
  ls_include-alias    = lo_class_naming->public_section.
  APPEND ls_include TO lt_include.
  ls_include-inclname = lo_class_naming->protected_section.
  ls_include-alias    = lo_class_naming->protected_section.
  APPEND ls_include TO lt_include.
  ls_include-inclname = lo_class_naming->private_section.
  ls_include-alias    = lo_class_naming->private_section.
  APPEND ls_include TO lt_include.
  ls_include-inclname = lo_class_naming->locals_old.
  ls_include-alias    = lo_class_naming->locals_old.
  APPEND ls_include TO lt_include.
  ls_include-inclname = lo_class_naming->locals_def.
  ls_include-alias    = lo_class_naming->locals_def.
  APPEND ls_include TO lt_include.
  ls_include-inclname = lo_class_naming->locals_imp.
  ls_include-alias    = lo_class_naming->locals_imp.
  APPEND ls_include TO lt_include.
  ls_include-inclname = lo_class_naming->macros.
  ls_include-alias    = lo_class_naming->macros.
  APPEND ls_include TO lt_include.

  CALL METHOD lo_class_naming->get_all_method_includes
    RECEIVING
      methods_w_include           = lt_meth_include
    EXCEPTIONS
      internal_class_not_existing = 1
      OTHERS                      = 2.
  IF sy-subrc = 0.
    LOOP AT lt_meth_include ASSIGNING <ls_meth_include>.
      ls_include-inclname = <ls_meth_include>-incname.
      ls_include-alias    = <ls_meth_include>-cpdkey.
      APPEND ls_include TO lt_include.
    ENDLOOP.
  ENDIF.

*       get source
  LOOP AT lt_include INTO ls_include.
    CLEAR lt_source[].
    CALL METHOD ZCL_TOOL_RS_SERVICE=>get_source
      EXPORTING
        iv_objtype = 'PROG'
        iv_objname = ls_include-inclname
      IMPORTING
        et_source  = lt_source.
    APPEND LINES OF lt_source TO et_source.
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_TOOL_RS_SERVICE=>GET_SOURCE_FUGR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNAME                     TYPE        CSEQUENCE
* | [<---] ET_SOURCE                      TYPE        TY_MTX_SOURCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_SOURCE_FUGR.

  DATA:
    lt_source       TYPE ZCL_TOOL_RS_SERVICE=>ty_mtx_source,
    lt_inclname    TYPE prognames,
    lt_area        TYPE TABLE OF tlibg-area,
    lv_area        TYPE tlibg-area,
    lv_inclname    TYPE progname.

  SELECT     area
    INTO     TABLE lt_area
    FROM     tlibg
    WHERE    area = iv_objname.                       "#EC CI_SGLSELECT

* process objects
  SORT lt_area.
  LOOP AT lt_area INTO lv_area.
    CALL METHOD ZCL_TOOL_RS_SERVICE=>get_fugr_includes
      EXPORTING
        iv_objname = lv_area
      IMPORTING
        et_include = lt_inclname.

*     get source
    LOOP AT lt_inclname INTO lv_inclname.
      CLEAR lt_source.
      CALL METHOD ZCL_TOOL_RS_SERVICE=>get_source
        EXPORTING
          iv_objtype = 'PROG'
          iv_objname = lv_inclname
        IMPORTING
          et_source  = lt_source.
      APPEND LINES OF lt_source TO et_source.
    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_TOOL_RS_SERVICE=>GET_SOURCE_PROG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJNAME                     TYPE        CSEQUENCE
* | [<---] ET_SOURCE                      TYPE        TY_MTX_SOURCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_SOURCE_PROG.

  DATA:
    ls_source   LIKE LINE OF et_source,
    lv_progname TYPE progname.

* BODY
  lv_progname = iv_objname.
  ls_source-subobj = iv_objname.
  READ REPORT lv_progname INTO ls_source-t_src.
  IF sy-subrc <> 0.
    return.
  ENDIF.
  APPEND ls_source TO et_source.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_TOOL_RS_SERVICE=>GET_TADIR_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SELECT_CLAS                 TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IT_SO_CLSNAME                  TYPE        TY_MT_SO_CLSNAME(optional)
* | [--->] IV_SELECT_FUGR                 TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IT_SO_FUGRNAME                 TYPE        TY_MT_SO_FUGRNAME(optional)
* | [--->] IV_SELECT_PROG                 TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IT_SO_PROGNAME                 TYPE        TY_MT_SO_PROGNAME(optional)
* | [--->] IT_SO_PACK                     TYPE        TY_MT_SO_PACK(optional)
* | [--->] IT_SO_AUTHOR                   TYPE        TY_MT_SO_AUTHOR(optional)
* | [--->] IT_SO_SRCSYSTEM                TYPE        TY_MT_SO_SRCSYS(optional)
* | [<---] ET_TADIR                       TYPE        TY_MT_TADIR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_TADIR_LIST.

* INIT RESULTS
  CLEAR et_tadir.

* PRECONDITIONS
  CHECK iv_select_clas = abap_true OR
        iv_select_fugr = abap_true OR
        iv_select_prog = abap_true.

* BODY
  IF iv_select_clas = abap_true.
    SELECT     *
      APPENDING TABLE et_tadir
      FROM     tadir
      WHERE    ( pgmid    =  'R3TR' )
        AND    ( object   =  'CLAS' )
        AND    ( obj_name IN it_so_clsname    )         "#EC CI_GENBUFF
        AND    ( devclass IN it_so_pack       )       "#EC CI_SGLSELECT
        AND    ( author   IN it_so_author     )
        AND    ( srcsystem IN it_so_srcsystem ).
  ENDIF.

  IF iv_select_fugr = abap_true.
    SELECT     *
      APPENDING TABLE et_tadir
      FROM     tadir
      WHERE    ( pgmid    =  'R3TR' )
        AND    ( object   =  'FUGR' )
        AND    ( obj_name IN it_so_fugrname   )         "#EC CI_GENBUFF
        AND    ( devclass IN it_so_pack       )       "#EC CI_SGLSELECT
        AND    ( author   IN it_so_author     )
        AND    ( srcsystem IN it_so_srcsystem ).
  ENDIF.

  IF iv_select_prog = abap_true.
    SELECT     *
      APPENDING TABLE et_tadir
      FROM     tadir
      WHERE    ( pgmid    =  'R3TR' )
        AND    ( object   =  'PROG' )
        AND    ( obj_name IN it_so_progname   )         "#EC CI_GENBUFF
        AND    ( devclass IN it_so_pack       )       "#EC CI_SGLSELECT
        AND    ( author   IN it_so_author     )
        AND    ( srcsystem IN it_so_srcsystem ).
  ENDIF.

ENDMETHOD.
ENDCLASS.