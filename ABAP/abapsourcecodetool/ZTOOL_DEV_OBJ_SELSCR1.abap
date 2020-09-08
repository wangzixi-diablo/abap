*&---------------------------------------------------------------------*
*& Include          ZTOOL_DEV_OBJ_SELSCR1
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include           ZTOOL_DEV_OBJ_SELSCR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  Include           TOOL_DEV_OBJ_SELSCR
*&---------------------------------------------------------------------*
* Select options for development objects
*=======================================================================
TABLES:   "for SELECTION-SCREEN only
*=======================================================================
  tdevc,
  tadir,
  tlibg,
  trdir,
  seoclassdf.

PARAMETERS:
  p_seltr TYPE xfeld RADIOBUTTON GROUP sel MODIF ID sel USER-COMMAND dummy.
SELECT-OPTIONS:
  s_devlay FOR tdevc-pdevclass MODIF ID lay.

SELECTION-SCREEN SKIP.

PARAMETERS:
  p_selpk TYPE xfeld RADIOBUTTON GROUP sel DEFAULT 'X' MODIF ID sel.
SELECT-OPTIONS:
  s_swcomp FOR tdevc-dlvunit  DEFAULT 'SAP_AP',
  s_pack   FOR tadir-devclass.
PARAMETERS p_subpk TYPE xfeld.
SELECT-OPTIONS:
  s_srcsys FOR tadir-srcsystem,
  s_author FOR tadir-author.

SELECTION-SCREEN SKIP.

PARAMETERS     p_ckclas TYPE xfeld DEFAULT 'X'  USER-COMMAND dummy.
SELECT-OPTIONS s_clas   FOR  seoclassdf-clsname MODIF ID cla.
SELECTION-SCREEN SKIP.

PARAMETERS     p_ckfugr TYPE xfeld DEFAULT 'X'  USER-COMMAND dummy.
SELECT-OPTIONS s_fugr   FOR  tlibg-area         MODIF ID fug.
SELECTION-SCREEN SKIP.

PARAMETERS     p_ckprog TYPE xfeld DEFAULT 'X'  USER-COMMAND dummy.
SELECT-OPTIONS s_prog   FOR  trdir-name         MODIF ID pro.