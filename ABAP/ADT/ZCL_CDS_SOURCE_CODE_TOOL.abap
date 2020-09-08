class ZCL_CDS_SOURCE_CODE_TOOL definition
  public
  final
  create public .

public section.

  types:
    begin of ty_result,
         view_name type ddlname,
         package type devclass,
         number type int4,
      end of ty_result .
  types:
    tt_result TYPE TABLE OF ty_result with key view_name package .

  class-methods RUN
    importing
      !IV_PACKAGE type DEVCLASS .
protected section.
private section.

  class-data MT_RESULT type TT_RESULT .

  class-methods GET_PACKAGE_LIST
    importing
      !IV_PACKAGE type DEVCLASS
    returning
      value(RT_LIST) type SMUD_DEVCLASS_TAB .
  class-methods FILL_VIEW_NUMBER_PER_PACKAGE
    importing
      !IV_PACKAGE type DEVCLASS .
ENDCLASS.



CLASS ZCL_CDS_SOURCE_CODE_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CDS_SOURCE_CODE_TOOL=>FILL_VIEW_NUMBER_PER_PACKAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fill_view_number_per_package.
    DATA: lt_view TYPE TABLE OF ddlname.
    DATA: my_locator TYPE REF TO cl_abap_db_c_locator,
          lv_total TYPE int4.

    SELECT obj_name INTO TABLE lt_view FROM tadir WHERE pgmid = 'R3TR' AND
      object = 'DDLS' AND devclass = iv_package.
    CHECK sy-subrc = 0.
    LOOP AT lt_view ASSIGNING FIELD-SYMBOL(<view_name>).
      SELECT SINGLE source INTO @DATA(src) FROM ddddlsrc WHERE
           ddlname = @<view_name>.
      CHECK sy-subrc = 0.
      FIND ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN src  RESULTS DATA(lt).
      APPEND INITIAL LINE TO mt_result ASSIGNING FIELD-SYMBOL(<result>).
      <result> = VALUE #( view_name = <view_name> package = iv_package number = lines( lt ) + 1 ).
      lv_total = lv_total + <result>-number.
    ENDLOOP.

    APPEND INITIAL LINE TO mt_result ASSIGNING FIELD-SYMBOL(<total>).
    <total> = value #( number = lv_total package = 'Total:' ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CDS_SOURCE_CODE_TOOL=>GET_PACKAGE_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [<-()] RT_LIST                        TYPE        SMUD_DEVCLASS_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_package_list.
    SELECT SINGLE devclass FROM tdevc INTO @DATA(lv_valid) WHERE devclass = @iv_package.

    DATA: lt_parent LIKE rt_list.
    CHECK sy-subrc = 0.
    APPEND lv_valid TO rt_list.
    APPEND lv_valid TO lt_parent.
    DO.
      SELECT devclass INTO TABLE @DATA(lt_sub) FROM tdevc FOR ALL ENTRIES IN
         @lt_parent WHERE parentcl = @lt_parent-table_line.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      APPEND LINES OF lt_sub TO rt_list.
      lt_parent = lt_sub.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CDS_SOURCE_CODE_TOOL=>RUN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run.
    CLEAR: mt_result.
    DATA(lt_package) = get_package_list( iv_package ).
    LOOP AT lt_package ASSIGNING FIELD-SYMBOL(<package>).
      fill_view_number_per_package( <package> ).
    ENDLOOP.

    cl_demo_output=>display( mt_result ).
  ENDMETHOD.
ENDCLASS.