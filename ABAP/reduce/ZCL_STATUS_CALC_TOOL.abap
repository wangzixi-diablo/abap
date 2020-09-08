CLASS zcl_status_calc_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_status_result,
        obtyp TYPE crm_jsto-obtyp,
        stsma TYPE crm_jsto-stsma,
        count TYPE int4,
      END OF ty_status_result .
    TYPES:
      tt_status_result TYPE STANDARD TABLE OF ty_status_result WITH KEY obtyp stsma .
    TYPES:
      tt_raw_input TYPE STANDARD TABLE OF crm_jsto WITH KEY objnr .

    METHODS get_result
      RETURNING
        VALUE(rt_result) TYPE tt_status_result .
    METHODS add_result
      IMPORTING
        !is_result     TYPE ty_status_result
      RETURNING
        VALUE(ro_this) TYPE REF TO zcl_status_calc_tool .
    METHODS get_result_traditional_way
      IMPORTING
        !it_raw          TYPE tt_raw_input
      RETURNING
        VALUE(rt_result) TYPE tt_status_result .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_result TYPE tt_status_result .
ENDCLASS.



CLASS ZCL_STATUS_CALC_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STATUS_CALC_TOOL->ADD_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_RESULT                      TYPE        TY_STATUS_RESULT
* | [<-()] RO_THIS                        TYPE REF TO ZCL_STATUS_CALC_TOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_result.
    APPEND is_result TO me->mt_result.
    ro_this = me.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STATUS_CALC_TOOL->GET_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_RESULT                      TYPE        TT_STATUS_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_result.
    SORT me->mt_result BY count DESCENDING.
    rt_result = me->mt_result.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_STATUS_CALC_TOOL->GET_RESULT_TRADITIONAL_WAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_RAW                         TYPE        TT_RAW_INPUT
* | [<-()] RT_RESULT                      TYPE        TT_STATUS_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_result_traditional_way.
    LOOP AT it_raw ASSIGNING FIELD-SYMBOL(<group>) GROUP BY (
        obtyp = <group>-obtyp stsma = <group>-stsma
       size = GROUP SIZE index = GROUP INDEX )
                  ASCENDING REFERENCE INTO DATA(group_ref).

      DATA(ls_result) = VALUE ty_status_result( obtyp = group_ref->obtyp
                               stsma = group_ref->stsma
                               count = group_ref->size ).

      APPEND ls_result TO rt_result.
    ENDLOOP.

    SORT rt_result BY count DESCENDING.
  ENDMETHOD.
ENDCLASS.