*&---------------------------------------------------------------------*
*& Report  ZCREATE_OPP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zcreate_opp.

PARAMETERS: opp_name TYPE string LOWER CASE OBLIGATORY DEFAULT 'Jerry Opp'.

zcl_odata_tool=>get_csrf_token_and_cookie( IMPORTING et_cookies = DATA(lt_cookie)
                                                     ev_token = DATA(lv_token)  ).

zcl_odata_tool=>create_opp( iv_name = opp_name iv_token = lv_token it_cookies = lt_cookie ).