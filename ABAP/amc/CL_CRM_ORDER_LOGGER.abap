class CL_CRM_ORDER_LOGGER definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods SEND
    importing
      !IV_TEXT type STRING .
  class-methods LOG
    importing
      !IT_OPPORT_H type CRMT_OPPORT_H_COMT optional
      !IT_PRICING type CRMT_PRICING_COMT optional
      !IT_SHIPPING type CRMT_SHIPPING_COMT optional
      !IT_ACTIVITY_H type CRMT_ACTIVITY_H_COMT optional
      !IT_PARTNER type CRMT_PARTNER_COMT optional
      !CT_ORDERADM_H type CRMT_ORDERADM_H_COMT optional
      !CT_ORDERADM_I type CRMT_ORDERADM_I_COMT optional .
protected section.
private section.

  class-data SO_SENDER type ref to IF_AMC_MESSAGE_PRODUCER_TEXT .
  class-data ST_RESULT type STRING_TABLE .

  class-methods LOG_STRUCTURE
    importing
      !IS_DATA type ANY .
  class-methods LOG_TABLE
    importing
      !IT_TABLE type ANY TABLE .
ENDCLASS.



CLASS CL_CRM_ORDER_LOGGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_CRM_ORDER_LOGGER=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CLASS_CONSTRUCTOR.
    so_sender ?= cl_amc_channel_manager=>create_message_producer(
         i_application_id = 'ZORDERLOG'
         i_channel_id = '/order_log' ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_CRM_ORDER_LOGGER=>LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_OPPORT_H                    TYPE        CRMT_OPPORT_H_COMT(optional)
* | [--->] IT_PRICING                     TYPE        CRMT_PRICING_COMT(optional)
* | [--->] IT_SHIPPING                    TYPE        CRMT_SHIPPING_COMT(optional)
* | [--->] IT_ACTIVITY_H                  TYPE        CRMT_ACTIVITY_H_COMT(optional)
* | [--->] IT_PARTNER                     TYPE        CRMT_PARTNER_COMT(optional)
* | [--->] CT_ORDERADM_H                  TYPE        CRMT_ORDERADM_H_COMT(optional)
* | [--->] CT_ORDERADM_I                  TYPE        CRMT_ORDERADM_I_COMT(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method LOG.
    IF sy-uname <> 'WANGJER' AND sy-uname <> 'MATTHAEI'.
        RETURN.
    ENDIF.
    CLEAR: st_result.

    log_table( it_opport_h ).
    log_table( it_pricing ).
    log_table( it_shipping ).
    log_table( it_activity_h ).
    log_table( it_partner ).
    log_table( ct_orderadm_h ).
    log_table( ct_orderadm_i ).

    LOOP AT st_result ASSIGNING FIELD-SYMBOL(<text>).
       send( <text> ).
    ENDLOOP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_CRM_ORDER_LOGGER=>LOG_STRUCTURE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method LOG_STRUCTURE.
    DATA(lo_struct_descr) = CAST cl_abap_structdescr( cl_abap_datadescr=>describe_by_data( is_data ) ).
    DATA(lv_comp) = |Data type: { lo_struct_descr->get_relative_name( ) } changed by user: { sy-uname }|.
    APPEND lv_comp TO st_result.
    DATA(lt_comp) = lo_struct_descr->components.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE is_data TO FIELD-SYMBOL(<data>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      READ TABLE lt_comp ASSIGNING FIELD-SYMBOL(<comp>) INDEX sy-index.
      IF <data> IS NOT INITIAL.
         DATA(lv_color) = sy-index MOD 4.
         DATA(lv_print) = |Field: { <comp>-name }, value: { <data> } |.
         APPENd lv_print TO st_result.
      ENDIF.
    ENDDO.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_CRM_ORDER_LOGGER=>LOG_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TABLE                       TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method LOG_TABLE.
    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<item>).
       log_structure( <item> ).
    ENDLOOP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_CRM_ORDER_LOGGER=>SEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SEND.
      so_sender->send( i_message = iv_text ).
  endmethod.
ENDCLASS.