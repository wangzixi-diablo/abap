class CL_CRM_ORDER_2_PDF definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  class-methods OPEN_PDF
    importing
      !IO_COL_WRAPPER type ref to CL_BSP_WD_COLLECTION_WRAPPER
      !IO_WINDOW_MANAGER type ref to IF_BSP_WD_WINDOW_MANAGER .
protected section.
private section.

  methods GET_OUTPUT_DATA
    importing
      !IV_UUID type STRING
    exporting
      !FPCONTENT type XSTRING .
  methods GET_GUID_TAB
    importing
      !IV_GUID type STRING
    returning
      value(RT_GUID_TAB) type CRMT_OBJECT_GUID_TAB .
ENDCLASS.



CLASS CL_CRM_ORDER_2_PDF IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method CL_CRM_ORDER_2_PDF->GET_GUID_TAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_GUID                        TYPE        STRING
* | [<-()] RT_GUID_TAB                    TYPE        CRMT_OBJECT_GUID_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_GUID_TAB.
    DATA: lt_result TYPE string_table.
    SPLIT iv_guid AT ',' INTO Table lt_result.

    DELETE lt_result INDEX 1.
    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<guid>).
       APPEND <guid> TO rt_guid_Tab.
    ENDLOOP.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method CL_CRM_ORDER_2_PDF->GET_OUTPUT_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UUID                        TYPE        STRING
* | [<---] FPCONTENT                      TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_output_data.
    DATA:ls_pdf_file TYPE fpformoutput,
         ls_input    TYPE crms_order_pdf,
         lv_fm_name  TYPE rs38l_fnam.

    DATA(ls_outputparams) = VALUE sfpoutputparams( noprint = 'X' nopributt = 'X' noarchive = 'X'
                                                    nodialog  = 'X' preview   = 'X' getpdf    = 'X' ).

    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.

    ASSERT sy-subrc = 0.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = 'PF_CRM_ORDER_LIST'
          IMPORTING
            e_funcname = lv_fm_name.
      CATCH cx_fp_api_repository
            cx_fp_api_usage
            cx_fp_api_internal.
        RETURN.
    ENDTRY.

    DATA(ls_docparams) = VALUE sfpdocparams( langu = 'E' country = 'US' ).

    DATA(lt_guid) = get_guid_tab( iv_uuid ).

    SELECT * INTO TABLE ls_input-order_list FROM crmd_orderadm_h  FOR ALL ENTRIES IN lt_guid
       WHERE guid = lt_guid-table_line.
    ASSERT sy-subrc = 0.

    ls_input-order_num = lines( ls_input-order_list ).
    CALL FUNCTION lv_fm_name
      EXPORTING
        /1bcdwb/docparams  = ls_docparams
        orderlist          = ls_input
      IMPORTING
        /1bcdwb/formoutput = ls_pdf_file
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.

    ASSERT sy-subrc = 0.
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.

    fpcontent = ls_pdf_file-pdf.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method CL_CRM_ORDER_2_PDF->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IF_HTTP_EXTENSION~HANDLE_REQUEST.
    CHECK sy-uname = 'WANGJER'.
    CONSTANTS c_linelen TYPE i VALUE 255.
    DATA: wa_data(c_linelen) TYPE x,
          lt_data            LIKE TABLE OF wa_data.
    DATA: lv_pdf_length  TYPE i,
          lv_pdf_xstring TYPE xstring,
          ls_guid_str    TYPE string.

    DATA(lv_uuid) = server->request->get_form_field( 'uuid' ).

    CALL METHOD me->get_output_data
      EXPORTING
        iv_uuid   = lv_uuid
      IMPORTING
        fpcontent = lv_pdf_xstring.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_pdf_xstring
      IMPORTING
        output_length = lv_pdf_length
      TABLES
        binary_tab    = lt_data.

    DATA(lv_contenttype) = 'application/pdf'.
    ls_guid_str = lv_uuid.
    CONCATENATE ls_guid_str '.pdf' INTO DATA(lv_filename).

    server->response->append_data(
                        data   = lv_pdf_xstring
                        length = lv_pdf_length ).

    CONCATENATE 'inline; filename=' lv_filename
      INTO DATA(lv_contentdisposition).

    CALL METHOD server->response->set_header_field
      EXPORTING
        name  = 'content-disposition'
        value = lv_contentdisposition.

    CALL METHOD server->response->set_header_field
      EXPORTING
        name  = 'content-type'
        value = CONV #( lv_contenttype ).

    CALL METHOD server->response->set_header_field
      EXPORTING
        name  = 'content-filename'
        value = lv_filename.

    server->response->delete_header_field(
             name = 'Cache-Control' ).

    server->response->delete_header_field(
             name = 'Expires' ).
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_CRM_ORDER_2_PDF=>OPEN_PDF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_COL_WRAPPER                 TYPE REF TO CL_BSP_WD_COLLECTION_WRAPPER
* | [--->] IO_WINDOW_MANAGER              TYPE REF TO IF_BSP_WD_WINDOW_MANAGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD open_pdf.
    DATA: lv_query TYPE string.

    CHECK io_col_wrapper->size( ) > 0.
    DATA(iterator) = io_col_wrapper->get_iterator( ).
    DATA(bol) = iterator->get_current( ).
    WHILE bol IS NOT INITIAL.
      lv_query = lv_query && ',' && bol->get_property_as_string( 'GUID' ).
      bol = iterator->get_next( ).
    ENDWHILE.

    lv_query = 'uuid=' && lv_query.
    DATA(lv_url) = cl_crm_web_utility=>create_url( iv_path = '/sap/crm/order_print'
                                             iv_query = lv_query
                                             iv_in_same_session = 'X' ).

    DATA(lv_title) = 'Service Order PDF List'.
    DATA(lr_popup) =  io_window_manager->create_popup(  iv_interface_view_name = 'GSURLPOPUP/MainWindow'
                                                                    iv_usage_name          = 'CUGURLPopup'
                                                                    iv_title               = CONV #( lv_title ) ).
    DATA(lr_cn) = lr_popup->get_context_node( 'PARAMS' ).
    DATA(lr_obj) = lr_cn->collection_wrapper->get_current( ).

    DATA(ls_params) = VALUE crmt_gsurlpopup_params( url = lv_url height = '1000' ).
    lr_obj->set_properties( ls_params ).
    lr_popup->set_display_mode( if_bsp_wd_popup=>c_display_mode_plain ).
    lr_popup->set_window_width( 1000 ).
    lr_popup->set_window_height( 1000 ).
    lr_popup->open( ).
  ENDMETHOD.
ENDCLASS.