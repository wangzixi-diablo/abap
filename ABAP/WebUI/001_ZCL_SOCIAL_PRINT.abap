CLASS zcl_social_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .
  PROTECTED SECTION.
private section.

  methods GET_OUTPUT_DATA
    importing
      !IV_UUID type STRING
    exporting
      !FPCONTENT type XSTRING .
  methods _HANDLE_REQUEST
    importing
      !SERVER type ref to IF_HTTP_SERVER .
ENDCLASS.



CLASS ZCL_SOCIAL_PRINT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SOCIAL_PRINT->GET_OUTPUT_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_UUID                        TYPE        STRING
* | [<---] FPCONTENT                      TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_output_data.
    DATA:ls_pdf_file TYPE fpformoutput,
         ls_post     TYPE socialdata,
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
            i_name     = 'ZPF_SOCIAL_POST'
          IMPORTING
            e_funcname = lv_fm_name.
      CATCH cx_fp_api_repository
            cx_fp_api_usage
            cx_fp_api_internal.
        RETURN.
    ENDTRY.

    DATA(ls_docparams) = VALUE sfpdocparams( langu = 'E' country = 'US' ).

    SELECT SINGLE * INTO ls_post FROM socialdata WHERE socialdatauuid = iv_uuid.
    ASSERT sy-subrc = 0.

    CALL FUNCTION lv_fm_name
      EXPORTING
        /1bcdwb/docparams  = ls_docparams
        post_id            = ls_post-internal_id
        created_by         = ls_post-sender_user_account
        reply              = ls_post-reply_created_by
        processor          = ls_post-processor
        creation_date_time = ls_post-creationdatetime
        content            = ls_post-socialposttext
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
* | Instance Public Method ZCL_SOCIAL_PRINT->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_http_extension~handle_request.

    DATA: lv_text TYPE string VALUE 'hello world'.
    CONSTANTS: cv_white_id TYPE string VALUE 'i042416'.

    DATA(lv_origin) = server->request->get_header_field( 'origin' ).
    DATA(lv_userid) = server->request->get_form_field( 'userId' ).
    IF lv_userid = cv_white_id.
      server->response->set_header_field(
         EXPORTING
           name  = 'Access-Control-Allow-Origin'
           value = lv_origin ).
    ENDIF.
    server->response->append_cdata(
                         data   = lv_text
                         length = strlen( lv_text ) ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SOCIAL_PRINT->_HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method _HANDLE_REQUEST.
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
ENDCLASS.