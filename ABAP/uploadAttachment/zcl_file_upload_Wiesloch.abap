CLASS zcl_file_upload DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    CLASS-METHODS class_constructor .
  PROTECTED SECTION.
private section.

  constants FILENAME type STRING value 'filename=' ##NO_TEXT.
  constants FILECONTENT type STRING value 'content-type' ##NO_TEXT.
  constants FILELENGTH type STRING value 'Content-Length:' ##NO_TEXT.
  class-data CRLFLENGTH type I .
  data MT_UPLOADED type STRING_TABLE .

  methods UPLOAD_RESPONSE
    importing
      !SERVER type ref to IF_HTTP_SERVER .
  methods RESPONSE
    importing
      !SERVER type ref to IF_HTTP_SERVER .
  methods GET_FILE_DETAIL
    importing
      !IV_CONTENT type STRING
    exporting
      !EV_FILENAME type STRING
      !EV_FILETYPE type STRING
      !EV_LENGTH type INT4
      !EV_CONTENT type XSTRING .
  methods GET_FILE_CONTENT
    importing
      !IV_SUBSTR type STRING
      !IV_FILETYPE type STRING
    exporting
      !EV_LENGTH type INT4
      !EV_CONTENT type XSTRING .
  methods CREATE_ATTACHMENT
    importing
      !IV_DATA type XSTRING
      !IV_GUID type COMM_PRODUCT-PRODUCT_GUID
      !IV_FILE_NAME type STRING
      !IV_FILE_TYPE type STRING
      !IV_BOR_TYPE type STRING .
ENDCLASS.



CLASS ZCL_FILE_UPLOAD IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_FILE_UPLOAD=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
    crlflength = strlen( cl_abap_char_utilities=>cr_lf ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FILE_UPLOAD->CREATE_ATTACHMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA                        TYPE        XSTRING
* | [--->] IV_GUID                        TYPE        COMM_PRODUCT-PRODUCT_GUID
* | [--->] IV_FILE_NAME                   TYPE        STRING
* | [--->] IV_FILE_TYPE                   TYPE        STRING
* | [--->] IV_BOR_TYPE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_attachment.
    DATA:
      ls_bo              TYPE sibflporb,
      ls_prop            TYPE LINE OF sdokproptys,
      lt_prop            TYPE sdokproptys,
      lt_properties_attr TYPE crmt_attr_name_value_t,
      ls_file_info       TYPE sdokfilaci,
      lt_file_info       TYPE sdokfilacis,
      lt_file_content    TYPE sdokcntbins,
      lv_length          TYPE i,
      lv_file_xstring    TYPE xstring,
      ls_loio            TYPE skwf_io,
      ls_phio            TYPE skwf_io,
      ls_error           TYPE skwf_error.

    ls_prop-name = 'DESCRIPTION'.
    ls_prop-value = iv_file_name.
    APPEND ls_prop TO lt_prop.

    ls_prop-name = 'KW_RELATIVE_URL'.
    ls_prop-value = iv_file_name.
    APPEND ls_prop TO lt_prop.

    ls_prop-name = 'LANGUAGE'.
    ls_prop-value = sy-langu.
    APPEND ls_prop TO lt_prop.

    lv_file_xstring = iv_data.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_file_xstring
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = lt_file_content.

    ls_file_info-binary_flg = 'X'.
    ls_file_info-file_name = iv_file_name.
    ls_file_info-file_size = lv_length.
    ls_file_info-mimetype = iv_file_type.
    APPEND ls_file_info TO lt_file_info.

    ls_bo-instid = iv_guid.
    ls_bo-typeid = iv_bor_type.
    ls_bo-catid = 'BO'.

    CALL METHOD cl_crm_documents=>create_with_table
      EXPORTING
        business_object     = ls_bo
        properties          = lt_prop
        properties_attr     = lt_properties_attr
        file_access_info    = lt_file_info
        file_content_binary = lt_file_content
        raw_mode            = 'X'
      IMPORTING
        loio                = ls_loio
        phio                = ls_phio
        error               = ls_error.

    ASSERT ls_error IS INITIAL.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FILE_UPLOAD->GET_FILE_CONTENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SUBSTR                      TYPE        STRING
* | [--->] IV_FILETYPE                    TYPE        STRING
* | [<---] EV_LENGTH                      TYPE        INT4
* | [<---] EV_CONTENT                     TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_file_content.
    DATA: lt_result     TYPE string_table,
          lt_result_tab TYPE  match_result_tab,
          ls_result     LIKE LINE OF lt_result_tab,
          lv_sub        TYPE string,
          lv_offset     TYPE i,
          lv_digit      TYPE c,
          lv_index      TYPE i,
          lv_offset2    TYPE i,
          lv_len        TYPE i,
          l_conv        TYPE REF TO cl_abap_conv_out_ce.

    FIELD-SYMBOLS: <hex> TYPE x.
    SPLIT iv_substr AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_result.

    READ TABLE lt_result INTO lv_sub INDEX 1.

    FIND filelength IN lv_sub MATCH OFFSET lv_offset.

    lv_len = strlen( filelength ).

    lv_offset = lv_offset + lv_len.
    lv_len = strlen( lv_sub ) - lv_len.

    lv_sub = lv_sub+lv_offset(lv_len).

    CONDENSE lv_sub.

    ev_length = lv_sub.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_substr MATCH OFFSET lv_offset.
    FIND ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN iv_substr RESULTS lt_result_tab.
    READ TABLE lt_result_tab INTO ls_result INDEX lines( lt_result_tab ).

    lv_len = ls_result-offset - lv_offset - crlflength.
    lv_offset = lv_offset + crlflength.

    lv_sub = iv_substr+lv_offset(lv_len).
    CLEAR: ev_content.
    DO lv_len TIMES.
      lv_index = sy-index - 1.
      lv_digit = lv_sub+lv_index(1).
      ASSIGN lv_digit TO <hex> CASTING.
      CONCATENATE ev_content <hex>(1) INTO ev_content IN BYTE MODE.
    ENDDO.

    ASSERT xstrlen( ev_content ) = ev_length.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FILE_UPLOAD->GET_FILE_DETAIL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CONTENT                     TYPE        STRING
* | [<---] EV_FILENAME                    TYPE        STRING
* | [<---] EV_FILETYPE                    TYPE        STRING
* | [<---] EV_LENGTH                      TYPE        INT4
* | [<---] EV_CONTENT                     TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_file_detail.
    DATA: lv_offset1 TYPE int4,
          lv_offset2 TYPE int4,
          lv_len     TYPE int4,
          lv_sub     TYPE string,
          lv_sub2    TYPE string.

    FIND FIRST OCCURRENCE OF filename IN iv_content MATCH OFFSET lv_offset1.
    FIND FIRST OCCURRENCE OF filecontent IN iv_content MATCH OFFSET lv_offset2.

    lv_len = lv_offset2 - lv_offset1 - strlen( filecontent ) + 1.

    lv_offset1 = lv_offset1 + strlen( filename ).
    lv_sub = iv_content+lv_offset1(lv_len).
    REPLACE ALL OCCURRENCES OF '"' IN lv_sub WITH space.
    CONDENSE lv_sub.

    ev_filename = lv_sub.

    FIND FIRST OCCURRENCE OF filelength IN iv_content MATCH OFFSET lv_offset1.
    lv_len = lv_offset1 - lv_offset2 - strlen( filelength ) - 2.
    lv_offset2 = lv_offset2 + strlen( filecontent ) + 1.
    lv_sub = iv_content+lv_offset2(lv_len).
    CONDENSE lv_sub.

    ev_filetype = lv_sub.

    lv_len = strlen( iv_content ) - lv_offset1.
    lv_sub2 = iv_content+lv_offset1(lv_len).

    CALL METHOD get_file_content
      EXPORTING
        iv_substr   = lv_sub2
        iv_filetype = ev_filetype
      IMPORTING
        ev_length   = ev_length
        ev_content  = ev_content.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_FILE_UPLOAD->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_http_extension~handle_request.

    DATA: lt_form         TYPE  tihttpnvp,
          lv_data         TYPE string,
          lv_boundary     TYPE string,
          lv_content_type TYPE string,
          lv_filename     TYPE string,
          lt_result       TYPE string_table,
          lv_line         TYPE string,
          lv_filelength   TYPE i,
          lv_filecontent  TYPE xstring,
          lv_filetype     TYPE string.

    lv_content_type = server->request->get_header_field( 'content-type' ).
    SPLIT lv_content_type AT 'boundary=' INTO TABLE lt_result.
    READ TABLE lt_result INTO lv_boundary INDEX 2.
    server->request->get_form_fields( CHANGING fields = lt_form ).
    lv_data = server->request->get_cdata( ).

    SPLIT lv_data AT lv_boundary INTO TABLE lt_result.
    LOOP AT lt_result INTO lv_line WHERE table_line CS filelength.
      CALL METHOD get_file_detail
        EXPORTING
          iv_content  = lv_line
        IMPORTING
          ev_filename = lv_filename
          ev_filetype = lv_filetype
          ev_length   = lv_filelength
          ev_content  = lv_filecontent.

       CALL METHOD create_attachment
       EXPORTING
          iv_data = lv_filecontent
          iv_guid = '00163EA71FFC1ED1ABC4900237411DB4'
          iv_file_name = lv_filename
          iv_file_type = lv_filetype
          iv_bor_type = 'BUS1178'.

       APPEND lv_filename TO mt_uploaded.

    ENDLOOP.

    upload_response( server ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FILE_UPLOAD->RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD response.
    DATA: lv_input_str TYPE string,
          lv_html      TYPE string,
          lt_scarr     TYPE TABLE OF scarr.

    FIELD-SYMBOLS:
    <fs_scarr> TYPE scarr.

    lv_input_str = server->request->get_form_field( 'query' ).

    SELECT * FROM scarr INTO TABLE lt_scarr.

    IF strlen( lv_input_str ) > 0.
      LOOP AT lt_scarr ASSIGNING <fs_scarr>.
        FIND lv_input_str IN <fs_scarr>-carrname IGNORING CASE.
        CHECK sy-subrc = 0.
        IF strlen( lv_html ) = 0.
          CONCATENATE `<a href=’` <fs_scarr>-url `’ target=’_blank’>`
            <fs_scarr>-carrname `</a>` INTO lv_html.
        ELSE.
          CONCATENATE lv_html `<br />` `<a href=’` <fs_scarr>-url `’ target=’_blank’>`
            <fs_scarr>-carrname `</a>` INTO lv_html.

        ENDIF.
      ENDLOOP.
    ENDIF.

    IF strlen( lv_html ) = 0.
      lv_html = '&lt;no suggestion&gt;'.
    ENDIF.

    server->response->set_cdata( lv_html ).
    server->response->set_header_field( name = 'Access-Control-Allow-Origin' value = '*' ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_FILE_UPLOAD->UPLOAD_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD UPLOAD_RESPONSE.
    DATA: lv_str TYPE string,
          lv_html      TYPE string.

    LOOP AT mt_uploaded INTO lv_str.
       CONCATENATE lv_html `<p>File uploaded successfully: ` lv_str `</p>` INTO lv_html.
    ENDLOOP.

    IF strlen( lv_html ) = 0.
      lv_html = '&lt;no file uploaded&gt;'.
    ENDIF.

    server->response->set_cdata( lv_html ).
    server->response->set_header_field( name = 'Access-Control-Allow-Origin' value = '*' ).
  ENDMETHOD.
ENDCLASS.