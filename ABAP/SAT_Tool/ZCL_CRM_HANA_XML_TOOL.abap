class ZCL_CRM_HANA_XML_TOOL definition
  public
  final
  create public .

public section.

  class-methods UPLOAD_XML
    importing
      !IV_XML_PATH type STRING .
  class ZCL_CRM_HANA_TOOL definition load .
  class-methods GET_OUTPUT
    exporting
      !OUT_HEADER type SATR_TAB_KEY
      !OUT_TRACE_DATA type ZCL_CRM_HANA_TOOL=>TT_DATA
      !OUT_TRACE_OBJ_NAME type SATR_DIRECTORY-OBJ_NAME .
protected section.
private section.

  class-data MO_IXML type ref to IF_IXML .
  class-data MO_XML_DOCUMENT type ref to IF_IXML_DOCUMENT .
  class-data MO_STREAM_FACTORY type ref to IF_IXML_STREAM_FACTORY .
  class-data MO_ISTREAM type ref to IF_IXML_ISTREAM .
  class-data MO_PARSER type ref to IF_IXML_PARSER .
  type-pools ABAP .
  class-data MS_ERROR_OCCURRED type ABAP_BOOL .
  class-data MO_XML_ROOT type ref to IF_IXML_NODE .
  class-data MV_KEY type SATR_TAB_KEY .
  class ZCL_CRM_HANA_TOOL definition load .
  class-data MT_DATA type ZCL_CRM_HANA_TOOL=>TT_DATA .
  class-data MV_TRACE_OBJ type SATR_DIRECTORY-OBJ_NAME .

  class-methods EXTRACT_ITEM
    importing
      !IV_ITEM type ref to IF_IXML_NODE .
  class-methods PARSE_ITEM
    importing
      !IV_PARENT type ref to IF_IXML_NODE .
  class-methods INIT
    importing
      !IV_XML type XSTRING .
  class-methods UPLOAD
    importing
      !IV_XML_PATH type STRING
    exporting
      !OUT_XML type XSTRING .
  class-methods PARSE .
  class-methods GET_NODE_BY_NAME
    importing
      !IN_NODE_NAME type STRING
      !IN_PARENT type ref to IF_IXML_NODE
    exporting
      !OUT_CHILD type ref to IF_IXML_NODE .
  class-methods PARSE_HEADER .
ENDCLASS.



CLASS ZCL_CRM_HANA_XML_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_XML_TOOL=>EXTRACT_ITEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ITEM                        TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method EXTRACT_ITEM.
  DATA: ls_line LIKE LINE OF mt_data,
        lr_element TYPE REF TO IF_IXML_ELEMENT.

  lr_element ?= iv_item.
  ls_line-hier_feld = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>co_code ).
  CALL METHOD ZCL_CRM_HANA_TOOL=>REMOVE_SPECIAL( CHANGING cv_line = ls_line-hier_feld ).
  CONDENSE ls_line-hier_feld.
  ls_line-EBENE = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>CO_CALL_STACK_LAYER ).
  ls_line-BRUTTO = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>CO_GROSS_TIME ).
  ls_line-netto = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>CO_net_TIME ).
  ls_line-CONTOFFS = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>CO_OFFSET ).
  ls_line-progindex = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>co_progindex ).
  ls_line-index = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>co_index ).
  APPEND ls_line TO mt_data.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_XML_TOOL=>GET_NODE_BY_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IN_NODE_NAME                   TYPE        STRING
* | [--->] IN_PARENT                      TYPE REF TO IF_IXML_NODE
* | [<---] OUT_CHILD                      TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_NODE_BY_NAME.
  DATA: lt_child TYPE REF TO IF_IXML_NODE_LIST,
        lv_length TYPE i,
        lv_index TYPE i VALUE 0,
        lv_item TYPE REF TO IF_IXML_NODE,
        lv_name TYPE string.

  CHECK in_parent IS NOT INITIAL.
  lt_child = in_parent->GET_CHILDREN( ).
  lv_length = lt_child->GET_LENGTH( ).
  IF lv_length <= 0.
    EXIT.
  ELSE.
    DO lv_length TIMES.
      lv_item = lt_child->GET_ITEM( lv_index ).
      lv_name = lv_item->GET_NAME( ).
      IF lv_name = in_node_name.
        out_child = lv_item.
        EXIT.
      ENDIF.
      lv_index = lv_index + 1.
    ENDDO.
  ENDIF.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_XML_TOOL=>GET_OUTPUT
* +-------------------------------------------------------------------------------------------------+
* | [<---] OUT_HEADER                     TYPE        SATR_TAB_KEY
* | [<---] OUT_TRACE_DATA                 TYPE        ZCL_CRM_HANA_TOOL=>TT_DATA
* | [<---] OUT_TRACE_OBJ_NAME             TYPE        SATR_DIRECTORY-OBJ_NAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_OUTPUT.
   out_header         = mv_key.
   out_trace_data     = mt_data.
   out_trace_obj_name = mv_trace_obj.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_XML_TOOL=>INIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_XML                         TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method INIT.
   CLEAR: MO_IXML, MO_XML_DOCUMENT, MO_STREAM_FACTORY, MO_ISTREAM, MO_PARSER, MS_ERROR_OCCURRED,
          mo_xml_root, mv_key, mt_data, mv_trace_obj.

   mo_ixml = cl_ixml=>create( ).
   mo_xml_document = mo_ixml->create_document( ).

   mo_stream_factory = mo_ixml->create_stream_factory( ).
   mo_istream = mo_stream_factory->create_istream_xstring(
                  string = iv_xml
               ).

   mo_parser = mo_ixml->create_parser(
                stream_factory = mo_stream_factory
                istream = mo_istream
                document = mo_xml_document
              ).

   IF mo_parser->parse( ) NE 0.
      MS_ERROR_OCCURRED = abap_true.
      WRITE:/ 'Error Occurred when Parsing XML file!' COLOR COL_NEGATIVE INTENSIFIED ON.
      RETURN.
   ENDIF.

   mo_xml_root = mo_xml_document->GET_ROOT_ELEMENT( ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_XML_TOOL=>PARSE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method PARSE.

   " MO_XML_ROOT: Header

   parse_header( ).

   parse_item( iv_parent = MO_XML_ROOT ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_XML_TOOL=>PARSE_HEADER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method PARSE_HEADER.
   DATA:  lr_element TYPE REF TO IF_IXML_ELEMENT.

   lr_element ?= mo_xml_root.
   mv_key-cprog = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>CO_TRACE_ID ).
   mv_key-datum = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>CO_TRACE_DATE ).
   mv_key-uzeit = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>CO_TRACE_TIME ).
   mv_trace_obj = lr_element->get_attribute( ZCL_CRM_HANA_TOOL=>CO_TRACE_obj_name ).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_XML_TOOL=>PARSE_ITEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PARENT                      TYPE REF TO IF_IXML_NODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
method PARSE_ITEM.
  DATA: lt_child TYPE REF TO IF_IXML_NODE_LIST,
        lv_length TYPE i,
        lv_index TYPE i VALUE 0,
        lv_item TYPE REF TO IF_IXML_NODE,
        lv_name TYPE string.

  CHECK iv_parent IS NOT INITIAL.
  lt_child = iv_parent->GET_CHILDREN( ).
  lv_length = lt_child->GET_LENGTH( ).
  DO lv_length TIMES.
     lv_item = lt_child->GET_ITEM( lv_index ).
     "lv_name = lv_item->GET_NAME( ).
     extract_item( lv_item ).
     parse_item( lv_item ).
     lv_index = lv_index + 1.
  ENDDO.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_XML_TOOL=>UPLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_XML_PATH                    TYPE        STRING
* | [<---] OUT_XML                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method UPLOAD.
  DATA: l_filename TYPE string,
        l_rawtab   TYPE STANDARD TABLE OF raw255,
        l_len      TYPE i.

  l_filename = iv_xml_path.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_filename
      filetype                = 'BIN'
    IMPORTING
      filelength              = l_len
    CHANGING
      data_tab                = l_rawtab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc IS NOT INITIAL.
     MS_ERROR_OCCURRED = abap_true.
     WRITE:/ 'Error Occurred when Uploading XML file!' COLOR COL_NEGATIVE INTENSIFIED ON.
     RETURN.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = l_len
    IMPORTING
      buffer       = out_xml
    TABLES
      binary_tab   = l_rawtab
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc IS NOT INITIAL.
     MS_ERROR_OCCURRED = abap_true.
  ENDIF.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_XML_TOOL=>UPLOAD_XML
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_XML_PATH                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method UPLOAD_XML.
   DATA: lv_xml TYPE xstring.

   upload( EXPORTING iv_xml_path = iv_xml_path IMPORTING out_xml = lv_xml ).
   CHECK MS_ERROR_OCCURRED = abap_false.

   INIT( EXPORTING iv_xml = lv_xml ).
   CHECK MS_ERROR_OCCURRED = abap_false.

   PARSE( ).
endmethod.
ENDCLASS.