class ZCL_CRM_HANA_TOOL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_data,
         EBENE TYPE SATR_AUSTAB_GESAMT-EBENE,
         HIER_FELD TYPE SATR_AUSTAB_GESAMT-HIER_FELD,
         BRUTTO TYPE SATR_AUSTAB_GESAMT-BRUTTO,
         NETTO TYPE SATR_AUSTAB_GESAMT-NETTO,
         CONTOFFS TYPE i,
         progindex type i,
         index type i,
         color(4) TYPE c,
  END OF ty_data .
  types:
    tt_data type STANDARD TABLE OF ty_data .

  constants CO_HEADER_NAME type STRING value 'Header'. "#EC NOTEXT
  constants CO_ITEM_NAME type STRING value 'Item'. "#EC NOTEXT
  constants CO_CODE type STRING value 'Code'. "#EC NOTEXT
  constants CO_TRACE_ID type STRING value 'TraceID'. "#EC NOTEXT
  constants CO_TRACE_DATE type STRING value 'TraceDate'. "#EC NOTEXT
  constants CO_TRACE_TIME type STRING value 'TraceTime'. "#EC NOTEXT
  constants CO_CALL_STACK_LAYER type STRING value 'CallStackLayer'. "#EC NOTEXT
  constants CO_GROSS_TIME type STRING value 'GrossTime'. "#EC NOTEXT
  constants CO_OFFSET type STRING value 'Offset'. "#EC NOTEXT
  constants CO_NET_TIME type STRING value 'NetTime'. "#EC NOTEXT
  constants CO_TRACE_OBJ_NAME type STRING value 'TraceObjName'. "#EC NOTEXT
  constants CO_INDEX type STRING value 'Index'. "#EC NOTEXT
  constants CO_PROGINDEX type STRING value 'ProgIndex'. "#EC NOTEXT

  class-methods CLASS_CONSTRUCTOR .
  class-methods DOWNLOAD_XML
    importing
      !IV_DATA type ANY TABLE
      !IV_KEY type SATR_TAB_KEY
      !IV_XML_PATH type STRING .
  class-methods VALUE_HELP_FOR_XML_PATH
    changing
      !CV_XML_PATH type STRING .
  class-methods ASSEMBLE_DATA
    importing
      !IV_CODE type STRING
      !IV_TRACE_TIME type SATR_DIRECTORY-TRACE_TIME
      !IV_TRACE_DATE type SATR_DIRECTORY-TRACE_DATE
    exporting
      !OUT_DATA type TT_DATA
      !OUT_KEY type SATR_TAB_KEY .
  class-methods DISPLAY_ALV
    changing
      !CT_DATA type TT_DATA .
  class-methods GET_XML_FILE_PATH
    importing
      !IV_CODE type STRING
      !IV_TRACE_DATE type SATR_DIRECTORY-TRACE_DATE
      !IV_TRACE_TIME type SATR_DIRECTORY-TRACE_TIME
    exporting
      !RV_RESULT type STRING .
  class-methods REMOVE_SPECIAL
    changing
      !CV_LINE type SATR_AUSTAB_GESAMT-HIER_FELD .
protected section.
private section.

  types:
    BEGIN OF ty_node_map,
             index type i,
             node type ref to if_ixml_element,
    END OF ty_node_map .
  types:
    tt_node_map type STANDARD TABLE OF ty_node_map .

  class-data MO_IXML type ref to IF_IXML .
  class-data MO_STREAM_FACTORY type ref to IF_IXML_STREAM_FACTORY .
  class-data MO_OSTREAM type ref to IF_IXML_OSTREAM .
  class-data MO_RENDERER type ref to IF_IXML_RENDERER .
  class-data MO_DOCUMENT type ref to IF_IXML_DOCUMENT .
  class-data MO_ENCODING type ref to IF_IXML_ENCODING .
  class-data MO_CURRENT_ELEMENT type ref to IF_IXML_ELEMENT .
  class-data MV_KEY type SATR_TAB_KEY .
  class-data MT_TABLE type TT_DATA .
  class-data MT_NODE_MAP type TT_NODE_MAP .
  class-data MV_CURRENT_MAP type TY_NODE_MAP .
  class-data MO_ROOT_ELEMENT type ref to IF_IXML_ELEMENT .
  type-pools ABAP .
  class-data MV_FIRST_ENTRY type ABAP_BOOL value ABAP_TRUE. "#EC NOTEXT .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  . " .
  constants CO_DUMMY type STRING value '°'. "#EC NOTEXT
  class-data MT_COLOR type STRING_TABLE .
  class-data MV_REPORT_NAME type SATR_DIRECTORY-OBJ_NAME .

  class-methods COPY_LINE
    importing
      !IV_LINE type SATR_AUSTAB_GESAMT
    changing
      !CV_LINE type TY_DATA .
  class-methods GET_PARENT
    importing
      !IV_CHILD_DATA type TY_DATA
    returning
      value(RV_PARENT) type ref to IF_IXML_ELEMENT .
  class-methods CREATE_HEADER .
  class-methods DOWNLOAD
    importing
      !IV_XML_PATH type STRING .
  class-methods CREATE_BODY .
  class-methods CREATE_LINE
    importing
      !IV_LINE type TY_DATA .
  class-methods REMOVE_UNDERLINE
    changing
      !CV_LINE type STRING .
  class-methods REMOVE_DUMMY
    changing
      !CV_LINE type SATR_AUSTAB_GESAMT-HIER_FELD .
  class-methods CONVERT_HIERARCHY
    changing
      !CV_LINE type SATR_AUSTAB_GESAMT .
  class-methods GET_I_NUMBER
    returning
      value(RV_I_NUMBER) type STRING .
ENDCLASS.



CLASS ZCL_CRM_HANA_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_TOOL=>ASSEMBLE_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CODE                        TYPE        STRING
* | [--->] IV_TRACE_TIME                  TYPE        SATR_DIRECTORY-TRACE_TIME
* | [--->] IV_TRACE_DATE                  TYPE        SATR_DIRECTORY-TRACE_DATE
* | [<---] OUT_DATA                       TYPE        TT_DATA
* | [<---] OUT_KEY                        TYPE        SATR_TAB_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
method ASSEMBLE_DATA.
  DATA: lo_tool    type ref to CL_ATRA_TOOL_SE30_MAIN,
        ls_type    like line of lo_tool->IT_AUSTAB_HIER,
        lt_all_hit LIKE lo_tool->IT_AUSTAB_HIER,
        ls_line    like ls_type,
        ls_data    type ty_data,
        ls_key     type SATR_DIRECTORY,
        lr_line    LIKE REF TO ls_line,
        lt_index   type STANDARD TABLE OF int4,
        lv_index   type int4.

  select single * INTO ls_key FROM SATR_DIRECTORY where trace_date = iv_trace_date and trace_time = iv_trace_time AND TRACE_USER = sy-uname.
  if sy-subrc <> 0.
     WRITE: / 'No related record found. Please check your trace data' COLOR COL_NEGATIVE.
     RETURN.
  ENDIF.

  out_key-CPROG  = ls_key-SATR_KEY.
  out_key-datum  = iv_trace_date.
  out_key-uzeit  = iv_trace_time.
  mv_report_name = ls_key-obj_name.

  call method CL_ATRA_TOOL_SE30_MAIN=>CREATE_OBJECT
    EXPORTING
       P_CONTAINER_KEY = out_key
       P_TO_DO         = abap_true
       P_WITH_DB_TIMES = abap_true
       p_index         = 0
    IMPORTING
       EO_REF_TO_MAIN = lo_tool.


 LOOP AT lo_tool->IT_AUSTAB_HIER INTO ls_type WHERE HIER_FELD CS iv_code AND NOT hier_feld CS '<'.
    APPEND ls_type TO lt_all_hit.
 ENDLOOP.

 IF lt_all_hit IS INITIAL.
    WRITE: / 'No related record found!' COLOR COL_NEGATIVE.
    RETURN.
 ENDIF.

 LOOP AT lt_all_hit INTO ls_type.
   LOOP AT lo_tool->IT_AUSTAB_HIER REFERENCE INTO lr_line FROM ls_type-start TO ls_type-end.

      CHECK lr_line->BRUTTO <> 0 AND lr_line->NETTO <> 0.
      READ TABLE lt_index TRANSPORTING NO FIELDS WITH KEY table_line = lr_line->index.
      IF sy-subrc = 0.
         CONTINUE.
      ELSE.
         APPEND lr_line->index TO lt_index.
      ENDIF.
      CALL METHOD remove_dummy( CHANGING cv_line = lr_line->hier_feld ).
      CALL METHOD remove_special( CHANGING cv_line = lr_line->hier_feld ).
      CALL METHOD convert_hierarchy( CHANGING cv_line = lr_line->* ).
      CALL METHOD copy_line( EXPORTING iv_line = lr_line->* CHANGING cv_line = ls_data ).
      APPEND ls_data TO out_data.
   ENDLOOP.
 ENDLOOP.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_TOOL=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CLASS_CONSTRUCTOR.
  mo_ixml = cl_ixml=>create( ).
  mo_document = mo_ixml->create_document( ).
  mo_encoding = mo_ixml->create_encoding(
                  character_set = 'utf-8'
                  byte_order = -1
                ).
  mv_first_entry = abap_true.

  APPEND 'C100' TO mt_color.
  APPEND 'C200' TO mt_color.
  APPEND 'C300' TO mt_color.
  APPEND 'C400' TO mt_color.
  APPEND 'C500' TO mt_color.
  APPEND 'C600' TO mt_color.
  APPEND 'C700' TO mt_color.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>CONVERT_HIERARCHY
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_LINE                        TYPE        SATR_AUSTAB_GESAMT
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CONVERT_HIERARCHY.
   CHECK cv_line-EBENE > 1.
   DO cv_line-EBENE TIMES.
      cv_line-hier_feld = `__` && cv_line-hier_feld.
   ENDDO.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>COPY_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LINE                        TYPE        SATR_AUSTAB_GESAMT
* | [<-->] CV_LINE                        TYPE        TY_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method COPY_LINE.
   DATA: lv_color_index TYPE i.

   cv_line-EBENE     = iv_line-EBENE.
   cv_line-HIER_FELD = iv_line-HIER_FELD.
   cv_line-BRUTTO    = iv_line-BRUTTO.
   cv_line-NETTO     = iv_line-NETTO.
   cv_line-CONTOFFS  = iv_line-CONTOFFS.
   cv_line-progindex = iv_line-progindex.
   cv_line-index     = iv_line-index.
   lv_color_index    = ( iv_line-EBENE MOD 7 ) + 1.
   READ TABLE mt_color INTO cv_line-color INDEX lv_color_index.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>CREATE_BODY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CREATE_BODY.
  DATA: ls_line LIKE LINE OF mt_table.

  LOOP AT mt_table INTO ls_line.
     create_line( EXPORTING iv_line = ls_line ).
  ENDLOOP.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>CREATE_HEADER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CREATE_HEADER.
  data: lv_temp type string.

  mo_root_element = mo_document->create_element( name = CO_HEADER_NAME ).

  lv_temp = mv_key-cprog.
  mo_root_element->SET_ATTRIBUTE( name = co_trace_id value = lv_temp  ).

  lv_temp = mv_key-datum.
  mo_root_element->SET_ATTRIBUTE( name = co_trace_date value = lv_temp ).

  lv_temp = mv_key-uzeit.
  mo_root_element->SET_ATTRIBUTE( name = co_trace_time value = lv_temp ).

  lv_temp = mv_report_name.
  mo_root_element->SET_ATTRIBUTE( name = CO_TRACE_OBJ_NAME value = lv_temp ).

  mo_document->append_child( new_child = mo_root_element ).

  MV_CURRENT_MAP-index = 0.
  MV_CURRENT_MAP-node = mo_root_element.
  APPEND mv_current_map to mt_node_map.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>CREATE_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LINE                        TYPE        TY_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CREATE_LINE.
   DATA: lr_line TYPE REF TO IF_IXML_ELEMENT,
         lr_parent TYPE REF TO IF_IXML_ELEMENT,
         ls_map TYPE TY_NODE_MAP,
         lv_temp type string.

   lr_line = mo_document->create_element( name = CO_ITEM_NAME ).

   lv_temp = iv_line-hier_feld.
   remove_underline( CHANGING cv_line = lv_temp ).
   lr_line->set_attribute( name = CO_CODE value = lv_temp  ).

   lv_temp = iv_line-ebene.
   lr_line->set_attribute( name = CO_CALL_STACK_LAYER value = lv_temp  ).

   lv_temp = iv_line-BRUTTO.
   lr_line->set_attribute( name = CO_gross_time value = lv_temp  ).

   lv_temp = iv_line-index.
   lr_line->set_attribute( name = CO_index value = lv_temp  ).

   lv_temp = iv_line-NETTO.
   lr_line->set_attribute( name = CO_net_time value = lv_temp  ).

   lv_temp = iv_line-contoffs.
   lr_line->set_attribute( name = CO_offset value = lv_temp  ).

   lv_temp = iv_line-progindex.
   lr_line->set_attribute( name = CO_progindex value = lv_temp  ).

   lr_parent = get_parent( iv_child_data = iv_line ).
   CHECK lr_parent IS NOT INITIAL.
   lr_parent->append_child( new_child = lr_line ).

   ls_map-index = iv_line-ebene.
   ls_map-node = lr_line.
   APPEND ls_map TO mt_node_map.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_TOOL=>DISPLAY_ALV
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_DATA                        TYPE        TT_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method DISPLAY_ALV.
  DATA:

  ls_fieldcat TYPE LINE OF slis_t_fieldcat_alv,
  it_fieldcat TYPE slis_t_fieldcat_alv,
  ls_layout TYPE slis_layout_alv.

  ls_fieldcat-fieldname     = 'EBENE'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-seltext_l     = 'Call Stack Level'.
  ls_fieldcat-seltext_m     = 'Call Stack Level'.
  ls_fieldcat-seltext_s     = 'Call Stack Level'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'HIER_FELD'.
  ls_fieldcat-seltext_l     = 'Code Name'.
  ls_fieldcat-seltext_m     = 'Code Name'.
  ls_fieldcat-seltext_s     = 'Code Name'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'BRUTTO'.
  ls_fieldcat-seltext_l     = 'Gross Time'.
  ls_fieldcat-seltext_m     = 'Gross Time'.
  ls_fieldcat-seltext_s     = 'Gross Time'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_fieldcat-fieldname     = 'NETTO'.
  ls_fieldcat-seltext_l     = 'Net Time'.
  ls_fieldcat-seltext_m     = 'Net Time'.
  ls_fieldcat-seltext_s     = 'Net Time'.
  APPEND ls_fieldcat TO it_fieldcat.

  ls_layout-zebra             = 'X'.
  ls_layout-cell_merge        = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-info_fieldname  = 'COLOR'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-cprog
      is_layout                = ls_layout
      it_fieldcat              = it_fieldcat[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = ct_data.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>DOWNLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_XML_PATH                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method DOWNLOAD.
  TYPES: x_line(80) TYPE x.
  TYPES: x_line_tab TYPE TABLE OF x_line.
  DATA: ct_data TYPE x_line_tab,
        lv_path type string.

  "Generate output data
  mo_stream_factory = mo_ixml->create_stream_factory( ).
  mo_ostream = mo_stream_factory->create_ostream_itable(
                  table = ct_data
                ).

  mo_ostream->set_encoding( encoding = mo_encoding ).

  mo_renderer = mo_ixml->create_renderer(
                document = mo_document
                ostream = mo_ostream
              ).

  mo_renderer->set_normalizing( ).
  mo_renderer->render( ).

  mo_ostream->close( ).
  lv_path = iv_xml_path.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename          = lv_path
      filetype          = 'BIN'
      codepage          = '4110'
      confirm_overwrite = ''
    TABLES
      data_tab          = ct_data
    EXCEPTIONS
      OTHERS            = 1.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_TOOL=>DOWNLOAD_XML
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATA                        TYPE        ANY TABLE
* | [--->] IV_KEY                         TYPE        SATR_TAB_KEY
* | [--->] IV_XML_PATH                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method DOWNLOAD_XML.

   mv_key = iv_key.
   mt_table = iv_data.
   CREATE_HEADER( ).
   CREATE_BODY( ).
   DOWNLOAD( iv_xml_path = iv_xml_path ).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>GET_I_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_I_NUMBER                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_I_NUMBER.
   DATA: lv_i_number TYPE string,
         lv_SNC TYPE BAPISNCU,
         lv_R1 TYPE STANDARD TABLE OF BAPIRET2,
         lv_offset TYPE i,
         lv_offset2 type i,
         lv_len type i.

   CALL FUNCTION 'BAPI_USER_GET_DETAIL'
     EXPORTING
       USERNAME = sy-uname
     IMPORTING
       SNC      = lv_SNC
     TABLES
       RETURN   = lv_R1.

   CHECK lv_snc IS NOT INITIAL.

   FIND '=' IN lv_snc MATCH OFFSET lv_offset.
   CHECK sy-subrc = 0.
   FIND ',' IN lv_SNC MATCH OFFSET lv_offset2.
   CHECK sy-subrc = 0.

   lv_len = lv_offset2 - lv_offset - 1.
   lv_offset = lv_offset + 1.
   rv_i_number = lv_snc+lv_offset(lv_len).

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>GET_PARENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CHILD_DATA                  TYPE        TY_DATA
* | [<-()] RV_PARENT                      TYPE REF TO IF_IXML_ELEMENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_PARENT.
  DATA: lv_line TYPE i,
        ls_map LIKE LINE OF mt_node_map.

  IF mv_first_entry = abap_true.
     mv_first_entry = abap_false.
     rv_parent = mo_root_element.
     RETURN.
  ENDIF.

  lv_line = lines( mt_node_map ).

  WHILE lv_line > 0.
     READ TABLE mt_node_map INTO ls_map INDEX lv_line.
     IF ls_map-index = iv_child_data-ebene - 1.
        rv_parent = ls_map-node.
        RETURN.
     ENDIF.

     lv_line = lv_line - 1.
  ENDWHILE.

  rv_parent = mo_root_element.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_TOOL=>GET_XML_FILE_PATH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CODE                        TYPE        STRING
* | [--->] IV_TRACE_DATE                  TYPE        SATR_DIRECTORY-TRACE_DATE
* | [--->] IV_TRACE_TIME                  TYPE        SATR_DIRECTORY-TRACE_TIME
* | [<---] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method GET_XML_FILE_PATH.
   DATA: lv_i_number type string.

   lv_i_number = get_i_number( ).

   CHECK lv_i_number IS NOT INITIAL.

   CONCATENATE 'C:\Users\' lv_i_number '\Desktop\' iv_code '_' iv_trace_date '_' iv_trace_time '.xml'
      INTO rv_result.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>REMOVE_DUMMY
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_LINE                        TYPE        SATR_AUSTAB_GESAMT-HIER_FELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REMOVE_DUMMY.
   data: lv_length type i,
         lv_start type i,
         lv_index type i,
         lt_match type match_result_tab,
         ls_match LIKE LINE OF lt_match.

   CLEAR: lv_length, lt_match, lv_start.

   FIND ALL OCCURRENCES OF co_dummy IN cv_line RESULTS lt_match.
   READ TABLE lt_match INTO ls_match INDEX 2.
   lv_length = strlen( cv_line ) - ls_match-offset - 1.
   lv_start = ls_match-offset + 1.
   cv_line = cv_line+lv_start(lv_length).
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_TOOL=>REMOVE_SPECIAL
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_LINE                        TYPE        SATR_AUSTAB_GESAMT-HIER_FELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REMOVE_SPECIAL.


 CONSTANTS: co_special type x LENGTH 4 VALUE '0300'.

 DATA: lv_char type c length 1.

 FIELD-SYMBOLS: <special> like lv_char.

 ASSIGN co_special TO <special> CASTING.

 FIND <special> IN cv_line.

 IF sy-subrc = 0.
    REPLACE ALL OCCURRENCES OF <special> IN cv_line with SPACE.
    CONDENSE cv_line.
 ENDIF.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_CRM_HANA_TOOL=>REMOVE_UNDERLINE
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_LINE                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method REMOVE_UNDERLINE.
  SHIFT cv_line LEFT DELETING LEADING '_'.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_CRM_HANA_TOOL=>VALUE_HELP_FOR_XML_PATH
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CV_XML_PATH                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
method VALUE_HELP_FOR_XML_PATH.
    DATA: l_filetable TYPE filetable,
          l_path      LIKE LINE OF l_filetable,
          l_status    TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_extension       = 'XML'
      file_filter             = cl_gui_frontend_services=>filetype_all
    CHANGING
      file_table              = l_filetable
      rc                      = l_status
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc IS INITIAL AND l_status > 0.
    READ TABLE l_filetable INTO l_path INDEX 1.
    cv_xml_path = l_path.
  ENDIF.
endmethod.
ENDCLASS.