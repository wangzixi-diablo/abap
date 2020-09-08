class CL_ABAP_GIT_ISSUE_TOOL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_sorted_node,
        index     TYPE string,
        attribute TYPE string,
        value     TYPE string,
      END OF ty_sorted_node .
  types:
    tt_sorted_node TYPE STANDARD TABLE OF ty_sorted_node .

  class-methods READ_TXT_FILE
    importing
      !IV_PATH type STRING
    returning
      value(RV_TEXT) type STRING .
  class-methods DOWNLOAD_AS_TEXT_FILE
    importing
      !IV_FILE_PATH type STRING
      !IV_TEXT_CONTENT type STRING .
  class-methods PARSE_JSON_TO_INTERNAL_TABLE
    importing
      !IV_JSON type STRING
    exporting
      !ET_NODE type TT_SORTED_NODE
      !EV_NODE_NUMBER type INT4 .
  class-methods START_BACKUP
    importing
      !IV_REPO type CHAR4 .
  class-methods DOWNLOAD_ISSUE
    importing
      !IV_REPO_NAME type CHAR4
      !IV_ISSUE_NUMBER type INT4
      !IV_FOLDER_NAME type STRING .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_level,
        level     TYPE i,
        indicator TYPE string,
      END OF ty_level .
  types:
    tt_level TYPE STANDARD TABLE OF ty_level WITH KEY level .
  types:
    BEGIN OF ty_node,
        node_type TYPE string,
        prefix    TYPE string,
        name      TYPE string,
        nsuri     TYPE string,
        value     TYPE string,
        value_raw TYPE xstring,
      END OF ty_node .
  types:
    tt_node TYPE TABLE OF ty_node .

  constants GC_JSON_OPEN_ELEMENT type STRING value 'open element' ##NO_TEXT.
  constants GC_JSON_ATTRIBUTE type STRING value 'attribute' ##NO_TEXT.
  constants GC_JSON_CLOSE_ELEMENT type STRING value 'close element' ##NO_TEXT.
  constants GC_JSON_VALUE type STRING value 'value' ##NO_TEXT.
  constants GC_JSON_ERROR type STRING value 'Error' ##NO_TEXT.
  class-data SV_URL type STRING .
  class-data SV_MAX_NUMBER_IN_DB type INT4 .
  class-data SV_REPO_SHORT_NAME type CHAR4 .
  class-data SV_MAX_NUMBER_REACHED type ABAP_BOOL .

  class-methods WRITE_TO_DB
    importing
      !IT_SORTED_NODE type TT_SORTED_NODE
      !IV_ISSUE_NUM type INT4 .
  class-methods GET_NEXT_PAGE
    importing
      !IT_HEADER type TIHTTPNVP
    returning
      value(RV_NEXT_PAGE_URL) type STRING .
  class-methods PARSE_JSON_TO_RAW_TABLE
    importing
      !IV_JSON type STRING
    exporting
      !ET_NODE type TT_NODE
    exceptions
      JSON_PARSE_ERROR .
  class-methods SORT_RAW_TABLE
    importing
      !IT_NODE type TT_NODE
    exporting
      !ET_SORTED_NODE type TT_SORTED_NODE
      !EV_NODE_NUMBER type INT4 .
  class-methods BACKUP_GIVEN_URL
    importing
      !IV_URL type STRING .
  class-methods HANDLE_HTTP_RESPONSE
    importing
      !IV_JSON type STRING .
ENDCLASS.



CLASS CL_ABAP_GIT_ISSUE_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_TOOL=>BACKUP_GIVEN_URL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_URL                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD backup_given_url.
    DATA:lo_http_client TYPE REF TO if_http_client,
         lv_status      TYPE i,
         lt_fields      TYPE tihttpnvp,
         lv_sysubrc     TYPE sysubrc.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = iv_url
        proxy_host         = 'PROXY.WDF.SAP.CORP'
        proxy_service      = '8080'
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    ASSERT sy-subrc = 0.

    CALL METHOD lo_http_client->request->set_method( if_http_request=>co_request_method_get ).

    CALL METHOD lo_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    ASSERT sy-subrc = 0.

    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    IF sy-subrc <> 0.
      CALL METHOD lo_http_client->get_last_error
        IMPORTING
          code    = lv_sysubrc
          message = DATA(ev_message).
      WRITE: / |error occurred during receive data: { ev_message } | COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.

    DATA(lv_json) = lo_http_client->response->get_cdata( ).

    handle_http_response( lv_json ).
    lo_http_client->response->get_header_fields( CHANGING fields = lt_fields ).
    lo_http_client->close( ).

    DATA(lv_subsequent_page) = get_next_page( lt_fields ).
    IF lv_subsequent_page IS NOT INITIAL.
      backup_given_url( lv_subsequent_page ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_TOOL=>DOWNLOAD_AS_TEXT_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE_PATH                   TYPE        STRING
* | [--->] IV_TEXT_CONTENT                TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_as_text_file.
    DATA: lt_data_tab TYPE TABLE OF sdokcntasc.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_text_content
      TABLES
        ftext_tab = lt_data_tab.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename = iv_file_path
        codepage = '8400'
      CHANGING
        data_tab = lt_data_tab.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_TOOL=>DOWNLOAD_ISSUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_NAME                   TYPE        CHAR4
* | [--->] IV_ISSUE_NUMBER                TYPE        INT4
* | [--->] IV_FOLDER_NAME                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_issue.
    DATA: path TYPE string.

    SELECT SINGLE * INTO @DATA(ls_issue) FROM crmd_git_issue
       WHERE repo_name = @iv_repo_name AND issue_num = @iv_issue_number.
    CHECK sy-subrc = 0.

    path = | { iv_folder_name }\\{ iv_repo_name }\\{ iv_issue_number }.md|.
    download_as_text_file( iv_file_path = path iv_text_content = ls_issue-issue_body ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_TOOL=>GET_NEXT_PAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_HEADER                      TYPE        TIHTTPNVP
* | [<-()] RV_NEXT_PAGE_URL               TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_next_page.
    CHECK sv_max_number_reached = abap_false.
    READ TABLE it_header ASSIGNING FIELD-SYMBOL(<link>) WITH KEY
       name = 'link'.
    CHECK sy-subrc = 0.

    SPLIT <link>-value AT ';' INTO TABLE DATA(lt_page).
    READ TABLE lt_page ASSIGNING FIELD-SYMBOL(<next_page>) INDEX 1.
    CHECK sy-subrc = 0.
    rv_next_page_url = <next_page>.
    REPLACE ALL OCCURRENCES OF '<' IN rv_next_page_url WITH space.
    REPLACE ALL OCCURRENCES OF '>' IN rv_next_page_url WITH space.
    CONDENSE rv_next_page_url NO-GAPS.
    FIND 'page=' IN rv_next_page_url MATCH OFFSET DATA(lv_offset).
    ASSERT sy-subrc = 0.
    DATA(lv_len) = strlen( rv_next_page_url ) - lv_offset - 5.

    lv_offset = lv_offset + 5.
    DATA(next_page_number) = CONV int4( rv_next_page_url+lv_offset(lv_len) ).

    IF next_page_number = 1.
      CLEAR: rv_next_page_url.
    ELSE.
      WRITE:/ 'Next Page:', rv_next_page_url.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_TOOL=>HANDLE_HTTP_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD handle_http_response.
    CALL METHOD parse_json_to_internal_table
      EXPORTING
        iv_json        = iv_json
      IMPORTING
        et_node        = DATA(lt_node)
        ev_node_number = DATA(lv_number).

    CALL METHOD write_to_db
      EXPORTING
        it_sorted_node = lt_node
        iv_issue_num   = lv_number.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_TOOL=>PARSE_JSON_TO_INTERNAL_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* | [<---] ET_NODE                        TYPE        TT_SORTED_NODE
* | [<---] EV_NODE_NUMBER                 TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_json_to_internal_table.
    DATA lt_raw_node TYPE tt_node.

    CALL METHOD parse_json_to_raw_table
      EXPORTING
        iv_json          = iv_json
      IMPORTING
        et_node          = lt_raw_node
      EXCEPTIONS
        json_parse_error = 1
        OTHERS           = 2.

    ASSERT sy-subrc = 0.

    CALL METHOD sort_raw_table
      EXPORTING
        it_node        = lt_raw_node
      IMPORTING
        et_sorted_node = et_node
        ev_node_number = ev_node_number.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_TOOL=>PARSE_JSON_TO_RAW_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_JSON                        TYPE        STRING
* | [<---] ET_NODE                        TYPE        TT_NODE
* | [EXC!] JSON_PARSE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD parse_json_to_raw_table.
    DATA:
      node_wa TYPE ty_node.

    DATA(json) = cl_abap_codepage=>convert_to( iv_json ).
    DATA(reader) = cl_sxml_string_reader=>create( json ).

    TRY.
        DO.
          CLEAR node_wa.
          DATA(node) = reader->read_next_node( ).
          IF node IS INITIAL.
            EXIT.
          ENDIF.
          CASE node->type.
            WHEN if_sxml_node=>co_nt_element_open.
              DATA(open_element) = CAST if_sxml_open_element( node ).
              node_wa-node_type = gc_json_open_element.
              node_wa-prefix    = open_element->prefix.
              node_wa-name      = open_element->qname-name.
              node_wa-nsuri     = open_element->qname-namespace.
              DATA(attributes)  = open_element->get_attributes( ).
              APPEND node_wa TO et_node.
              LOOP AT attributes INTO DATA(attribute).
                node_wa-node_type = gc_json_attribute.
                node_wa-prefix    = attribute->prefix.
                node_wa-name      = attribute->qname-name.
                node_wa-nsuri     = attribute->qname-namespace.
                IF attribute->value_type = if_sxml_value=>co_vt_text.
                  node_wa-value = attribute->get_value( ).
                ELSEIF attribute->value_type =
                                   if_sxml_value=>co_vt_raw.
                  node_wa-value_raw = attribute->get_value_raw( ).
                ENDIF.
                APPEND node_wa TO et_node.
              ENDLOOP.
              CONTINUE.
            WHEN if_sxml_node=>co_nt_element_close.
              DATA(close_element) = CAST if_sxml_close_element( node ).
              node_wa-node_type   = gc_json_close_element.
              node_wa-prefix      = close_element->prefix.
              node_wa-name        = close_element->qname-name.
              node_wa-nsuri       = close_element->qname-namespace.
              APPEND node_wa TO et_node.
              CONTINUE.
            WHEN if_sxml_node=>co_nt_value.
              DATA(value_node) = CAST if_sxml_value_node( node ).
              node_wa-node_type   = gc_json_value.
              IF value_node->value_type = if_sxml_value=>co_vt_text.
                node_wa-value = value_node->get_value( ).
              ELSEIF value_node->value_type = if_sxml_value=>co_vt_raw.
                node_wa-value_raw = value_node->get_value_raw( ).
              ENDIF.
              APPEND node_wa TO et_node.
              CONTINUE.
            WHEN OTHERS.
              node_wa-node_type   = gc_json_error.
              APPEND node_wa TO et_node.
              EXIT.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error INTO DATA(parse_error).
        RAISE json_parse_error.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_TOOL=>READ_TXT_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PATH                        TYPE        STRING
* | [<-()] RV_TEXT                        TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_txt_file.

    DATA: l_filename TYPE string,
          l_rawtab   TYPE string_table,
          l_len      TYPE i.

    l_filename = iv_path.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = l_filename
        filetype                = 'ASC'
        codepage                = '8400'
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

    ASSERT sy-subrc = 0.

    LOOP AT l_rawtab ASSIGNING FIELD-SYMBOL(<line>).
      rv_text = rv_text && <line>.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_TOOL=>SORT_RAW_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_NODE                        TYPE        TT_NODE
* | [<---] ET_SORTED_NODE                 TYPE        TT_SORTED_NODE
* | [<---] EV_NODE_NUMBER                 TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sort_raw_table.
    DATA:
      ls_node              TYPE ty_node,
      lv_level_counter     TYPE i VALUE 0,
      lv_attribute_name    TYPE string,
      lv_seperator         TYPE char1,
      ls_sorted_node       TYPE ty_sorted_node,
      lv_node_counter      TYPE i VALUE 1,
      lv_node_flag_counter TYPE i,
      lt_level_tab         TYPE tt_level,
      ls_level_tab         TYPE ty_level,
      lv_index             TYPE i,
      lv_temp_counter      TYPE i.

    FIELD-SYMBOLS <fs_level_tab> TYPE ty_level.

    LOOP AT it_node INTO ls_node.
*Check if open element, if yes increase level counter
      IF  ls_node-node_type = gc_json_open_element.
        lv_level_counter = lv_level_counter + 1.

*Check if it is new node, if yes increase node counter
        IF lv_node_flag_counter IS NOT INITIAL AND lv_level_counter = lv_node_flag_counter.
          lv_node_counter = lv_node_counter + 1.
        ENDIF.

*Add level indicator to level table in order to remember which level we are in
        CLEAR ls_level_tab.
        READ TABLE lt_level_tab INTO ls_level_tab WITH TABLE KEY level = lv_level_counter.
        IF ls_level_tab IS INITIAL.
          ls_level_tab-level = lv_level_counter.
          APPEND ls_level_tab TO lt_level_tab.
        ENDIF.
      ENDIF.

*Check if attribute
      IF  ls_node-node_type = gc_json_attribute.
*If no entry in our generated result table then me mark current level as the begining of each node
        IF et_sorted_node IS INITIAL.
          lv_node_flag_counter = lv_level_counter - 1.
        ENDIF.

        LOOP AT lt_level_tab ASSIGNING <fs_level_tab> WHERE level = lv_level_counter.
          <fs_level_tab>-indicator =  ls_node-value.
        ENDLOOP.
      ENDIF.


*Check if value
*-------------------------------------------------------------------------
*Add level indicator to level table in order to show hierachy node
*For instance if we have following node hieracy
*   -A
*     -a
*     -b
*we wil have following naming convertion in our generated table
*  A-a  &  A-b
*-------------------------------------------------------------------------
      IF  ls_node-node_type = gc_json_value.
        CLEAR lv_attribute_name.
        LOOP AT lt_level_tab ASSIGNING <fs_level_tab> FROM 0 TO lv_level_counter.
          IF <fs_level_tab>-indicator IS NOT INITIAL.
            CONCATENATE lv_attribute_name '-' <fs_level_tab>-indicator INTO lv_attribute_name.
          ENDIF.
        ENDLOOP.

        CLEAR: lv_seperator, lv_index.
        lv_seperator = lv_attribute_name+0(1).
        IF lv_seperator = '-'.
          lv_index = strlen( lv_attribute_name ) - 1.
          lv_attribute_name = lv_attribute_name+1(lv_index).
        ENDIF.

        IF lv_attribute_name IS NOT INITIAL.
          ls_sorted_node-attribute = lv_attribute_name.
          ls_sorted_node-value =  ls_node-value.
          ls_sorted_node-index = lv_node_counter.
          APPEND ls_sorted_node TO et_sorted_node.
        ENDIF.
        CLEAR: ls_sorted_node.
      ENDIF.

*Check if close element
      IF  ls_node-node_type = gc_json_close_element.
        lv_level_counter = lv_level_counter - 1.

*Remove level indicator from level table
        DESCRIBE TABLE lt_level_tab LINES lv_temp_counter.
        LOOP AT lt_level_tab ASSIGNING <fs_level_tab> FROM lv_level_counter + 1 TO lv_temp_counter.
          <fs_level_tab>-indicator = ''.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

*Return total number of nodes
    ev_node_number = lv_node_counter.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_TOOL=>START_BACKUP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO                        TYPE        CHAR4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD start_backup.
    sv_repo_short_name = iv_repo.
    SELECT SINGLE * INTO @DATA(ls_repo) FROM crmd_git_repo
        WHERE repo_name = @iv_repo.

    ASSERT sy-subrc = 0.

    SELECT MAX( issue_num ) FROM crmd_git_issue INTO sv_max_number_in_db
       WHERE repo_name = iv_repo.
    backup_given_url( |https://api.github.com/repos/{ ls_repo-repo_owner }/{ ls_repo-repo_fullname }/issues| ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_TOOL=>WRITE_TO_DB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_SORTED_NODE                 TYPE        TT_SORTED_NODE
* | [--->] IV_ISSUE_NUM                   TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD write_to_db.
    DATA: ls_issue TYPE crmd_git_issue,
          lt_issue TYPE TABLE OF crmd_git_issue.
    DO iv_issue_num TIMES.
      IF sv_max_number_reached = abap_true.
         EXIT.
      ENDIF.
      CLEAR: ls_issue.
      LOOP AT it_sorted_node ASSIGNING FIELD-SYMBOL(<node>) WHERE index = sy-index.
        CASE <node>-attribute.
          WHEN 'number'.
            IF sv_max_number_in_db >= <node>-value.
               sv_max_number_reached = abap_true.
              EXIT.
            ENDIF.
            ls_issue-issue_num = <node>-value.
          WHEN 'title'.
            ls_issue-title = <node>-value.
          WHEN 'body'.
            ls_issue-issue_body = <node>-value.
          WHEN 'created_at'.
            ls_issue-created_at = <node>-value.
          WHEN 'updated_at'.
            ls_issue-updated_at = <node>-value.
        ENDCASE.
      ENDLOOP.
      IF ls_issue-issue_num IS NOT INITIAL.
        ls_issue-repo_name = sv_repo_short_name.
        ls_issue-mandt = sy-mandt.
        APPEND ls_issue TO lt_issue.
      ENDIF.
    ENDDO.

    IF lt_issue IS NOT INITIAL.
      INSERT crmd_git_issue FROM TABLE lt_issue.
    ENDIF.
  ENDMETHOD.
ENDCLASS.