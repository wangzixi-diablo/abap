CLASS cl_abap_git_issue_image_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_image_reference,
        image_name TYPE string,
        image_url  TYPE string,
      END OF ty_image_reference .
    TYPES:
      tt_image_reference TYPE TABLE OF ty_image_reference WITH KEY image_name .
    TYPES:
      BEGIN OF ty_download_list,
        image_url  TYPE string,
        image_name TYPE string,
        repo_name  TYPE char4,
        issue_num  TYPE int4,
      END OF ty_download_list .
    TYPES:
      tt_download_list TYPE TABLE OF ty_download_list WITH KEY image_url .

    CONSTANTS cv_api_url TYPE string VALUE 'https://jerrylist.cfapps.eu10.hana.ondemand.com' ##NO_TEXT.

    CLASS-METHODS class_constructor .
    CLASS-METHODS get_image_reference
      IMPORTING
        !iv_issue_source_code TYPE string
      RETURNING
        VALUE(rt_image)       TYPE tt_image_reference .
    CLASS-METHODS get_image_ref_via_js_service
      IMPORTING
        !iv_issue_source_code TYPE string
      RETURNING
        VALUE(rt_image)       TYPE tt_image_reference .
    CLASS-METHODS start_backup
      IMPORTING
        !iv_repo_name TYPE char4 .
    CLASS-METHODS export_locally
      IMPORTING
        !iv_local_path TYPE string
        !iv_binary     TYPE xstring .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_image_parse_task,
        repo_name  TYPE char4,
        issue_num  TYPE int4,
        issue_body TYPE string,
      END OF ty_image_parse_task .
    TYPES:
      tt_image_parse_task TYPE TABLE OF ty_image_parse_task WITH KEY repo_name issue_num .

    CLASS-DATA sv_image_pattern TYPE string VALUE '(!\[.*\]\(.*\))' ##NO_TEXT.
    CLASS-DATA so_client TYPE REF TO if_http_client .

    CLASS-METHODS get_image_binary_data
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_data) TYPE xstring .
    CLASS-METHODS get_image_list_to_download
      IMPORTING
        !it_image_task       TYPE tt_image_parse_task
      RETURNING
        VALUE(rt_image_list) TYPE tt_download_list .
    CLASS-METHODS get_md_parse_task_list
      IMPORTING
        !iv_repo_name  TYPE char4
      RETURNING
        VALUE(rt_task) TYPE tt_image_parse_task .
    CLASS-METHODS download_image
      IMPORTING
        !it_download_list TYPE tt_download_list .
ENDCLASS.



CLASS CL_ABAP_GIT_ISSUE_IMAGE_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD class_constructor.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = cv_api_url
        proxy_host         = 'proxy.wdf.sap.corp'
        proxy_service      = '8080'
      IMPORTING
        client             = so_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    IF sy-subrc <> 0.
      WRITE:/ 'Error when creating client'.
      RETURN.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>DOWNLOAD_IMAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_DOWNLOAD_LIST               TYPE        TT_DOWNLOAD_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD download_image.
    DATA: ls_image_db TYPE crmd_git_image,
          lt_image_db TYPE TABLE OF crmd_git_image,
          lv_index    TYPE int4 VALUE 1.
    DATA(lv_total) = lines( it_download_list ).
    LOOP AT it_download_list INTO DATA(image) GROUP BY ( repo_name = image-repo_name )
      ASSIGNING FIELD-SYMBOL(<image>).
      "WRITE:/ | Issue for repo: { image-repo_name }| COLOR COL_NEGATIVE.
      CLEAR: lt_image_db.
      LOOP AT GROUP <image> ASSIGNING FIELD-SYMBOL(<element>).
        ls_image_db = CORRESPONDING #( <element> ).
        DATA(lv_text) = |file:{ ls_image_db-image_name } - { lv_index }/{ lv_total }|.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = lv_index * 100 / lv_total
            text       = lv_text.
        ls_image_db-image_index = lv_index.
        ls_image_db-image_binary_data = get_image_binary_data( <element>-image_url ).
        ADD 1 TO lv_index.
        APPEND ls_image_db TO lt_image_db.
      ENDLOOP.
      IF lt_image_db IS NOT INITIAL.
        INSERT crmd_git_image FROM TABLE lt_image_db.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>EXPORT_LOCALLY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LOCAL_PATH                  TYPE        STRING
* | [--->] IV_BINARY                      TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD export_locally.
    TYPES: BEGIN OF ts_line,
             data(1024) TYPE x,
           END OF ts_line.

    DATA: lv_size TYPE int4,
          lt_data TYPE STANDARD TABLE OF ts_line.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_binary
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_data.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize = lv_size
        filename     = iv_local_path
        filetype     = 'BIN'
        append       = space
      IMPORTING
        filelength   = lv_size
      CHANGING
        data_tab     = lt_data
      EXCEPTIONS
        OTHERS       = 01.

    ASSERT sy-subrc = 0.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>GET_IMAGE_BINARY_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_URL                         TYPE        STRING
* | [<-()] RV_DATA                        TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_image_binary_data.
    DATA: so_client TYPE REF TO if_http_client.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = iv_url
        proxy_host         = 'proxy.wdf.sap.corp'
        proxy_service      = '8080'
      IMPORTING
        client             = so_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    ASSERT sy-subrc = 0.
    so_client->request->set_method( 'GET' ).
    CALL METHOD so_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.
    ASSERT sy-subrc = 0.

    CALL METHOD so_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    ASSERT sy-subrc = 0.

    rv_data = so_client->response->get_data( ).
    so_client->close( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>GET_IMAGE_LIST_TO_DOWNLOAD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_IMAGE_TASK                  TYPE        TT_IMAGE_PARSE_TASK
* | [<-()] RT_IMAGE_LIST                  TYPE        TT_DOWNLOAD_LIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_image_list_to_download.
* repo name, issue number, issue_body
    LOOP AT it_image_task ASSIGNING FIELD-SYMBOL(<image_task>).
* Image file name, url
      DATA(lt_parsed_image) = get_image_ref_via_js_service( <image_task>-issue_body ).
      LOOP AT lt_parsed_image ASSIGNING FIELD-SYMBOL(<parsed_image>).
        DATA(ls_download_list) = CORRESPONDING ty_download_list( <parsed_image> ).
        ls_download_list-repo_name = <image_task>-repo_name.
        ls_download_list-issue_num = <image_task>-issue_num.
        APPEND ls_download_list TO rt_image_list.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>GET_IMAGE_REFERENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ISSUE_SOURCE_CODE           TYPE        STRING
* | [<-()] RT_IMAGE                       TYPE        TT_IMAGE_REFERENCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_image_reference.
    DATA: lv_reg_pattern TYPE string,
          lt_result_tab  TYPE match_result_tab.

    FIND ALL OCCURRENCES OF '![' IN iv_issue_source_code MATCH COUNT DATA(lv_count).
    CHECK lv_count > 0.
    lv_reg_pattern = sv_image_pattern.
*    IF lv_count > 1.
*      DO lv_count - 1 TIMES.
*        lv_reg_pattern = lv_reg_pattern && '.*' && sv_image_pattern.
*      ENDDO.
*    ENDIF.
    TRY.
        FIND REGEX lv_reg_pattern
             IN iv_issue_source_code
             RESULTS lt_result_tab.
      CATCH cx_root INTO DATA(cx_root).
        WRITE:/ cx_root->get_text( ).
        RETURN.
    ENDTRY.
    READ TABLE lt_result_tab ASSIGNING FIELD-SYMBOL(<result>) INDEX 1.
    CHECK sy-subrc = 0.

    LOOP AT <result>-submatches ASSIGNING FIELD-SYMBOL(<match>).
      WRITE:/ 'Match...........'.
      WRITE:/ iv_issue_source_code+<match>-offset(<match>-length).
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>GET_IMAGE_REF_VIA_JS_SERVICE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ISSUE_SOURCE_CODE           TYPE        STRING
* | [<-()] RT_IMAGE                       TYPE        TT_IMAGE_REFERENCE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_image_ref_via_js_service.
    DATA: lv_sysubrc LIKE sy-subrc,
          lv_message TYPE string,
          ls_data    LIKE LINE OF rt_image.
    so_client->request->set_method( 'POST' ).
    so_client->request->set_content_type('application/x-www-form-urlencoded').
    so_client->request->set_form_field( name = 'markdown_source' value = iv_issue_source_code ).
    CALL METHOD so_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    IF sy-subrc <> 0.
      CALL METHOD so_client->get_last_error
        IMPORTING
          code    = lv_sysubrc
          message = lv_message.

      so_client->close( ).
      WRITE:/ 'Error:', lv_message.
      RETURN.
    ENDIF.

    CALL METHOD so_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.

    IF sy-subrc <> 0.
      CALL METHOD so_client->get_last_error
        IMPORTING
          code    = lv_sysubrc
          message = lv_message.

      so_client->close( ).
      WRITE:/ 'Error:', lv_message.
      RETURN.
    ENDIF.

    lv_message = so_client->response->get_cdata( ).

    CALL METHOD cl_abap_git_issue_tool=>parse_json_to_internal_table
      EXPORTING
        iv_json        = lv_message
      IMPORTING
        et_node        = DATA(node)
        ev_node_number = DATA(lv_num).

    so_client->close( ).

    DO lv_num TIMES.
      CLEAR: ls_data.
      LOOP AT node ASSIGNING FIELD-SYMBOL(<node>) WHERE index = sy-index.
        CASE <node>-attribute.
          WHEN 'localFile'.
            ls_data-image_name = <node>-value.
          WHEN 'fileUrl'.
            ls_data-image_url = <node>-value.
        ENDCASE.
      ENDLOOP.
      IF ls_data-image_url IS NOT INITIAL.
        APPEND ls_data TO rt_image.
      ENDIF.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>GET_MD_PARSE_TASK_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_NAME                   TYPE        CHAR4
* | [<-()] RT_TASK                        TYPE        TT_IMAGE_PARSE_TASK
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_md_parse_task_list.
    DATA: lt_issue TYPE TABLE OF crmd_git_issue.
    SELECT * INTO TABLE lt_issue FROM crmd_git_issue WHERE repo_name = iv_repo_name.
    IF sy-subrc <> 0.
      WRITE:/ 'Please backup issue body first' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.

    SELECT * INTO TABLE @DATA(lt_image) FROM crmd_git_image WHERE repo_name = @iv_repo_name.

    LOOP AT lt_issue ASSIGNING FIELD-SYMBOL(<issue>).
      READ TABLE lt_image WITH KEY repo_name = <issue>-repo_name
         issue_num = <issue>-issue_num TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DATA(ls_task) = CORRESPONDING ty_image_parse_task( <issue> ).
        APPEND ls_task TO rt_task.
      ENDIF.
    ENDLOOP.

    CLEAR: lt_issue, lt_image.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method CL_ABAP_GIT_ISSUE_IMAGE_TOOL=>START_BACKUP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_NAME                   TYPE        CHAR4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD start_backup.

    DATA(lt_image_task) = get_md_parse_task_list( iv_repo_name ).

    DATA(lt_image_list) = get_image_list_to_download( lt_image_task ).

    download_image( lt_image_list ).
  ENDMETHOD.
ENDCLASS.