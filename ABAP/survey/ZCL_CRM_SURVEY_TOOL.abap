class ZCL_CRM_SURVEY_TOOL definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods SUBMIT
    importing
      !IV_ANSWER_LIST type STRING
    returning
      value(RV_RESPONSE_MESSAGE) type STRING .
  methods GET_SURVEY_ANSWER
    returning
      value(RV_RESULT) type STRING .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_question,
        answer TYPE string_table,
      END OF ty_question .
  types:
    tt_question TYPE TABLE OF ty_question .

  data MV_QUESTION_TEMPLATE type STRING .
  constants CV_GUID_PATTERN type STRING value '.*svyValueGuid(?:.*)value="(.*)">.*svyValueVersion.*' ##NO_TEXT.
  constants CV_APP_ID type CRM_SVY_DB_APPL_ID value 'CRM_SURVEY_SERVICE' ##NO_TEXT.
  constants CV_SURVEY_ID type STRING value 'ZDIS_SERVICE_FEEDBACK' ##NO_TEXT.
  constants CV_SURVEY_VERSION type STRING value '0000000011' ##NO_TEXT.
  constants CV_Q1 type STRING value 'survey/result/q1_last_buy_date/q1_last_buy_date=' ##NO_TEXT.
  constants CV_Q2 type STRING value 'survey/result/q2_service_attitude/q2_service_attitude=' ##NO_TEXT.
  constants CV_Q3 type STRING value 'survey/result/q3_service_level/q3_service_level=' ##NO_TEXT.
  constants CV_Q4 type STRING value 'survey/result/q4_service_engineer/q4_service_engineer=' ##NO_TEXT.
  constants CV_Q5 type STRING value 'survey/result/q5_response_time/q5_response_time=' ##NO_TEXT.
  constants CV_Q6 type STRING value 'survey/result/q6_training/q6_training=' ##NO_TEXT.
  data MT_QUESTION_LIST type TT_QUESTION .

  methods ASSEMBLE_REQUEST_BODY
    importing
      !IV_ANSWER_LIST type STRING
      !IV_GUID type CRMT_OBJECT_GUID
    returning
      value(RV_REQUEST_BODY) type STRING .
  methods GET_NEW_SURVEY_INSTANCE_GUID
    importing
      !IV_TEMPLATE type STRING
    returning
      value(RV_GUID) type CRMT_OBJECT_GUID .
  methods GET_REQUEST_PAYLOAD_HEADER
    importing
      !IV_VALUE_GUID type CRMT_OBJECT_GUID
    returning
      value(RV_RESULT) type STRING .
  methods GET_SURVEY_TEMPLATE
    returning
      value(RV_TEMPLATE) type STRING .
ENDCLASS.



CLASS ZCL_CRM_SURVEY_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CRM_SURVEY_TOOL->ASSEMBLE_REQUEST_BODY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ANSWER_LIST                 TYPE        STRING
* | [--->] IV_GUID                        TYPE        CRMT_OBJECT_GUID
* | [<-()] RV_REQUEST_BODY                TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assemble_request_body.
    DATA: lv_name TYPE string.
    FIELD-SYMBOLS:<answer_prefix> TYPE string.

    CONSTANTS: cv_question TYPE string VALUE 'CV_Q'.

    rv_request_body = get_request_payload_header( iv_guid ).

    SPLIT iv_answer_list AT ',' INTO TABLE DATA(lt_answer).

    LOOP AT lt_answer ASSIGNING FIELD-SYMBOL(<answer>).
      DATA(lv_index) = sy-tabix.
      READ TABLE mt_question_list ASSIGNING FIELD-SYMBOL(<question_answer_id>) INDEX lv_index.
      " CV_Q1
      lv_name = |CV_Q{ lv_index }|.
      ASSIGN (lv_name) TO <answer_prefix>.
      CHECK sy-subrc = 0.

      READ TABLE <question_answer_id>-answer ASSIGNING FIELD-SYMBOL(<answer_id>) INDEX <answer>.
      rv_request_body = rv_request_body && <answer_prefix> && <answer_id> && '&'.
    ENDLOOP.

    "RV_REQUEST_BODY = RV_REQUEST_BODY && CV_q1_buy_date.
    rv_request_body = rv_request_body && '&onInputProcessing=SUBMIT'.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_SURVEY_TOOL->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    DATA: ls_question TYPE ty_question.
* question1 :
    APPEND 'a1o_1_one_week_before' TO ls_question-answer.
    APPEND 'a1o_2_one_month_before' TO ls_question-answer.
    APPEND 'a1o_3_twothree_month_before' TO ls_question-answer.
    APPEND 'a1o_4_half_year_before' TO ls_question-answer.
    APPEND 'a1o_5_one_year_before' TO ls_question-answer.
    APPEND 'a1o_6_more_than_year' TO ls_question-answer.

    APPEND ls_question TO mt_question_list.
    CLEAR: ls_question.

* question2 :
    APPEND 'a2o_very_satisfied' TO ls_question-answer.
    APPEND 'a2o_satisfied' TO ls_question-answer.
    APPEND 'a2o_just_soso' TO ls_question-answer.
    APPEND 'a2o_not_satisfied' TO ls_question-answer.
    APPEND 'a2o_very_unsatisfied' TO ls_question-answer.

    APPEND ls_question TO mt_question_list.
    CLEAR: ls_question.

* question3
    APPEND 'a3o_very_satisfied' TO ls_question-answer.
    APPEND 'a3o_satisfied' TO ls_question-answer.
    APPEND 'a3o_just_soso' TO ls_question-answer.
    APPEND 'a3o_not_satisfied' TO ls_question-answer.
    APPEND 'a3o_very_unsatisfied' TO ls_question-answer.

    APPEND ls_question TO mt_question_list.
    CLEAR: ls_question.

* question4
    APPEND 'a4o_very_satisfied' TO ls_question-answer.
    APPEND 'a4o_satisfied' TO ls_question-answer.
    APPEND 'a4o_just_soso' TO ls_question-answer.
    APPEND 'a4o_not_satisfied' TO ls_question-answer.
    APPEND 'a4o_very_unsatisfied' TO ls_question-answer.
    APPEND ls_question TO mt_question_list.
    CLEAR: ls_question.


* question5
    APPEND 'a5o_very_satisfied' TO ls_question-answer.
    APPEND 'a5o_satisfied' TO ls_question-answer.
    APPEND 'a5o_just_soso' TO ls_question-answer.
    APPEND 'a5o_not_satisfied' TO ls_question-answer.
    APPEND 'a5o_very_unsatisfied' TO ls_question-answer.
    APPEND ls_question TO mt_question_list.
    CLEAR: ls_question.

* question6
    APPEND 'a6o_very_satisfied' TO ls_question-answer.
    APPEND 'a6o_satisfied' TO ls_question-answer.
    APPEND 'a6o_just_soso' TO ls_question-answer.
    APPEND 'a6o_not_satisfied' TO ls_question-answer.
    APPEND 'a6o_very_unsatisfied' TO ls_question-answer.
    APPEND ls_question TO mt_question_list.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CRM_SURVEY_TOOL->GET_NEW_SURVEY_INSTANCE_GUID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TEMPLATE                    TYPE        STRING
* | [<-()] RV_GUID                        TYPE        CRMT_OBJECT_GUID
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_new_survey_instance_guid.

    TRY.
        DATA(lo_regex) = NEW cl_abap_regex( pattern = cv_guid_pattern ).

        DATA(lo_matcher) = lo_regex->create_matcher( EXPORTING text = iv_template ).

        IF lo_matcher->match( ) <> abap_true.
          WRITE:/ 'fail in input scan!'.
          RETURN.
        ENDIF.

        DATA(lt_reg_match_result) = lo_matcher->find_all( ).

        READ TABLE lt_reg_match_result ASSIGNING FIELD-SYMBOL(<match>) INDEX 1.

        READ TABLE <match>-submatches ASSIGNING FIELD-SYMBOL(<sub>) INDEX 1..

        rv_guid = iv_template+<sub>-offset(<sub>-length).

      CATCH cx_root INTO DATA(cx_root).
        WRITE:/ cx_root->get_text( ).
        RETURN.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CRM_SURVEY_TOOL->GET_REQUEST_PAYLOAD_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VALUE_GUID                  TYPE        CRMT_OBJECT_GUID
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_request_payload_header.
    rv_result = 'svyApplicationId=' && cv_app_id && '&SurveyId=' && cv_survey_id && '&svySurveyId='
    && cv_survey_id && '&svyVersion=' && cv_survey_version && '&'
    && 'SchemaVersion=1&svySchemaVersion=1&svyLanguage=EN&conid=&svyValueGuid='
    && iv_value_guid && '&svyValueVersion=0000000001&svyMandatoryMessage='
    && 'Fill all mandatory fields before saving&'.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_SURVEY_TOOL->GET_SURVEY_ANSWER
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_RESULT                      TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_survey_answer.
    DATA: lt_result TYPE zdis_question_feedback_t,
          ls_result TYPE ZDIS_QUESTION_FEEDBACK_result,
          ls_answer TYPE zdis_question_feedback.

    SELECT * INTO TABLE @DATA(lt_data) FROM zsurvey_value_agg.

    CHECK sy-subrc = 0.

    SELECT * INTO TABLE @DATA(lt_text) FROM zdis_survey_text.

    SORT lt_data BY question_index ASCENDING.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<data>).

      AT NEW question_index.
        CLEAR: ls_answer.
        DATA(lv_question_id) = to_upper( <data>-question ).

        READ TABLE lt_text ASSIGNING FIELD-SYMBOL(<question_text>)
          WITH KEY id = lv_question_id.
        ASSERT sy-subrc = 0.
        ls_answer-question_text = <question_text>-text.
      ENDAT.
      DATA(lv_answer_id) = to_upper( <data>-answer ).

      READ TABLE lt_text ASSIGNING FIELD-SYMBOL(<answer_text>)
       WITH KEY id = lv_answer_id.
      ASSERT sy-subrc = 0.

      APPEND <answer_text>-text  TO ls_answer-answer_text.
      APPEND <data>-answer_count TO ls_answer-answer_value.

      AT END OF question_index.
        APPEND ls_answer TO lt_result.
      ENDAT.
    ENDLOOP.

    ls_result-result = lt_result.

    rv_result = /UI2/CL_JSON=>serialize( ls_result ).


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_CRM_SURVEY_TOOL->GET_SURVEY_TEMPLATE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_TEMPLATE                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_survey_template.
    DATA: vguid    TYPE crm_svy_db_sv_guid.
    DATA: vvers    TYPE crm_svy_db_svers.
    DATA: svy_content TYPE string.
    DATA: ret TYPE bapiret1.
    DATA: apppar   TYPE TABLE OF crm_svy_db_sv_pair.
    DATA: lv_x TYPE xstring.
    DATA lr_conv TYPE REF TO cl_abap_conv_in_ce.

    CALL FUNCTION 'CRM_SVY_SURVEY_GET'
      EXPORTING
        application_id     = cv_app_id
        survey_id          = CONV crm_svy_db_sid( cv_survey_id )
        survey_version     = CONV crm_svy_db_svers( cv_survey_version )
        language           = 'E'
        media_type         = '01'
        parameter_xml      = 'CRM_SVY_BSP_SYSTEMPARAM.XML'
        values_guid        = vguid
        values_version     = vvers
      IMPORTING
        return             = ret
        content            = svy_content
      TABLES
        application_params = apppar.

    CALL FUNCTION 'CRM_SVY_DB_CONVERT_STRING2HEX'
      EXPORTING
        s = svy_content
      IMPORTING
        x = lv_x.

    CALL METHOD cl_abap_conv_in_ce=>create
      EXPORTING
        input = lv_x
      RECEIVING
        conv  = lr_conv.

    CALL METHOD lr_conv->read
      IMPORTING
        data = rv_template.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_SURVEY_TOOL->SUBMIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ANSWER_LIST                 TYPE        STRING
* | [<-()] RV_RESPONSE_MESSAGE            TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD submit.
    DATA: ret TYPE bapiret1.

* Step0: check answer list
    SPLIT iv_answer_list AT ',' INTO TABLE DATA(lt_answer).
    IF lines( lt_answer ) <> 6.
       rv_response_message = 'Invalid answer list, you must reply exactly all 6 questions'.
       RETURN.
    ENDIF.
* Step1: get Survey Template
    DATA(survey_template) = get_survey_template( ).

* Step2: create a new Survey instance guid

    DATA(survey_guid) = get_new_survey_instance_guid( survey_template ).

* Step3: assemble request body

    DATA(lv_request_body) = assemble_request_body( iv_answer_list = iv_answer_list
                                                   iv_guid      = survey_guid ).

* Step4: Submit survey
    CALL FUNCTION 'CRM_SVY_RESULT_DISPATCHER'
      EXPORTING
        survey_data = lv_request_body
      IMPORTING
        return      = ret.

    COMMIT WORK AND WAIT.

    rv_response_message = ret-message.

  ENDMETHOD.
ENDCLASS.