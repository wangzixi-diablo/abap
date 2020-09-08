class ZCL_CRM_ATTACHMENTS_TOOL definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF lty_object_key,
        instid TYPE sibfboriid,
        typeid TYPE sibftypeid,
      END OF lty_object_key .
  types:
    ltty_object_key TYPE TABLE OF lty_object_key with key instid typeid .

  methods COMPARE_READ_RESULT
    importing
      !IT_ORIGIN type CRMT_ODATA_TASK_ATTACHMENTT
      !IT_JERRY type CRMT_ODATA_TASK_ATTACHMENTT
    returning
      value(RV_EQUAL) type ABAP_BOOL .
  methods GET_ATTACHMENTS_ORIGIN
    importing
      !IT_OBJECTS type LTTY_OBJECT_KEY
    returning
      value(RT_ATTACHMENTS) type CRMT_ODATA_TASK_ATTACHMENTT .
  methods START .
  methods STOP
    importing
      !IV_MESSAGE type STRING optional .
  methods COMPARE_LINK
    importing
      !IT_BP type OBL_T_LINK
      !IT_JERRY type OBL_T_LINK
    returning
      value(RV_EQUAL) type ABAP_BOOL .
  methods SEQUENTIAL_READ
    importing
      !IT_ORDERS type LTTY_OBJECT_KEY
    returning
      value(RT_ATTACHMENTS) type CRMT_ODATA_TASK_ATTACHMENTT .
  methods PARALLEL_READ
    importing
      !IV_BLOCK_SIZE type I
      !IT_ORDERS type LTTY_OBJECT_KEY
    returning
      value(RT_ATTACHMENTS) type CRMT_ODATA_TASK_ATTACHMENTT .
  methods READ_FINISHED
    importing
      !P_TASK type CLIKE .
  methods CONSTRUCTOR .
  methods GET_TESTDATA
    returning
      value(RT_DATA) type CRMT_OBJECT_KEY_T .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_order_atta_link,
        order_guid    TYPE sibfboriid,
        order_bortype TYPE sibftypeid,
        atta_type     TYPE skwf_ioty,
        attid_lo      TYPE sdok_docid,
        attid_ph      TYPE sdok_docid,
      END OF ty_order_atta_link .
  types:
    BEGIN OF ty_user_name,
        user_name TYPE bapibname-bapibname,
        fullname  TYPE ad_namtext,
      END OF ty_user_name .
  types:
    tt_user_name TYPE STANDARD TABLE OF ty_user_name WITH KEY user_name .
  types:
    tt_order_atta_link TYPE STANDARD TABLE OF ty_order_atta_link .

  data MT_ATTACHMENT_RESULT type CRMT_ODATA_TASK_ATTACHMENTT .
  data MT_GUID_FOR_TEST type CRMT_OBJECT_GUID_TAB .
  data MT_BP_TEST_DATA type LTTY_OBJECT_KEY .
  data MV_FINISHED type INT4 .
  data MT_USER_NAME type TT_USER_NAME .
  data MT_ORDER_ATTA_LINK type TT_ORDER_ATTA_LINK .
  data MV_START type I .
  data MV_END type I .
  data MV_REGULAR_TEST_NUM type INT4 value 10000 ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CRM_ATTACHMENTS_TOOL IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->COMPARE_LINK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_BP                          TYPE        OBL_T_LINK
* | [--->] IT_JERRY                       TYPE        OBL_T_LINK
* | [<-()] RV_EQUAL                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD COMPARE_LINK.
    CHECK lines( it_bp ) = lines( it_jerry ).

    LOOP AT it_bp ASSIGNING FIELD-SYMBOL(<bp>).
      READ TABLE it_jerry ASSIGNING FIELD-SYMBOL(<jerry>) WITH KEY brelguid = <bp>-brelguid.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF <bp>-instid_a <> <jerry>-instid_a OR <bp>-instid_b <> <jerry>-instid_b
         OR <bp>-utctime <> <jerry>-utctime.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_equal = abap_true.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->COMPARE_READ_RESULT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ORIGIN                      TYPE        CRMT_ODATA_TASK_ATTACHMENTT
* | [--->] IT_JERRY                       TYPE        CRMT_ODATA_TASK_ATTACHMENTT
* | [<-()] RV_EQUAL                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD COMPARE_READ_RESULT.
    CHECK lines( it_origin ) = lines( it_jerry ).

    LOOP AT it_origin ASSIGNING FIELD-SYMBOL(<origin>).
      READ TABLE it_jerry ASSIGNING FIELD-SYMBOL(<jerry>) WITH KEY
        documentid = <origin>-documentid.

      IF <jerry> <> <origin>.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_equal = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CONSTRUCTOR.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->GET_ATTACHMENTS_ORIGIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_OBJECTS                     TYPE        LTTY_OBJECT_KEY
* | [<-()] RT_ATTACHMENTS                 TYPE        CRMT_ODATA_TASK_ATTACHMENTT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_ATTACHMENTS_ORIGIN.
    DATA: lt_key_tab TYPE /iwbep/t_mgw_name_value_pair,
          ls_expand  TYPE crmt_odata_task_hdr_expanded.

    DATA(lo_tool) = NEW cl_crm_task_rt( ).

    LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<object>).
      CLEAR: lt_key_tab, ls_expand.

      DATA(ls_key) = VALUE /iwbep/s_mgw_name_value_pair( name = 'Guid'
        value = <object>-instid ).

      APPEND ls_key TO lt_key_tab.
      CALL METHOD lo_tool->get_task_attachments
        EXPORTING
          iv_entity_name     = space
          iv_entity_set_name = space
          iv_source_name     = space
          it_key_tab         = lt_key_tab
        IMPORTING
          et_task_expanded   = ls_expand.

      APPEND LINES OF ls_expand-attachments TO rt_attachments.
    ENDLOOP.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->GET_TESTDATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_DATA                        TYPE        CRMT_OBJECT_KEY_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD GET_TESTDATA.
    DATA: lt_guid TYPE STANDARD TABLE OF ztask_with_follo.

    SELECT * INTO TABLE lt_guid FROM ztask_with_follo.

    FIELD-SYMBOLS: <item> LIKE LINE OF mt_bp_test_data.

    LOOP AT lt_guid ASSIGNING FIELD-SYMBOL(<guid>).
      APPEND INITIAL LINE TO mt_bp_test_data ASSIGNING <item>.
      <item>-typeid = 'BUS2000125'.
      <item>-instid = <guid>-task_guid .
    ENDLOOP.

    rt_data = mt_bp_test_data.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->PARALLEL_READ
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BLOCK_SIZE                  TYPE        I
* | [--->] IT_ORDERS                      TYPE        LTTY_OBJECT_KEY
* | [<-()] RT_ATTACHMENTS                 TYPE        CRMT_ODATA_TASK_ATTACHMENTT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD PARALLEL_READ.
    DATA:lv_taskid        TYPE c LENGTH 8,
         lv_index         TYPE c LENGTH 4,
         lv_current_index TYPE int4,
         lt_task          LIKE it_orders,
         lt_attachment    TYPE crmt_odata_task_attachmentt.

* TODO: validation on iv_process_num and lines( it_orders )
    DATA(lv_total) = lines( it_orders ).
    DATA(lv_additional) = lv_total MOD iv_block_size.
    DATA(lv_task_num) = lv_total DIV iv_block_size.
    IF lv_additional <> 0.
       lv_task_num = lv_task_num + 1.
    ENDIF.
    DO lv_task_num TIMES.
      CLEAR: lt_task.
      lv_current_index = 1 +  iv_block_size * ( sy-index - 1 ).
      DO iv_block_size TIMES.
        READ TABLE it_orders ASSIGNING FIELD-SYMBOL(<task>) INDEX lv_current_index.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO lt_task ASSIGNING FIELD-SYMBOL(<cur_task>).
          MOVE-CORRESPONDING <task> TO <cur_task>.
          lv_current_index = lv_current_index + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      IF lt_task IS NOT INITIAL.
        lv_index = sy-index.
        lv_taskid = 'Task' && lv_index.
        CALL FUNCTION 'ZJERRYGET_ATTACHMENTS'
          STARTING NEW TASK lv_taskid
          CALLING read_finished ON END OF TASK
          EXPORTING
            it_objects = lt_task.
      ENDIF.
    ENDDO.

    WAIT UNTIL mv_finished = lv_task_num.

    rt_attachments = mt_attachment_result.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->READ_FINISHED
* +-------------------------------------------------------------------------------------------------+
* | [--->] P_TASK                         TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD READ_FINISHED.
    DATA: lt_attachment TYPE crmt_odata_task_attachmentt.

    ADD 1 TO mv_finished.
    RECEIVE RESULTS FROM FUNCTION 'ZJERRYGET_ATTACHMENTS'
    CHANGING
      ct_attachments              = lt_attachment
    EXCEPTIONS
      system_failure        = 1
      communication_failure = 2.

    APPEND LINES OF lt_attachment TO mt_attachment_result.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->SEQUENTIAL_READ
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ORDERS                      TYPE        LTTY_OBJECT_KEY
* | [<-()] RT_ATTACHMENTS                 TYPE        CRMT_ODATA_TASK_ATTACHMENTT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SEQUENTIAL_READ.
      rt_attachments = get_attachments_origin( it_orders ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->START
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD START.
    GET RUN TIME FIELD mv_start.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_CRM_ATTACHMENTS_TOOL->STOP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MESSAGE                     TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD STOP.
    GET RUN TIME FIELD mv_end.

    mv_end = mv_end - mv_start.

    DATA: lv_text TYPE string.

    IF iv_message IS SUPPLIED.
      lv_text = iv_message.
    ENDIF.

    lv_text = lv_text && ' consumed time: ' && mv_end.

    WRITE: / lv_text COLOR COL_NEGATIVE.
  ENDMETHOD.
ENDCLASS.