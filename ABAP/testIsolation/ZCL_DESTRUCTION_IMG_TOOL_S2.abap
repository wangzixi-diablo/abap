class ZCL_DESTRUCTION_IMG_TOOL_S1 definition
  public
  create protected .

public section.

  methods RUN .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_DESTRUCTION_IMG_TOOL_S1 .
protected section.

  type-pools ABAP .
  methods DEPENDENT_OBJECT_EXISTED
    importing
      !IV_SOCIAL_POST_ID type STRING
    returning
      value(RV_DEP_OBJ_EXISTED) type ABAP_BOOL .
private section.

  class-data MT_WORKLIST type STRING_TABLE .
  class-data MO_INSTANCE type ref to ZCL_DESTRUCTION_IMG_TOOL_S1 .

  methods FILL_WORKLIST .
  methods DELETE
    importing
      !IV_SOCIAL_POST_ID type STRING .
ENDCLASS.



CLASS ZCL_DESTRUCTION_IMG_TOOL_S1 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DESTRUCTION_IMG_TOOL_S1->DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SOCIAL_POST_ID              TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DELETE.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_DESTRUCTION_IMG_TOOL_S1->DEPENDENT_OBJECT_EXISTED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SOCIAL_POST_ID              TYPE        STRING
* | [<-()] RV_DEP_OBJ_EXISTED             TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DEPENDENT_OBJECT_EXISTED.
     WRITE: / 'Productive code to check dependent object existence for ID: ' , iv_social_post_id.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DESTRUCTION_IMG_TOOL_S1->FILL_WORKLIST
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method FILL_WORKLIST.
     CLEAR: mt_worklist.

     APPEND '20130001' TO mt_worklist.
     APPEND '20130002' TO mt_worklist.
     APPEND '20130003' TO mt_worklist.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DESTRUCTION_IMG_TOOL_S1=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_DESTRUCTION_IMG_TOOL_S1
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_INSTANCE.
     IF mo_instance IS INITIAL.
        CREATE OBJECT mo_instance.
     ENDIF.

     ro_instance = mo_instance.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DESTRUCTION_IMG_TOOL_S1->RUN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method RUN.
    DATA: lv_social_post_id TYPE string.

    fill_worklist( ).

    LOOP AT mt_worklist INTO lv_social_post_id.
       IF dependent_object_existed( lv_social_post_id ) = abap_false.
          delete( lv_social_post_id ).
       ENDIF.
    ENDLOOP.

  endmethod.
ENDCLASS.


class lcl_Destruction_Test definition for testing
  duration short
     inheriting from ZCL_DESTRUCTION_IMG_TOOL_S1  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Destruction_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_DESTRUCTION_IMG_TOOL_S1
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>

  PUBLIC SECTION.
    class-methods: get_sub_instance RETURNING
      VALUE(ro_instance) TYPE REF TO lcl_Destruction_Test.
  PROTECTED SECTION.
   methods: dependent_object_existed REDEFINITION.

  private section.
    data:
      f_Cut type ref to lcl_Destruction_Test.  "class under test
    class-data: mo_instance TYPE REF TO lcl_Destruction_Test.

    class-methods: class_Setup.
    class-methods: class_Teardown.

    methods: setup.
    methods: teardown.
    methods: start FOR TESTING.

endclass.       "lcl_Destruction_Test


class lcl_Destruction_Test implementation.

  method get_sub_instance.
     IF mo_instance IS INITIAL.
        CREATE OBJECT mo_instance.
     ENDIF.

     ro_instance = mo_instance.
  endmethod.
  method class_Setup.

  endmethod.

  METHOD dependent_object_existed.
     WRITE: / 'Test mock code to check dependent object existence for ID: ' , iv_social_post_id.
  ENDMETHOD.


  method class_Teardown.



  endmethod.


  method setup.



  endmethod.


  method teardown.



  endmethod.


  method start.

    f_cut = lcl_Destruction_Test=>get_sub_instance( ).
    f_cut->run(  ).

  endmethod.




endclass.