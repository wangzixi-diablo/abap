class ZCL_DESTRUCTION_IMG_TOOL_S3 definition
  public
  final
  create private .

public section.

  class-methods RUN .
protected section.
private section.

  class-data MT_WORKLIST type STRING_TABLE .
  class-data MV_DETECTOR_TYPE_NAME type STRING value 'LCL_PROD_DEP_DETECTOR'. "#EC NOTEXT .  . " .
  class-data MO_DEP_OBJ_DETECTOR type ref to ZIF_SOC_DEPENDENCY_DETECTOR .

  class-methods FILL_WORKLIST .
  class-methods DELETE
    importing
      !IV_SOCIAL_POST_ID type STRING .
ENDCLASS.



CLASS ZCL_DESTRUCTION_IMG_TOOL_S3 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_DESTRUCTION_IMG_TOOL_S3=>DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SOCIAL_POST_ID              TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DELETE.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_DESTRUCTION_IMG_TOOL_S3=>FILL_WORKLIST
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method FILL_WORKLIST.
     CLEAR: mt_worklist.

     APPEND '20130001' TO mt_worklist.
     APPEND '20130002' TO mt_worklist.
     APPEND '20130003' TO mt_worklist.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DESTRUCTION_IMG_TOOL_S3=>RUN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method RUN.
    DATA: lv_social_post_id TYPE string.

    CREATE OBJECT mo_dep_obj_detector TYPE (MV_DETECTOR_TYPE_NAME).
    fill_worklist( ).

    LOOP AT mt_worklist INTO lv_social_post_id.
       IF mo_dep_obj_detector->dependent_object_existed( lv_social_post_id ) = abap_false.
          delete( lv_social_post_id ).
       ENDIF.
    ENDLOOP.

  endmethod.
ENDCLASS.

*"* use this source file for your ABAP unit test classes


CLASS lcl_Destruction_Test DEFINITION DEFERRED.

CLASS ZCL_DESTRUCTION_IMG_TOOL_S3 DEFINITION
            LOCAL FRIENDS lcl_Destruction_Test.

class lcl_Destruction_Test definition for testing
  duration short
  risk level harmless
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Destruction_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_DESTRUCTION_IMG_TOOL_S3
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  public section.
   INTERFACES: ZIF_SOC_DEPENDENCY_DETECTOR.
  private section.
    data:
      f_Cut type ref to zcl_Destruction_Img_Tool_S3.  "class under test

    methods: setup.
    methods: teardown.
    methods: run for testing.
endclass.       "lcl_Destruction_Test


class lcl_Destruction_Test implementation.

  method setup.


    create object f_Cut.
  endmethod.


  method teardown.



  endmethod.


  method run.

    zcl_destruction_img_tool_s3=>mv_detector_type_name = 'LCL_DESTRUCTION_TEST'.
    zcl_Destruction_Img_Tool_S3=>run(  ).
  endmethod.

  METHOD ZIF_SOC_DEPENDENCY_DETECTOR~DEPENDENT_OBJECT_EXISTED.
       WRITE: / 'Test Mock code to check dependent object existence for ID: ' , iv_social_post_id.
  ENDMETHOD.


endclass.
