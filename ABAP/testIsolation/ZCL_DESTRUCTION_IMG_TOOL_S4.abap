class ZCL_DESTRUCTION_IMG_TOOL_S4 definition
  public
  final
  create private .

public section.

  methods RUN .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_DESTRUCTION_IMG_TOOL_S4 .
protected section.
private section.

  class-data MT_WORKLIST type STRING_TABLE .
  class-data MO_INSTANCE type ref to ZCL_DESTRUCTION_IMG_TOOL_S4 .
  class-data MO_DEP_OBJ_DETECTOR type ref to ZIF_SOC_DEPENDENCY_DETECTOR .

  methods FILL_WORKLIST .
  methods DELETE
    importing
      !IV_SOCIAL_POST_ID type STRING .
  methods CONSTRUCTOR .
ENDCLASS.



CLASS ZCL_DESTRUCTION_IMG_TOOL_S4 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DESTRUCTION_IMG_TOOL_S4->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CONSTRUCTOR.
    CREATE OBJECT mo_dep_obj_detector TYPE lcl_prod_dep_detector .
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DESTRUCTION_IMG_TOOL_S4->DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SOCIAL_POST_ID              TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method DELETE.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_DESTRUCTION_IMG_TOOL_S4->FILL_WORKLIST
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method FILL_WORKLIST.
     CLEAR: mt_worklist.

     APPEND '20130001' TO mt_worklist.
     APPEND '20130002' TO mt_worklist.
     APPEND '20130003' TO mt_worklist.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_DESTRUCTION_IMG_TOOL_S4=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_DESTRUCTION_IMG_TOOL_S4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_INSTANCE.
     IF mo_instance IS INITIAL.
        CREATE OBJECT mo_instance.
     ENDIF.

     ro_instance = mo_instance.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_DESTRUCTION_IMG_TOOL_S4->RUN
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method RUN.
    DATA: lv_social_post_id TYPE string.

    fill_worklist( ).

    LOOP AT mt_worklist INTO lv_social_post_id.
       IF MO_DEP_OBJ_DETECTOR->dependent_object_existed( lv_social_post_id ) = abap_false.
          delete( lv_social_post_id ).
       ENDIF.
    ENDLOOP.

  endmethod.
ENDCLASS.

class lcl_Destruction_Test definition deferred.
class zcl_Destruction_Img_Tool_S4 definition local friends lcl_Destruction_Test.

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
*?<OBJECT_UNDER_TEST>ZCL_DESTRUCTION_IMG_TOOL_S4
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
      f_Cut type ref to zcl_Destruction_Img_Tool_S4.  "class under test

    methods: setup.
    methods: teardown.
    methods: run for testing.
endclass.       "lcl_Destruction_Test


class lcl_Destruction_Test implementation.

  method setup.


    f_cut = zcl_Destruction_Img_Tool_S4=>get_instance( ).
  endmethod.


  method teardown.



  endmethod.


  METHOD ZIF_SOC_DEPENDENCY_DETECTOR~DEPENDENT_OBJECT_EXISTED.
       WRITE: / 'Test Mock code to check dependent object existence for ID: ' , iv_social_post_id.
  ENDMETHOD.

  method run.

    f_cut->mo_dep_obj_detector = me.
    f_Cut->run(  ).
  endmethod.




endclass.
