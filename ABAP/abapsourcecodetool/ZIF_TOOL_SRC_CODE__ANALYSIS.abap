interface ZIF_TOOL_SRC_CODE__ANALYSIS
  public .


  methods ANALYZE
    importing
      !IS_OBJKEY type ZCL_TOOL_SRC_CODE_ANALYZE=>TY_MS_OBJKEY
      !IV_SUBOBJ type CSEQUENCE
      !IT_SOURCE type RSWSOURCET .
  methods GET_RESULT
    exporting
      !ET_LIST type TABLE .
  methods INIT
    exceptions
      ERROR .
endinterface.