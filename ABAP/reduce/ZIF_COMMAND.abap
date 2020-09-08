interface ZIF_COMMAND
  public .


  data NEXT type ref to ZIF_COMMAND .
  data RESULT_TYPE type STRING .

  methods DO .
  methods SET_TASK
    importing
      !IV_TASK type ANY
    returning
      value(RO_CMD) type ref to ZIF_COMMAND .
  methods GET_RESULT
    exporting
      !RV_RESULT type ANY .
endinterface.