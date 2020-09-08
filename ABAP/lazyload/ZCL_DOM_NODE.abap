class ZCL_DOM_NODE definition
  public
  final
  create public .

public section.
  methods CONSTRUCTOR
    importing
      !IV_NODE_NAME type STRING .
protected section.
private section.
  data MV_NODE_NAME type STRING .
ENDCLASS.

CLASS ZCL_DOM_NODE IMPLEMENTATION.
method CONSTRUCTOR.
    mv_node_name = iv_node_name.
endmethod.
ENDCLASS.