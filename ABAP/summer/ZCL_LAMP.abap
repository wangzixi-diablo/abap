class ZCL_LAMP definition
  public
  final
  create public .

public section.

  interfaces ZIF_SWITCHABLE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_LAMP IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LAMP->ZIF_SWITCHABLE~OFF
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_SWITCHABLE~OFF.
    WRITE: / 'lamp off'.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LAMP->ZIF_SWITCHABLE~ON
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ZIF_SWITCHABLE~ON.
    WRITE: / 'lamp on'.
  endmethod.
ENDCLASS.