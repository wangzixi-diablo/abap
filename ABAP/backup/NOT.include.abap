*&---------------------------------------------------------------------*
*&  Include  NOT
*&---------------------------------------------------------------------*
DATA: number TYPE i.

SELECT-OPTIONS: not FOR number NO-DISPLAY.
"data: not type i.

DEFINE not.

  endif.

END-OF-DEFINITION.

CLASS not DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS: not

      IMPORTING i_not        TYPE any

      RETURNING VALUE(r_not) TYPE abap_bool.

ENDCLASS.

CLASS not IMPLEMENTATION.

  METHOD not.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.