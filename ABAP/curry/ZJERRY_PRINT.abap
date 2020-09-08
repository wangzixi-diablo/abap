FUNCTION ZJERRY_PRINT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_FIRST) TYPE  STRING OPTIONAL
*"     REFERENCE(IV_SECOND) TYPE  STRING OPTIONAL
*"     REFERENCE(IV_THIRD) TYPE  STRING OPTIONAL
*"----------------------------------------------------------------------

IF iv_first IS SUPPLIED.
   WRITE: / iv_first.
ENDIF.

IF iv_second IS SUPPLIED.
   WRITE: / iv_second.
ENDIF.

IF iv_third IS SUPPLIED.
   WRITE: / iv_third.
ENDIF.

ENDFUNCTION.