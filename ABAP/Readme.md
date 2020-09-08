# 2014-03-08 17:10

Use FM TH_DISPLAY_USER_LIST to simulate SM04: get logon user list.

# 2017-02-21
1. useful tool to get callstack: CL_ABAP_GET_CALL_STACK
2. [CRQS](http://www.cnblogs.com/youxin/p/3149695.html)
3. Default value
```abap
DATA(ls3) = VALUE #( lt_data[ name = 'Spring2' ]
              DEFAULT VALUE #( name = 'SpringInvalid' value = 999 ) ).
```
4. check whether we are in IC - interaction center context
```abap
lt_page_instances = cl_bsp_context=>c_page_instances.
  READ TABLE lt_page_instances ASSIGNING <ic_instance>
     WITH KEY class = 'CL_BSP_WD_VIEW_MANAGER'.
  IF sy-subrc = 0.
    lv_ic_mode = 'X'.
  ENDIF.
``` 

# 2017-04-22

good tip: find //bas/745_COR/src/krn/abap/ -name "*.c" | xargs grep "IF_BADI_INTERFACE"

# 2017-05-07
1. txt1 = condense( txt2 && txt3 ).
2. String template: multiple line should be joined by &.

# 2017-05-21
try SSH - works without VPN

The method CHECK_TABLE_NAME_STR of the class CL_ABAP_DYN_PRG checks whether the database table specified exists and can be used.

I learned how to create an initial reference to interface today: demo_create_reference

# 2017-05-22
cannot connect to SAP git today :(
2017-05-22 16:40PM - changed a new version

# 2017-06-04

LET: define local variable in constructor expression

# 2017-10-07

There is no NULL value to be stored in an ABAP field. The IS NULL comparison is valid only for WHERE clause in SELECT statement. WHERE field = space is different from WHERE field IS NULL. A Database NULL value represents a field that has never been stored to database - this saving space, potentially.A NULL value in the database means that ORACLE will **NOT use physical storage space** for the value. NULL means there is nothing, not even an initial value stored.

## DDIC user

SAPSYS & DDIC are pre-defined R/3 system users. User DDIC is a user with special privileges in installation, software logistics, and the ABAP Dictionary. The user master record for the above id's is created in clients 000 and 001 when you first install your R/3 System
if a logon attempt fails, the dialog step for this logon will be saved with the internal user SAPSYS. Its simply used as a placeholder as long as the logon is not successful (because the system doesn't know if the user is valid one in this case).
You could check it by trying to logon with an incorrect password or not existing user. You will find a dialog steps for SAPSYS in STAD.

## Difference between two debuggers

Another difference might be due to the usage of the debugger: As long as 
you didn't get an exclusive debugging process, each single step in the 
debugger will result in an COMMIT WORK towards the database. Though this 
will have no influence on the SAP enqueues (locks), it will have a 
tremendous effect on the visibility of updated data on DB systems of the 
second type.

A substructure represented by a generic box. A polymorphic structure has a client-specific construction which is unknown statically. A polymorphic structure can either have no components or can be associated with the components of an append structure. Only dynamic access is possible to a polymorphic structure and its components. 

## difference between data and constant

One important hint is to declare variables with constant content as CONSTANTS and not
as DATA. Constants are stored in the program buffer once together with the program load,
variables declared with DATA are stored per user context. This is especially important for
INCLUDES with a high degree of reuse

## roll-in and roll-out

when user click /nse38 the dispatcher rolls your user context into an available dialog work process,which executes this task until the first screen comes up.The transaction is now waiting for your input.Because this work prrocess would be idle in the meantime, the system rolls out your user context so that another user can use the WP. If you performa any action on the screen, such as Pressing return key, the dispatcher will search for next available WP and rolls in your user context again, this time in the new WP.

# important table

* history table for ABAP workbench SE80: RSEUMOD

# multi-tenant

Cross-client access to databases is not permitted in application programs. For this reason, the addition USING CLIENT must not be used in application programs and no access to sy-mandt should take place. 

The addition WITH SYSTEM CLIENT PRIVILEGES can be used in system programs that require open access to any client. This addition is not permitted in application programs. 
The checks are deactivated only in the implementation of the method itself, and not in any units called from the method. 

# GTADIR

If the global GTABKEY switch in a system is switched ON, all table entries which are contained in a workbench or customizing request will be checked during the release process for conflicts with other software components and – if there are none – registered under the software component to which the workbench or customizing request belongs to.
The conflict check can be done manually before releasing the request via the menu Check à Syntax check or with STRG-F6 (Global key check) while displaying a request. Here, the table entries will not be registered.
If the GTABKEY-server is not available while releasing requests (RFC-problems, etc…), the entries are stored locally for later registration. The request can be released! If the conflict check is done manually before releasing the request, no error message is displayed.

# Generic box

A boxed component used to support multitenancy. Currently, a generic box can be used only as a component in an ABAP Dictionary structure. A generic box refers to **a polymorphic structure**. The box and its components **cannot** be addressed statically. Only dynamic access at program runtime is possible. 

# ? not supported in ABAP regular expression

[stackoverflow](https://stackoverflow.com/questions/22457596/greedy-non-greedy-quantifiers-in-abap-regular-expressions)

# Web service trace

SRT_UTIL

# ICF

* ICF controller: FM HTTP_DISPATCH_REQUEST
* ICF manager: CL_HTTP_SERVER
* IF_HTTP_EXTENSION~FLOW_RC controls the order in which the individual http request handlers are called. 
* Redirect could also be disabled:IF_HTTP_CLIENT~PROPERTYTYPE_REDIRECT.
* ICM and SAP web dispatcher shares the same code basis. Web dispatcher performs load balancing, and passes requests to ICM on the connected application servers rather than to work processes. ICM processes uses thread to parallelize the load (There is a thread control block inside the ICM). Thread control从thread pool里 wake一个thread响应TCP request. 工作线程包含IO handler,处理network input & output.
* Watch Dog: a work thread waits for the response, regardless of whether the worker thread is a server or a client. If a timeout occurs, the watchdog takes on the task of waiting for the response. This makes the worker thread available for other requests. When the watchdog receives the response, it informs the thread control components, which then call a work thread.

# Useful links

1. [good website](http://www.guidancetech.com/people/holland/sap/abap/) containing source code of ABAP ( most of them are basis )

2. Jerry's print: \\Cnctul000.ctul.sap.corp\restricted\ByD_SC\20_Technology\FieldExtension\75_Documents

# CRM Middleware

* ALE: Application Linking and Enabling
* IDOC: intermediate document

Transfer IDoc at once - Basis Type: CRMXIF_*_SAVE_M01
Message Type: CRMXIF_*_SAVE_M

BO can be sent in IDOC format using XIF - external interface.

# Adobe form

role ZCHENGDU AG3, Social post

# Mics
001. from Sean: 我们现在在中国大陆落地的云产品包括：ERP Cloud－SAP S/4HANA Cloud (ERP 云商务套件)
