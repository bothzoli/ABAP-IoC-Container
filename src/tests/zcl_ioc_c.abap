CLASS zcl_ioc_c DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    DATA:
      name TYPE string,
      age  TYPE int4.

    METHODS
      constructor
        IMPORTING
          name TYPE string
          age  TYPE int4 OPTIONAL.

ENDCLASS.



CLASS zcl_ioc_c IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IOC_C->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] NAME                           TYPE        STRING
* | [--->] AGE                            TYPE        INT4(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    me->name = name.
    me->age = age.
  ENDMETHOD.
ENDCLASS.