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



CLASS ZCL_IOC_C IMPLEMENTATION.


  METHOD constructor.
    me->name = name.
    me->age = age.
  ENDMETHOD.
ENDCLASS.
