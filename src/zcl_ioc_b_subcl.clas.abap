CLASS zcl_ioc_b_subcl DEFINITION
  PUBLIC
  CREATE PUBLIC
  INHERITING FROM zcl_ioc_b_super.

  PUBLIC SECTION.

    DATA
      ioc_c TYPE REF TO zcl_ioc_c.

    METHODS
      constructor
        IMPORTING
          ioc_c TYPE REF TO zcl_ioc_c.


ENDCLASS.



CLASS ZCL_IOC_B_SUBCL IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->ioc_c = ioc_c.
  ENDMETHOD.
ENDCLASS.
