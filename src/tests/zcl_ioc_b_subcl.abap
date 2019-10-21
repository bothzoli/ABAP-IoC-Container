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



CLASS zcl_ioc_b_subcl IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IOC_B_SUBCL->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IOC_C                          TYPE REF TO ZCL_IOC_C
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    super->constructor( ).
    me->ioc_c = ioc_c.
  ENDMETHOD.
ENDCLASS.