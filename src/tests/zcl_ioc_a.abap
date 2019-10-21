CLASS zcl_ioc_a DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_ioc_a.

    DATA ioc_b TYPE REF TO zif_ioc_b.

    METHODS constructor
      IMPORTING
        !ioc_b TYPE REF TO zif_ioc_b.
ENDCLASS.



CLASS zcl_ioc_a IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IOC_A->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IOC_B                          TYPE REF TO ZIF_IOC_B
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    me->ioc_b = ioc_b.
  ENDMETHOD.
ENDCLASS.