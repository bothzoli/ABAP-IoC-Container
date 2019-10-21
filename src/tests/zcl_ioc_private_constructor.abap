CLASS zcl_ioc_private_constructor DEFINITION
  PUBLIC
  CREATE PRIVATE

  GLOBAL FRIENDS zcl_ioc_container.

  PUBLIC SECTION.

    INTERFACES zif_ioc_a.

  PRIVATE SECTION.

    METHODS
      constructor.

ENDCLASS.



CLASS zcl_ioc_private_constructor IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_IOC_PRIVATE_CONSTRUCTOR->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.
ENDCLASS.