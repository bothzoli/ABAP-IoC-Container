CLASS zcl_ioc_container_subclass DEFINITION
  PUBLIC
  INHERITING FROM zcl_ioc_container_abstract
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(instance) TYPE REF TO zcl_ioc_container_subclass.

  PROTECTED SECTION.

    METHODS:
      create_object REDEFINITION.

  PRIVATE SECTION.

    CLASS-DATA:
      container TYPE REF TO zcl_ioc_container_subclass.

ENDCLASS.



CLASS ZCL_IOC_CONTAINER_SUBCLASS IMPLEMENTATION.


  METHOD create_object.

    IF parameter_tab IS SUPPLIED.
      CREATE OBJECT new_object
        TYPE (class_name)
        PARAMETER-TABLE parameter_tab.
    ELSE.
      CREATE OBJECT new_object
        TYPE (class_name).
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF container IS NOT BOUND.
      container = NEW #( ).
    ENDIF.
    instance = container.

  ENDMETHOD.
ENDCLASS.
