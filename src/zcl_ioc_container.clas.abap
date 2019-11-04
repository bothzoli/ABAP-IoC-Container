CLASS zcl_ioc_container DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(instance) TYPE REF TO zcl_ioc_container.

    METHODS:
      register
        IMPORTING
          !interface_name TYPE string
          !class_name     TYPE string
        RAISING
          zcx_ioc_container,
      deregister
        IMPORTING
          !interface_name TYPE string OPTIONAL,
      register_instance
        IMPORTING
          !interface_name TYPE string
          !instance       TYPE REF TO object,
      deregister_instance
        IMPORTING
          !interface_name TYPE string OPTIONAL,
      resolve
        IMPORTING
          !interface_name   TYPE string
        RETURNING
          VALUE(new_object) TYPE REF TO object.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_mapping_table.
    TYPES   interface_name  TYPE string.
    TYPES   class_name      TYPE string.
    TYPES END OF ts_mapping_table.
    TYPES:
      tt_mapping_table TYPE HASHED TABLE OF ts_mapping_table
        WITH UNIQUE KEY interface_name.

    TYPES:
      BEGIN OF ts_instance_mapping_table.
    TYPES   interface_name    TYPE string.
    TYPES   object_reference  TYPE REF TO object.
    TYPES END OF ts_instance_mapping_table.
    TYPES:
      tt_instance_mapping_table TYPE HASHED TABLE OF ts_instance_mapping_table
        WITH UNIQUE KEY interface_name.


    CONSTANTS:
      constructor_name   TYPE string VALUE `CONSTRUCTOR`,
      ioc_container_name TYPE string VALUE `ZCL_IOC_CONTAINER`.

    CLASS-DATA:
      container TYPE REF TO zcl_ioc_container.

    DATA:
      mapping_table          TYPE tt_mapping_table,
      instance_mapping_table TYPE tt_instance_mapping_table.

    METHODS:
      assert_creation_possible
        IMPORTING
          !class_name              TYPE string
        RETURNING
          VALUE(object_descriptor) TYPE REF TO cl_abap_objectdescr
        RAISING
          zcx_ioc_container,

      assert_obj_creation_possible
        IMPORTING
          !type_descriptor         TYPE REF TO cl_abap_typedescr
        RETURNING
          VALUE(object_descriptor) TYPE REF TO cl_abap_objectdescr
        RAISING
          zcx_ioc_container,

      check_create_private
        IMPORTING
          !type_descriptor TYPE REF TO cl_abap_typedescr
        RAISING
          zcx_ioc_container,

      check_not_instantiatable
        IMPORTING
          !object_descriptor TYPE REF TO cl_abap_objectdescr
        RAISING
          zcx_ioc_container,

      get_parameter_descriptor
        IMPORTING
          !object_descriptor          TYPE REF TO cl_abap_objectdescr
          !parameter_name             TYPE abap_parmname
        RETURNING
          VALUE(parameter_descriptor) TYPE REF TO cl_abap_datadescr,

      get_parameter_type_name
        IMPORTING
          !parameter_descriptor      TYPE REF TO cl_abap_datadescr
        RETURNING
          VALUE(parameter_type_name) TYPE string,

      get_registered_mapping
        IMPORTING
          !interface_name   TYPE string
        RETURNING
          VALUE(class_name) TYPE string,

      get_registered_object
        IMPORTING
          !interface_name          TYPE string
        RETURNING
          VALUE(registered_object) TYPE REF TO object,

      get_type_descriptor
        IMPORTING
          !class_name            TYPE string
        RETURNING
          VALUE(type_descriptor) TYPE REF TO cl_abap_typedescr.

ENDCLASS.



CLASS ZCL_IOC_CONTAINER IMPLEMENTATION.


  METHOD assert_creation_possible.

    DATA(type_descriptor) = get_type_descriptor( class_name ).

    IF type_descriptor IS NOT BOUND.
      RAISE EXCEPTION NEW zcx_ioc_container( zcx_ioc_container=>error_in_type_determination ).
    ENDIF.

    object_descriptor = assert_obj_creation_possible( type_descriptor ).

  ENDMETHOD.


  METHOD assert_obj_creation_possible.

    check_create_private( type_descriptor ).

    object_descriptor = CAST cl_abap_objectdescr( type_descriptor ).

    check_not_instantiatable( object_descriptor ).

  ENDMETHOD.


  METHOD check_create_private.

    DATA(class_descriptor) = CAST cl_abap_classdescr( type_descriptor ).

    IF class_descriptor->create_visibility NE cl_abap_objectdescr=>public.
      DATA(friends) = class_descriptor->get_friend_types( ).

      LOOP AT friends ASSIGNING FIELD-SYMBOL(<friend>).
        IF <friend>->get_relative_name( ) EQ ioc_container_name.
          RETURN.
        ENDIF.
      ENDLOOP.

      RAISE EXCEPTION NEW zcx_ioc_container( zcx_ioc_container=>class_is_create_private ).
    ENDIF.

  ENDMETHOD.


  METHOD check_not_instantiatable.

    IF object_descriptor->is_instantiatable( ) NE abap_true.
      RAISE EXCEPTION NEW zcx_ioc_container( zcx_ioc_container=>object_is_not_instantiatable ).
    ENDIF.

  ENDMETHOD.


  METHOD deregister.

    IF interface_name IS SUPPLIED.
      DELETE TABLE mapping_table
        WITH TABLE KEY interface_name = to_upper( interface_name ).
    ELSE.
      CLEAR mapping_table[].
    ENDIF.

  ENDMETHOD.


  METHOD deregister_instance.

    IF interface_name IS SUPPLIED.
      DELETE TABLE instance_mapping_table
        WITH TABLE KEY interface_name = to_upper( interface_name ).
    ELSE.
      CLEAR instance_mapping_table[].
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF container IS NOT BOUND.
      container = NEW #( ).
    ENDIF.
    instance = container.

  ENDMETHOD.


  METHOD get_parameter_descriptor.

    object_descriptor->get_method_parameter_type(
      EXPORTING
        p_method_name       = constructor_name
        p_parameter_name    = parameter_name
      RECEIVING
        p_descr_ref         = parameter_descriptor
      EXCEPTIONS
        OTHERS              = 1
    ).

    IF sy-subrc IS NOT INITIAL.
      " This should not happen as the method and parameter names
      " are returned from the object descriptor
      CLEAR parameter_descriptor.
    ENDIF.

  ENDMETHOD.


  METHOD get_parameter_type_name.

    IF parameter_descriptor->kind EQ cl_abap_typedescr=>kind_ref.
      DATA(reference_descriptor) = CAST cl_abap_refdescr( parameter_descriptor ).
      DATA(referenced_type) = reference_descriptor->get_referenced_type( ).

      IF referenced_type->kind EQ cl_abap_typedescr=>kind_class.
        DATA(parameter_obj_descriptor) = CAST cl_abap_objectdescr( referenced_type ).
        parameter_type_name  = parameter_obj_descriptor->get_relative_name( ).

      ELSEIF referenced_type->kind EQ cl_abap_typedescr=>kind_intf.
        DATA(interface_descriptor) = CAST cl_abap_intfdescr( referenced_type ).
        parameter_type_name = interface_descriptor->get_relative_name( ).
      ENDIF.
    ELSE.
      parameter_type_name = parameter_descriptor->get_relative_name( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_registered_mapping.

    class_name = VALUE #(
      mapping_table[
        interface_name = to_upper( interface_name )
      ]-class_name DEFAULT to_upper( interface_name ) ).

  ENDMETHOD.


  METHOD get_registered_object.

    registered_object = VALUE #( instance_mapping_table[
      interface_name = to_upper( interface_name )
      ]-object_reference OPTIONAL ).

  ENDMETHOD.


  METHOD get_type_descriptor.

    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = to_upper( class_name )
      RECEIVING
        p_descr_ref    = type_descriptor
      EXCEPTIONS
        OTHERS         = 1
    ).
    IF sy-subrc IS NOT INITIAL.
      CLEAR type_descriptor.
    ENDIF.

  ENDMETHOD.


  METHOD register.

    assert_creation_possible( to_upper( class_name ) ).

    ASSIGN mapping_table[ interface_name = to_upper( interface_name ) ]
      TO FIELD-SYMBOL(<mapping>).

    IF <mapping> IS ASSIGNED.
      <mapping>-class_name = to_upper( class_name ).
    ELSE.
      INSERT VALUE #(
        interface_name = to_upper( interface_name )
        class_name     = to_upper( class_name )
        ) INTO TABLE mapping_table.
    ENDIF.

  ENDMETHOD.


  METHOD register_instance.

    ASSIGN instance_mapping_table[ interface_name = to_upper( interface_name ) ]
      TO FIELD-SYMBOL(<instance_mapping>).

    IF <instance_mapping> IS ASSIGNED.
      <instance_mapping>-object_reference = instance.
    ELSE.
      INSERT VALUE #(
        interface_name    = to_upper( interface_name )
        object_reference  = instance
        ) INTO TABLE instance_mapping_table.
    ENDIF.

  ENDMETHOD.


  METHOD resolve.

    CHECK interface_name IS NOT INITIAL.

    new_object = get_registered_object( interface_name ).
    IF new_object IS BOUND.
      RETURN.
    ENDIF.

    DATA(class_name) = get_registered_mapping( interface_name ).
    DATA(type_descriptor) = get_type_descriptor( class_name ).

    IF type_descriptor IS NOT BOUND
      OR type_descriptor->kind NE cl_abap_typedescr=>kind_class.
      RETURN.
    ENDIF.

    TRY.
        DATA(object_descriptor) = assert_obj_creation_possible( type_descriptor ).
      CATCH zcx_ioc_container.
        RETURN.
    ENDTRY.

    DATA(constructor) = VALUE #( object_descriptor->methods[
        name = constructor_name
      ] OPTIONAL ).

    IF lines( constructor-parameters ) GT 0.
      DATA: ref TYPE REF TO data.
      FIELD-SYMBOLS: <fs> TYPE any.

      LOOP AT constructor-parameters INTO DATA(constructor_parameter).

        UNASSIGN <fs>.

        DATA(parameter_descriptor) = get_parameter_descriptor(
          object_descriptor = object_descriptor
          parameter_name    = constructor_parameter-name
        ).
        IF parameter_descriptor IS NOT BOUND.
          " This should not happen as the method and parameter names
          " are returned from the object descriptor
          CONTINUE.
        ENDIF.

        DATA(parameter_type_name) = get_parameter_type_name( parameter_descriptor ).

        IF constructor_parameter-type_kind EQ cl_abap_typedescr=>typekind_oref.
          CREATE DATA ref TYPE REF TO (parameter_type_name).
          ASSIGN ref->* TO <fs>.

          <fs> ?= resolve( parameter_type_name ).
        ELSE.
          CREATE DATA ref TYPE (parameter_type_name).
          ASSIGN ref->* TO <fs>.

        ENDIF.

        IF constructor_parameter-is_optional EQ abap_true
          AND ( <fs> IS NOT ASSIGNED OR <fs> IS INITIAL ).
          " Optional parameters that are "null" can be skipped
          CONTINUE.
        ENDIF.

        DATA(parameter_tab) = VALUE abap_parmbind_tab( (
          name  = constructor_parameter-name
          kind  = cl_abap_objectdescr=>exporting
          value = REF #( <fs> )
        ) ).

      ENDLOOP.

      CREATE OBJECT new_object
        TYPE (class_name)
        PARAMETER-TABLE parameter_tab.

    ELSE.

      CREATE OBJECT new_object
        TYPE (class_name).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
