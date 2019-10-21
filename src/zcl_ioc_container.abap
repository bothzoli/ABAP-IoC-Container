CLASS zcl_ioc_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      register
        IMPORTING
          !interface_name TYPE string
          !class_name     TYPE string
        RAISING
          zcx_ioc_container,
      deregister
        IMPORTING
          !interface_name TYPE string,
      register_instance
        IMPORTING
          !interface_name TYPE string
          !instance       TYPE REF TO object,
      deregister_instance
        IMPORTING
          !interface_name TYPE string,
      resolve
        IMPORTING
          !interface_name   TYPE string
        RETURNING
          VALUE(new_object) TYPE REF TO object.

  PRIVATE SECTION.

    CONSTANTS:
      constructor_name TYPE string VALUE `CONSTRUCTOR`.

    TYPES BEGIN OF ts_mapping_table.
    TYPES   interface_name  TYPE string.
    TYPES   class_name      TYPE string.
    TYPES END OF ts_mapping_table.

    TYPES tt_mapping_table TYPE HASHED TABLE OF ts_mapping_table
      WITH UNIQUE KEY interface_name.

    TYPES BEGIN OF ts_instance_mapping_table.
    TYPES   interface_name    TYPE string.
    TYPES   object_reference  TYPE REF TO object.
    TYPES END OF ts_instance_mapping_table.

    TYPES tt_instance_mapping_table TYPE HASHED TABLE OF ts_instance_mapping_table
      WITH UNIQUE KEY interface_name.

    DATA:
      mapping_table          TYPE tt_mapping_table,
      instance_mapping_table TYPE tt_instance_mapping_table.

    METHODS:

      assert_obj_create_is_possible
        IMPORTING
          class_name TYPE string
        RAISING
          zcx_ioc_container,

      check_create_private
        IMPORTING
          type_descriptor TYPE REF TO cl_abap_typedescr
        RAISING
          zcx_ioc_container,

      check_not_instantiatable
        IMPORTING
          object_descriptor TYPE REF TO cl_abap_objectdescr
        RAISING
          zcx_ioc_container,

      check_non_public_constructor
        IMPORTING
          object_descriptor TYPE REF TO cl_abap_objectdescr
        RAISING
          zcx_ioc_container,

      get_parameter_type_name
        IMPORTING
          parameter_descriptor       TYPE REF TO cl_abap_datadescr
        RETURNING
          VALUE(parameter_type_name) TYPE string.

ENDCLASS.



CLASS zcl_ioc_container IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_IOC_CONTAINER->ASSERT_OBJ_CREATE_IS_POSSIBLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] CLASS_NAME                     TYPE        STRING
* | [!CX!] ZCX_IOC_CONTAINER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD assert_obj_create_is_possible.

    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = to_upper( class_name )
      RECEIVING
        p_descr_ref    = DATA(type_descriptor)
      EXCEPTIONS
        OTHERS         = 1
    ).
    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION NEW zcx_ioc_container( zcx_ioc_container=>error_in_type_determination ).
    ENDIF.

    check_create_private( type_descriptor ).

    DATA(object_descriptor) = CAST cl_abap_objectdescr( type_descriptor ).
    check_not_instantiatable( object_descriptor ).

    check_non_public_constructor( object_descriptor ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_IOC_CONTAINER->CHECK_CREATE_PRIVATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] TYPE_DESCRIPTOR                TYPE REF TO CL_ABAP_TYPEDESCR
* | [!CX!] ZCX_IOC_CONTAINER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_create_private.

    DATA(class_descriptor) = CAST cl_abap_classdescr( type_descriptor ).
    IF class_descriptor->create_visibility NE cl_abap_objectdescr=>public.
      RAISE EXCEPTION NEW zcx_ioc_container( zcx_ioc_container=>class_is_create_private ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_IOC_CONTAINER->CHECK_NON_PUBLIC_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJECT_DESCRIPTOR              TYPE REF TO CL_ABAP_OBJECTDESCR
* | [!CX!] ZCX_IOC_CONTAINER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_non_public_constructor.

    DATA(constructor) = VALUE #( object_descriptor->methods[
        name = constructor_name
      ] DEFAULT VALUE #(
        visibility = cl_abap_objectdescr=>public
      ) ).
    IF constructor-visibility NE cl_abap_objectdescr=>public.
      RAISE EXCEPTION NEW zcx_ioc_container( zcx_ioc_container=>constructor_is_private ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_IOC_CONTAINER->CHECK_NOT_INSTANTIATABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJECT_DESCRIPTOR              TYPE REF TO CL_ABAP_OBJECTDESCR
* | [!CX!] ZCX_IOC_CONTAINER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_not_instantiatable.

    IF object_descriptor->is_instantiatable( ) NE abap_true.
      RAISE EXCEPTION NEW zcx_ioc_container( zcx_ioc_container=>object_is_not_instantiatable ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IOC_CONTAINER->DEREGISTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] INTERFACE_NAME                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deregister.

    DELETE TABLE mapping_table
      WITH TABLE KEY interface_name = to_upper( interface_name ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IOC_CONTAINER->DEREGISTER_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] INTERFACE_NAME                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deregister_instance.

    DELETE TABLE instance_mapping_table
      WITH TABLE KEY interface_name = to_upper( interface_name ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_IOC_CONTAINER->GET_PARAMETER_TYPE_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] PARAMETER_DESCRIPTOR           TYPE REF TO CL_ABAP_DATADESCR
* | [<-()] PARAMETER_TYPE_NAME            TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_parameter_type_name.

    DATA(reference_descriptor) = CAST cl_abap_refdescr( parameter_descriptor ).
    DATA(referenced_type) = reference_descriptor->get_referenced_type( ).

    IF referenced_type->kind EQ cl_abap_typedescr=>kind_class.
      DATA(parameter_obj_descriptor) = CAST cl_abap_objectdescr( referenced_type ).
      parameter_type_name  = parameter_obj_descriptor->get_relative_name( ).

    ELSEIF referenced_type->kind EQ cl_abap_typedescr=>kind_intf.
      DATA(interface_descriptor) = CAST cl_abap_intfdescr( referenced_type ).
      parameter_type_name = interface_descriptor->get_relative_name( ).

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IOC_CONTAINER->REGISTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] INTERFACE_NAME                 TYPE        STRING
* | [--->] CLASS_NAME                     TYPE        STRING
* | [!CX!] ZCX_IOC_CONTAINER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD register.

    assert_obj_create_is_possible( class_name ).

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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IOC_CONTAINER->REGISTER_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] INTERFACE_NAME                 TYPE        STRING
* | [--->] INSTANCE                       TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_IOC_CONTAINER->RESOLVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] INTERFACE_NAME                 TYPE        STRING
* | [<-()] NEW_OBJECT                     TYPE REF TO OBJECT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD resolve.

    new_object = VALUE #( instance_mapping_table[
      interface_name = to_upper( interface_name )
      ]-object_reference OPTIONAL ).

    IF new_object IS BOUND.
      RETURN.
    ENDIF.

    DATA(class_name) = VALUE #(
      mapping_table[
        interface_name = to_upper( interface_name )
      ]-class_name DEFAULT to_upper( interface_name ) ).

    CHECK class_name IS NOT INITIAL.

    cl_abap_classdescr=>describe_by_name(
      EXPORTING
        p_name         = class_name
      RECEIVING
        p_descr_ref    = DATA(new_type_descriptor)
      EXCEPTIONS
        OTHERS         = 1
    ).

    IF new_type_descriptor->kind NE cl_abap_typedescr=>kind_class
      OR sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(class_descriptor) = CAST cl_abap_classdescr( new_type_descriptor ).
    IF class_descriptor->create_visibility NE cl_abap_objectdescr=>public.
      RETURN.
    ENDIF.

    DATA(object_descriptor) = CAST cl_abap_objectdescr( new_type_descriptor ).
    IF object_descriptor->is_instantiatable( ) EQ abap_false.
      RETURN.
    ENDIF.

    DATA(constructor) = VALUE #( object_descriptor->methods[
        name = constructor_name
      ] DEFAULT VALUE #(
        visibility = cl_abap_objectdescr=>public
      ) ).
    IF constructor-visibility NE cl_abap_objectdescr=>public.
      RETURN.
    ENDIF.

    IF lines( constructor-parameters ) GT 0.

      FIELD-SYMBOLS: <fs> TYPE any.

      LOOP AT constructor-parameters INTO DATA(constructor_parameter).

        UNASSIGN <fs>.

        object_descriptor->get_method_parameter_type(
          EXPORTING
            p_method_name       = constructor-name
            p_parameter_name    = constructor_parameter-name
          RECEIVING
            p_descr_ref         = DATA(parameter_descriptor)
          EXCEPTIONS
            OTHERS              = 1
        ).

        IF sy-subrc IS NOT INITIAL.
          " This should not happen as the method and parameter names
          " are returned from the object descriptor
          CONTINUE.
        ENDIF.

        IF constructor_parameter-type_kind EQ cl_abap_typedescr=>typekind_oref.

          DATA(parameter_type_name) = get_parameter_type_name( parameter_descriptor ).

          DATA: ref TYPE REF TO data.
          CREATE DATA ref TYPE REF TO (parameter_type_name).
          ASSIGN ref->* TO <fs>.

          <fs> ?= resolve( parameter_type_name ).
        ELSE.
          DATA(type_name) = parameter_descriptor->get_relative_name( ).
          CREATE DATA ref TYPE (type_name).

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