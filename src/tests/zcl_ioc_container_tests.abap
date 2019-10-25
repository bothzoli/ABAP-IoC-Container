CLASS ltc_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      cut TYPE REF TO zcl_ioc_container.

    METHODS:
      setup,
      teardown,

      create_registered_instance  FOR TESTING
        RAISING
          zcx_ioc_container,
      create_existing_class       FOR TESTING
        RAISING
          zcx_ioc_container,
      setup_mapping_and_create    FOR TESTING
        RAISING
          zcx_ioc_container,
      setup_ml_mapping_and_create FOR TESTING
        RAISING
          zcx_ioc_container,
      setup_ll_mapping_and_create FOR TESTING
        RAISING
          zcx_ioc_container,
      no_mapping_returns_null     FOR TESTING
        RAISING
          zcx_ioc_container,
      create_private              FOR TESTING,
      create_private_friend       FOR TESTING
        RAISING
          zcx_ioc_container,
      not_instantiatable          FOR TESTING.
ENDCLASS.


CLASS ltc_test IMPLEMENTATION.

  METHOD setup.
    cut = zcl_ioc_container=>get_instance( ).
    cut->deregister( ).
    cut->deregister_instance( ).
  ENDMETHOD.


  METHOD create_registered_instance.

    DATA(base_interface_double) = CAST zif_ioc_a( cl_abap_testdouble=>create( `zif_ioc_a` ) ).

    cut->register_instance(
      interface_name = `zif_ioc_a`
      instance       = base_interface_double
    ).

    cl_abap_unit_assert=>assert_equals(
      act = cut->resolve( `zif_ioc_a` )
      exp = base_interface_double
    ).

    cut->register_instance(
      interface_name = `zif_ioc_a`
      instance       = NEW zcl_ioc_a( VALUE #( ) )
    ).

    DATA(ioc_a) = cut->resolve( `zif_ioc_a` ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_a IS INSTANCE OF zcl_ioc_a )
    ).

    cut->deregister_instance( `zif_ioc_a` ).

    cl_abap_unit_assert=>assert_not_bound(
      cut->resolve( `zif_ioc_a` )
    ).

    cut->register(
      interface_name = `zif_ioc_a`
      class_name     = `zcl_ioc_a`
    ).

    CLEAR ioc_a.
    ioc_a = cut->resolve( `zif_ioc_a` ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_a IS INSTANCE OF zcl_ioc_a )
    ).

  ENDMETHOD.


  METHOD create_existing_class.

    DATA(ioc_a) = cut->resolve( `zcl_ioc_a` ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_a IS INSTANCE OF zcl_ioc_a )
    ).

  ENDMETHOD.


  METHOD setup_mapping_and_create.

    cl_abap_unit_assert=>assert_not_bound(
      cut->resolve( `zif_ioc_b` )
    ).

    cut->register(
      interface_name = `zif_ioc_b`
      class_name     = `zcl_ioc_b_super`
    ).

    DATA(ioc_b_super) = cut->resolve( `zif_ioc_b` ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_b_super IS INSTANCE OF zcl_ioc_b_super )
    ).

    cut->register(
      interface_name = `zif_ioc_b`
      class_name     = `zcl_ioc_b_subcl`
    ).

    DATA(ioc_b_subcl) = cut->resolve( `zif_ioc_b` ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_b_subcl IS INSTANCE OF zcl_ioc_b_subcl )
    ).

    cut->deregister( `zif_ioc_b` ).

    cl_abap_unit_assert=>assert_not_bound(
      cut->resolve( `zif_ioc_b` )
    ).

  ENDMETHOD.


  METHOD setup_ml_mapping_and_create.

    cut->register(
      interface_name = `zif_ioc_b`
      class_name     = `zcl_ioc_b_super`
    ).

    cut->register(
      interface_name = `zif_ioc_a`
      class_name     = `zcl_ioc_a`
    ).

    DATA(ioc_a) = CAST zcl_ioc_a( cut->resolve( `zif_ioc_a` ) ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_a IS INSTANCE OF zcl_ioc_a )
    ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_a->ioc_b IS INSTANCE OF zcl_ioc_b_super )
    ).

  ENDMETHOD.


  METHOD setup_ll_mapping_and_create.

    cut->register(
      interface_name = `zif_ioc_b`
      class_name     = `zcl_ioc_b_subcl`
    ).

    DATA(ioc_a) = CAST zcl_ioc_a( cut->resolve( `zcl_ioc_a` ) ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_a IS INSTANCE OF zcl_ioc_a )
    ).

    cl_abap_unit_assert=>assert_bound( ioc_a->ioc_b ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_a->ioc_b IS INSTANCE OF zcl_ioc_b_subcl )
    ).

    DATA(ioc_b) = CAST zcl_ioc_b_subcl( ioc_a->ioc_b ).

    cl_abap_unit_assert=>assert_bound( ioc_b->ioc_c ).

    cl_abap_unit_assert=>assert_true(
      xsdbool( ioc_b->ioc_c IS INSTANCE OF zcl_ioc_c )
    ).

  ENDMETHOD.


  METHOD no_mapping_returns_null.

    cl_abap_unit_assert=>assert_not_bound( cut->resolve( `zif_ioc_a` ) ).

  ENDMETHOD.


  METHOD create_private.

    TRY.
        cut->register(
          interface_name = `zif_ioc_a`
          class_name     = `zcl_ioc_create_private`
        ).
        cl_abap_unit_assert=>fail( `Create private class should not be registered` ).
      CATCH zcx_ioc_container.

    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( cut->resolve( `zif_ioc_a` ) ).

  ENDMETHOD.


  METHOD create_private_friend.

    cut->register(
      interface_name = `zif_ioc_a`
      class_name     = `zcl_ioc_create_private_friend`
    ).

    DATA(create_private_friend) = cut->resolve( `zif_ioc_a` ).
    cl_abap_unit_assert=>assert_true(
      xsdbool( create_private_friend IS INSTANCE OF zcl_ioc_create_private_friend )
    ).

  ENDMETHOD.


  METHOD not_instantiatable.

    TRY.
        cut->register(
          interface_name = `zif_ioc_a`
          class_name     = `zcl_ioc_abstract`
        ).
        cl_abap_unit_assert=>fail( `Abstract class should not be registered` ).
      CATCH zcx_ioc_container.

    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( cut->resolve( `zcl_ioc_abstract` ) ).

    cl_abap_unit_assert=>assert_not_bound( cut->resolve( `zcl_ioc_abstract` ) ).

  ENDMETHOD.


  METHOD teardown.
    cut->deregister( ).
    cut->deregister_instance( ).
  ENDMETHOD.

ENDCLASS.