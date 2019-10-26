CLASS zcx_ioc_container DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    CONSTANTS class_is_create_private TYPE sotr_conc VALUE '3' ##NO_TEXT. "A generated UUID will be here
    CONSTANTS error_in_type_determination TYPE sotr_conc VALUE '4' ##NO_TEXT. "A generated UUID will be here
    CONSTANTS object_is_not_instantiatable TYPE sotr_conc VALUE '1' ##NO_TEXT. "A generated UUID will be here

    METHODS constructor
      IMPORTING
        !textid LIKE textid OPTIONAL.

ENDCLASS.



CLASS zcx_ioc_container IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid = textid.
  ENDMETHOD.
ENDCLASS.