class ZCX_IOC_CONTAINER definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants OBJECT_IS_NOT_INSTANTIATABLE type SOTR_CONC value '40F2E9AFC4E81ED9BCB28B1FF8027679' ##NO_TEXT.
  constants CLASS_IS_CREATE_PRIVATE type SOTR_CONC value '40F2E9AFC4E81ED9BCB28B1FF8023679' ##NO_TEXT.
  constants ERROR_IN_TYPE_DETERMINATION type SOTR_CONC value '0894EF4577A91ED9BCFA5BB75839718B' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional .
ENDCLASS.



CLASS ZCX_IOC_CONTAINER IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid = textid.
  ENDMETHOD.
ENDCLASS.
