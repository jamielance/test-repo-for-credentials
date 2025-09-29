*&---------------------------------------------------------------------*
*& Exception Class: ZCX_COS_PROCESSING_ERROR
*& Description: Exception class for COS processing errors
*&---------------------------------------------------------------------*
CLASS zcx_cos_processing_error DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF outbox_not_found,
        msgid TYPE symsgid VALUE 'ZCOS',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'GUID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF outbox_not_found,

      BEGIN OF mapping_not_found,
        msgid TYPE symsgid VALUE 'ZCOS',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'TRIGGER_GL',
        attr2 TYPE scx_attrname VALUE 'PRODUCT_CODE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF mapping_not_found,

      BEGIN OF document_creation_failed,
        msgid TYPE symsgid VALUE 'ZCOS',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'ERROR_MESSAGE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF document_creation_failed.

    DATA:
      guid           TYPE sysuuid_x16,
      trigger_gl     TYPE saknr,
      product_code   TYPE char20,
      error_message  TYPE string.

    METHODS:
      constructor
        IMPORTING
          textid   LIKE if_t100_message=>t100key OPTIONAL
          previous LIKE previous OPTIONAL
          guid     TYPE sysuuid_x16 OPTIONAL
          trigger_gl TYPE saknr OPTIONAL
          product_code TYPE char20 OPTIONAL
          error_message TYPE string OPTIONAL.

ENDCLASS.

CLASS zcx_cos_processing_error IMPLEMENTATION.

  METHOD constructor.
    super->constructor( textid = textid previous = previous ).
    me->guid = guid.
    me->trigger_gl = trigger_gl.
    me->product_code = product_code.
    me->error_message = error_message.
  ENDMETHOD.

ENDCLASS.

