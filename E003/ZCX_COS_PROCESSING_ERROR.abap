*&---------------------------------------------------------------------*
*& Exception Class: ZCX_COS_PROCESSING_ERROR
*& Description: Exception class for COS processing errors
*&---------------------------------------------------------------------*
CLASS zcx_cos_processing_error DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Outbox not found error</p>
    "! <p>Error constant for when an outbox entry cannot be found
    "! using the provided GUID.</p>
    CONSTANTS:
      BEGIN OF outbox_not_found,
        "! <p class="shorttext synchronized">Message class</p>
        "! <p>Message class for COS errors</p>
        msgid TYPE symsgid VALUE 'ZCOS',
        "! <p class="shorttext synchronized">Message number</p>
        "! <p>Message number for outbox not found</p>
        msgno TYPE symsgno VALUE '001',
        "! <p class="shorttext synchronized">Attribute 1</p>
        "! <p>GUID attribute for error context</p>
        attr1 TYPE scx_attrname VALUE 'GUID',
        "! <p class="shorttext synchronized">Attribute 2</p>
        "! <p>Not used</p>
        attr2 TYPE scx_attrname VALUE '',
        "! <p class="shorttext synchronized">Attribute 3</p>
        "! <p>Not used</p>
        attr3 TYPE scx_attrname VALUE '',
        "! <p class="shorttext synchronized">Attribute 4</p>
        "! <p>Not used</p>
        attr4 TYPE scx_attrname VALUE '',
      END OF outbox_not_found,

      "! <p class="shorttext synchronized">Mapping not found error</p>
      "! <p>Error constant for when COS mapping cannot be found
      "! for the specified trigger G/L account and product code.</p>
      BEGIN OF mapping_not_found,
        "! <p class="shorttext synchronized">Message class</p>
        "! <p>Message class for COS errors</p>
        msgid TYPE symsgid VALUE 'ZCOS',
        "! <p class="shorttext synchronized">Message number</p>
        "! <p>Message number for mapping not found</p>
        msgno TYPE symsgno VALUE '002',
        "! <p class="shorttext synchronized">Attribute 1</p>
        "! <p>Trigger G/L account attribute</p>
        attr1 TYPE scx_attrname VALUE 'TRIGGER_GL',
        "! <p class="shorttext synchronized">Attribute 2</p>
        "! <p>Product code attribute</p>
        attr2 TYPE scx_attrname VALUE 'PRODUCT_CODE',
        "! <p class="shorttext synchronized">Attribute 3</p>
        "! <p>Not used</p>
        attr3 TYPE scx_attrname VALUE '',
        "! <p class="shorttext synchronized">Attribute 4</p>
        "! <p>Not used</p>
        attr4 TYPE scx_attrname VALUE '',
      END OF mapping_not_found,

      "! <p class="shorttext synchronized">Document creation failed error</p>
      "! <p>Error constant for when COS document creation fails
      "! during BAPI processing.</p>
      BEGIN OF document_creation_failed,
        "! <p class="shorttext synchronized">Message class</p>
        "! <p>Message class for COS errors</p>
        msgid TYPE symsgid VALUE 'ZCOS',
        "! <p class="shorttext synchronized">Message number</p>
        "! <p>Message number for document creation failed</p>
        msgno TYPE symsgno VALUE '004',
        "! <p class="shorttext synchronized">Attribute 1</p>
        "! <p>Error message attribute</p>
        attr1 TYPE scx_attrname VALUE 'ERROR_MESSAGE',
        "! <p class="shorttext synchronized">Attribute 2</p>
        "! <p>Not used</p>
        attr2 TYPE scx_attrname VALUE '',
        "! <p class="shorttext synchronized">Attribute 3</p>
        "! <p>Not used</p>
        attr3 TYPE scx_attrname VALUE '',
        "! <p class="shorttext synchronized">Attribute 4</p>
        "! <p>Not used</p>
        attr4 TYPE scx_attrname VALUE '',
      END OF document_creation_failed,

      "! <p class="shorttext synchronized">Feature toggle setup failed error</p>
      "! <p>Error constant for when feature toggle setup fails
      "! during TVARVC operations.</p>
      BEGIN OF feature_toggle_setup_failed,
        "! <p class="shorttext synchronized">Message class</p>
        "! <p>Message class for COS errors</p>
        msgid TYPE symsgid VALUE 'ZCOS',
        "! <p class="shorttext synchronized">Message number</p>
        "! <p>Message number for feature toggle setup failed</p>
        msgno TYPE symsgno VALUE '005',
        "! <p class="shorttext synchronized">Attribute 1</p>
        "! <p>Feature name attribute</p>
        attr1 TYPE scx_attrname VALUE 'FEATURE_NAME',
        "! <p class="shorttext synchronized">Attribute 2</p>
        "! <p>Not used</p>
        attr2 TYPE scx_attrname VALUE '',
        "! <p class="shorttext synchronized">Attribute 3</p>
        "! <p>Not used</p>
        attr3 TYPE scx_attrname VALUE '',
        "! <p class="shorttext synchronized">Attribute 4</p>
        "! <p>Not used</p>
        attr4 TYPE scx_attrname VALUE '',
      END OF feature_toggle_setup_failed.

    "! <p class="shorttext synchronized">GUID</p>
    "! <p>GUID of the outbox entry that caused the error</p>
    DATA:
      guid           TYPE sysuuid_x16,
      "! <p class="shorttext synchronized">Trigger G/L account</p>
      "! <p>Trigger G/L account that caused the error</p>
      trigger_gl     TYPE saknr,
      "! <p class="shorttext synchronized">Product code</p>
      "! <p>Product code that caused the error</p>
      product_code   TYPE char20,
      "! <p class="shorttext synchronized">Error message</p>
      "! <p>Detailed error message for the exception</p>
      error_message  TYPE string,
      "! <p class="shorttext synchronized">Feature name</p>
      "! <p>Feature name that caused the error</p>
      feature_name   TYPE string.

    "! <p class="shorttext synchronized">Constructor</p>
    "! <p>Creates a new COS processing error exception with
    "! optional context information and previous exception.</p>
    "! @parameter textid | <p class="shorttext synchronized">Message text ID (optional)</p>
    "! @parameter previous | <p class="shorttext synchronized">Previous exception (optional)</p>
    "! @parameter guid | <p class="shorttext synchronized">GUID context (optional)</p>
    "! @parameter trigger_gl | <p class="shorttext synchronized">Trigger G/L account context (optional)</p>
    "! @parameter product_code | <p class="shorttext synchronized">Product code context (optional)</p>
    "! @parameter error_message | <p class="shorttext synchronized">Error message context (optional)</p>
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

