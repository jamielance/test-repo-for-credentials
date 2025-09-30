"! Payment Processing Exception Class
"! Custom exception for payment processing errors
"! Provides structured error handling with context information
CLASS zcx_payment_processing_error DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Error constants for different failure scenarios
    CONSTANTS:
      "! No invoice categories found for payment run
      BEGIN OF no_invoice_categories,
        msgid TYPE symsgid VALUE 'ZPAYMENT',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'PAYMENT_RUN_ID',
      END OF no_invoice_categories,

      "! No document data found
      BEGIN OF no_document_data,
        msgid TYPE symsgid VALUE 'ZPAYMENT',
        msgno TYPE symsgno VALUE '002',
      END OF no_document_data,

      "! No invoice data found
      BEGIN OF no_invoice_data,
        msgid TYPE symsgid VALUE 'ZPAYMENT',
        msgno TYPE symsgno VALUE '003',
      END OF no_invoice_data,

      "! Invalid payment method
      BEGIN OF invalid_payment_method,
        msgid TYPE symsgid VALUE 'ZPAYMENT',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'PAYMENT_METHOD',
      END OF invalid_payment_method.

    "! Payment run ID for context
    DATA:
      payment_run_id TYPE reguh-laufi,
      "! Payment method for context
      payment_method TYPE zlsch.

    "! Constructor for exception creation
    "! @parameter textid | Message ID and number
    "! @parameter previous | Previous exception
    "! @parameter payment_run_id | Payment run ID for context
    "! @parameter payment_method | Payment method for context
    METHODS:
      constructor
        IMPORTING
          textid   LIKE if_t100_message=>t100key OPTIONAL
          previous LIKE previous OPTIONAL
          payment_run_id TYPE reguh-laufi OPTIONAL
          payment_method TYPE zlsch OPTIONAL.

ENDCLASS.

CLASS zcx_payment_processing_error IMPLEMENTATION.

  METHOD constructor.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->payment_run_id = payment_run_id.
    me->payment_method = payment_method.
  ENDMETHOD.

ENDCLASS.
