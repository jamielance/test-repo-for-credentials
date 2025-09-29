*&---------------------------------------------------------------------*
*& Interface: ZIF_COS_VALIDATOR
*& Description: Interface for COS validation functionality
*&---------------------------------------------------------------------*
INTERFACE zif_cos_validator.
  TYPES: BEGIN OF ty_validation_result,
           is_valid    TYPE abap_bool,
           error_code  TYPE msgno,
           error_text  TYPE string,
         END OF ty_validation_result.

  METHODS:
    validate_company_code
      IMPORTING
        iv_bukrs TYPE bukrs
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    validate_fiscal_year
      IMPORTING
        iv_gjahr TYPE gjahr
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    validate_document_number
      IMPORTING
        iv_belnr TYPE belnr_d
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    validate_guid
      IMPORTING
        iv_guid TYPE sysuuid_x16
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    validate_gl_account
      IMPORTING
        iv_saknr TYPE saknr
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    validate_amount
      IMPORTING
        iv_amount TYPE dmbtr
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result.

ENDINTERFACE.

