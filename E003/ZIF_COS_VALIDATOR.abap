*&---------------------------------------------------------------------*
*& Interface: ZIF_COS_VALIDATOR
*& Description: Interface for COS validation functionality
*&---------------------------------------------------------------------*
INTERFACE zif_cos_validator.
  "! <p class="shorttext synchronized">Validation result structure</p>
  "! <p>Contains the result of validation operations including
  "! validity status, error codes, and error messages.</p>
  TYPES: BEGIN OF ty_validation_result,
           "! <p class="shorttext synchronized">Validation status</p>
           "! <p>True if validation passed, false if failed</p>
           is_valid    TYPE abap_bool,
           "! <p class="shorttext synchronized">Error code</p>
           "! <p>Message number for the error (if validation failed)</p>
           error_code  TYPE msgno,
           "! <p class="shorttext synchronized">Error text</p>
           "! <p>Human-readable error message</p>
           error_text  TYPE string,
         END OF ty_validation_result.

  "! <p class="shorttext synchronized">Validate company code</p>
  "! <p>Validates that the company code exists and is active
  "! for COS processing operations.</p>
  "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code to validate</p>
  "! @parameter rv_result | <p class="shorttext synchronized">Validation result</p>
  METHODS:
    validate_company_code
      IMPORTING
        iv_bukrs TYPE bukrs
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    "! <p class="shorttext synchronized">Validate fiscal year</p>
  "! <p>Validates that the fiscal year is valid and open
  "! for posting operations.</p>
  "! @parameter iv_gjahr | <p class="shorttext synchronized">Fiscal year to validate</p>
  "! @parameter rv_result | <p class="shorttext synchronized">Validation result</p>
    validate_fiscal_year
      IMPORTING
        iv_gjahr TYPE gjahr
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    "! <p class="shorttext synchronized">Validate document number</p>
  "! <p>Validates that the document number is properly formatted
  "! and exists in the system.</p>
  "! @parameter iv_belnr | <p class="shorttext synchronized">Document number to validate</p>
  "! @parameter rv_result | <p class="shorttext synchronized">Validation result</p>
    validate_document_number
      IMPORTING
        iv_belnr TYPE belnr_d
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    "! <p class="shorttext synchronized">Validate GUID</p>
  "! <p>Validates that the GUID is properly formatted and not empty.
  "! Used for outbox entry identification.</p>
  "! @parameter iv_guid | <p class="shorttext synchronized">GUID to validate</p>
  "! @parameter rv_result | <p class="shorttext synchronized">Validation result</p>
    validate_guid
      IMPORTING
        iv_guid TYPE sysuuid_x16
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    "! <p class="shorttext synchronized">Validate G/L account</p>
  "! <p>Validates that the G/L account exists and is active
  "! for the specified company code.</p>
  "! @parameter iv_saknr | <p class="shorttext synchronized">G/L account to validate</p>
  "! @parameter rv_result | <p class="shorttext synchronized">Validation result</p>
    validate_gl_account
      IMPORTING
        iv_saknr TYPE saknr
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result,

    "! <p class="shorttext synchronized">Validate amount</p>
  "! <p>Validates that the amount is within acceptable ranges
  "! and properly formatted for COS processing.</p>
  "! @parameter iv_amount | <p class="shorttext synchronized">Amount to validate</p>
  "! @parameter rv_result | <p class="shorttext synchronized">Validation result</p>
    validate_amount
      IMPORTING
        iv_amount TYPE dmbtr
      RETURNING
        VALUE(rv_result) TYPE ty_validation_result.

ENDINTERFACE.

