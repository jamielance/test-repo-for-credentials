*&---------------------------------------------------------------------*
*& Class: ZCL_COS_VALIDATOR
*& Description: Implementation of COS validation functionality
*&---------------------------------------------------------------------*
CLASS zcl_cos_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_cos_validator.

  PRIVATE SECTION.
    CONSTANTS:
      c_min_amount TYPE dmbtr VALUE '0.01',
      c_max_amount TYPE dmbtr VALUE '999999999.99'.

ENDCLASS.

CLASS zcl_cos_validator IMPLEMENTATION.

  METHOD zif_cos_validator~validate_company_code.
    " Check if company code is not empty and exists
    IF iv_bukrs IS INITIAL.
      DATA(ls_message) = zcl_cos_message_utility=>get_company_code_required_error( ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    " Check if company code exists using standard VDM
    SELECT SINGLE CompanyCode FROM I_CompanyCode INTO @DATA(lv_company_code)
      WHERE CompanyCode = @iv_bukrs.
    
    IF sy-subrc <> 0.
      ls_message = zcl_cos_message_utility=>get_company_code_not_exists_error( iv_bukrs ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    rv_result = VALUE #( is_valid = abap_true ).
  ENDMETHOD.

  METHOD zif_cos_validator~validate_fiscal_year.
    " Check if fiscal year is not empty and valid
    IF iv_gjahr IS INITIAL.
      DATA(ls_message) = zcl_cos_message_utility=>get_fiscal_year_required_error( ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    " Check if fiscal year is within reasonable range
    IF iv_gjahr < 2000 OR iv_gjahr > 2100.
      ls_message = zcl_cos_message_utility=>get_fiscal_year_invalid_error( iv_gjahr ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    rv_result = VALUE #( is_valid = abap_true ).
  ENDMETHOD.

  METHOD zif_cos_validator~validate_document_number.
    " Check if document number is not empty
    IF iv_belnr IS INITIAL.
      DATA(ls_message) = zcl_cos_message_utility=>get_document_number_required_error( ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    rv_result = VALUE #( is_valid = abap_true ).
  ENDMETHOD.

  METHOD zif_cos_validator~validate_guid.
    " Check if GUID is not empty and has correct format
    IF iv_guid IS INITIAL.
      DATA(ls_message) = zcl_cos_message_utility=>get_guid_required_error( ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    " Basic GUID format validation (16 characters)
    IF strlen( iv_guid ) <> 16.
      ls_message = zcl_cos_message_utility=>get_guid_invalid_format_error( ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    rv_result = VALUE #( is_valid = abap_true ).
  ENDMETHOD.

  METHOD zif_cos_validator~validate_gl_account.
    " Check if G/L account is not empty and exists
    IF iv_saknr IS INITIAL.
      DATA(ls_message) = zcl_cos_message_utility=>get_gl_account_required_error( ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    " Check if G/L account exists using standard VDM
    SELECT SINGLE GLAccount FROM I_GLAccount INTO @DATA(lv_gl_account)
      WHERE GLAccount = @iv_saknr.
    
    IF sy-subrc <> 0.
      ls_message = zcl_cos_message_utility=>get_gl_account_not_exists_error( iv_saknr ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    rv_result = VALUE #( is_valid = abap_true ).
  ENDMETHOD.

  METHOD zif_cos_validator~validate_amount.
    " Check if amount is within valid range
    IF iv_amount < c_min_amount.
      DATA(ls_message) = zcl_cos_message_utility=>get_amount_below_minimum_error(
        iv_amount = iv_amount
        iv_minimum = c_min_amount
      ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    IF iv_amount > c_max_amount.
      ls_message = zcl_cos_message_utility=>get_amount_exceeds_maximum_error(
        iv_amount = iv_amount
        iv_maximum = c_max_amount
      ).
      rv_result = VALUE #(
        is_valid = abap_false
        error_code = ls_message-msgno
        error_text = ls_message-msgv1
      ).
      RETURN.
    ENDIF.

    rv_result = VALUE #( is_valid = abap_true ).
  ENDMETHOD.

ENDCLASS.

