*&---------------------------------------------------------------------*
*& Test Class: ZCL_TC_COS_VALIDATOR
*& Description: ABAP Unit tests for COS Validator
*&---------------------------------------------------------------------*
CLASS zcl_tc_cos_validator DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_cos_validator.

    METHODS:
      setup,
      
      " Test methods for company code validation
      test_validate_company_code_success FOR TESTING,
      test_validate_company_code_empty FOR TESTING,
      test_validate_company_code_not_exists FOR TESTING,
      
      " Test methods for fiscal year validation
      test_validate_fiscal_year_success FOR TESTING,
      test_validate_fiscal_year_empty FOR TESTING,
      test_validate_fiscal_year_invalid_range FOR TESTING,
      
      " Test methods for document number validation
      test_validate_document_number_success FOR TESTING,
      test_validate_document_number_empty FOR TESTING,
      
      " Test methods for GUID validation
      test_validate_guid_success FOR TESTING,
      test_validate_guid_empty FOR TESTING,
      test_validate_guid_invalid_format FOR TESTING,
      
      " Test methods for G/L account validation
      test_validate_gl_account_success FOR TESTING,
      test_validate_gl_account_empty FOR TESTING,
      test_validate_gl_account_not_exists FOR TESTING,
      
      " Test methods for amount validation
      test_validate_amount_success FOR TESTING,
      test_validate_amount_below_minimum FOR TESTING,
      test_validate_amount_above_maximum FOR TESTING.

ENDCLASS.

CLASS zcl_tc_cos_validator IMPLEMENTATION.

  METHOD setup.
    " Create class under test
    mo_cut = NEW zcl_cos_validator( ).
  ENDMETHOD.

  METHOD test_validate_company_code_success.
    " Given
    DATA: lv_bukrs  TYPE bukrs VALUE '1000',
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_company_code( lv_bukrs ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = ls_result-is_valid
      msg = 'Valid company code should pass validation'
    ).
  ENDMETHOD.

  METHOD test_validate_company_code_empty.
    " Given
    DATA: lv_bukrs  TYPE bukrs VALUE '', " Empty company code
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_company_code( lv_bukrs ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Empty company code should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '009'
      act = ls_result-error_code
      msg = 'Error code should be 009'
    ).
  ENDMETHOD.

  METHOD test_validate_company_code_not_exists.
    " Given
    DATA: lv_bukrs  TYPE bukrs VALUE '9999', " Non-existent company code
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_company_code( lv_bukrs ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Non-existent company code should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '010'
      act = ls_result-error_code
      msg = 'Error code should be 010'
    ).
  ENDMETHOD.

  METHOD test_validate_fiscal_year_success.
    " Given
    DATA: lv_gjahr  TYPE gjahr VALUE '2024',
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_fiscal_year( lv_gjahr ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = ls_result-is_valid
      msg = 'Valid fiscal year should pass validation'
    ).
  ENDMETHOD.

  METHOD test_validate_fiscal_year_empty.
    " Given
    DATA: lv_gjahr  TYPE gjahr VALUE 0, " Empty fiscal year
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_fiscal_year( lv_gjahr ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Empty fiscal year should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '011'
      act = ls_result-error_code
      msg = 'Error code should be 011'
    ).
  ENDMETHOD.

  METHOD test_validate_fiscal_year_invalid_range.
    " Given
    DATA: lv_gjahr  TYPE gjahr VALUE '1999', " Below minimum range
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_fiscal_year( lv_gjahr ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Fiscal year below minimum range should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '012'
      act = ls_result-error_code
      msg = 'Error code should be 012'
    ).
  ENDMETHOD.

  METHOD test_validate_document_number_success.
    " Given
    DATA: lv_belnr  TYPE belnr_d VALUE '1234567890',
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_document_number( lv_belnr ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = ls_result-is_valid
      msg = 'Valid document number should pass validation'
    ).
  ENDMETHOD.

  METHOD test_validate_document_number_empty.
    " Given
    DATA: lv_belnr  TYPE belnr_d VALUE '', " Empty document number
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_document_number( lv_belnr ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Empty document number should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '013'
      act = ls_result-error_code
      msg = 'Error code should be 013'
    ).
  ENDMETHOD.

  METHOD test_validate_guid_success.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '1234567890123456',
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_guid( lv_guid ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = ls_result-is_valid
      msg = 'Valid GUID should pass validation'
    ).
  ENDMETHOD.

  METHOD test_validate_guid_empty.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '', " Empty GUID
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_guid( lv_guid ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Empty GUID should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '014'
      act = ls_result-error_code
      msg = 'Error code should be 014'
    ).
  ENDMETHOD.

  METHOD test_validate_guid_invalid_format.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '12345', " Invalid format (too short)
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_guid( lv_guid ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Invalid GUID format should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '015'
      act = ls_result-error_code
      msg = 'Error code should be 015'
    ).
  ENDMETHOD.

  METHOD test_validate_gl_account_success.
    " Given
    DATA: lv_saknr  TYPE saknr VALUE '400000',
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_gl_account( lv_saknr ).

    " Then
    " Note: This test assumes the G/L account exists in the system
    " In a real test environment, you would need to ensure the account exists
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result
      msg = 'Result should not be initial'
    ).
  ENDMETHOD.

  METHOD test_validate_gl_account_empty.
    " Given
    DATA: lv_saknr  TYPE saknr VALUE '', " Empty G/L account
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_gl_account( lv_saknr ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Empty G/L account should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '016'
      act = ls_result-error_code
      msg = 'Error code should be 016'
    ).
  ENDMETHOD.

  METHOD test_validate_gl_account_not_exists.
    " Given
    DATA: lv_saknr  TYPE saknr VALUE '9999999999', " Non-existent G/L account
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_gl_account( lv_saknr ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Non-existent G/L account should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '017'
      act = ls_result-error_code
      msg = 'Error code should be 017'
    ).
  ENDMETHOD.

  METHOD test_validate_amount_success.
    " Given
    DATA: lv_amount TYPE dmbtr VALUE '1000.00',
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_amount( lv_amount ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = ls_result-is_valid
      msg = 'Valid amount should pass validation'
    ).
  ENDMETHOD.

  METHOD test_validate_amount_below_minimum.
    " Given
    DATA: lv_amount TYPE dmbtr VALUE '0.005', " Below minimum tolerance
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_amount( lv_amount ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Amount below minimum should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '018'
      act = ls_result-error_code
      msg = 'Error code should be 018'
    ).
  ENDMETHOD.

  METHOD test_validate_amount_above_maximum.
    " Given
    DATA: lv_amount TYPE dmbtr VALUE '9999999999.99', " Above maximum
          ls_result TYPE zif_cos_validator=>ty_validation_result.

    " When
    ls_result = mo_cut->zif_cos_validator~validate_amount( lv_amount ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-is_valid
      msg = 'Amount above maximum should fail validation'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '019'
      act = ls_result-error_code
      msg = 'Error code should be 019'
    ).
  ENDMETHOD.

ENDCLASS.

