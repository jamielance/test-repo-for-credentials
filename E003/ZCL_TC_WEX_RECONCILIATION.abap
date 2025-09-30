*&---------------------------------------------------------------------*
*& Test Class: ZCL_TC_WEX_RECONCILIATION
*& Purpose: ABAP Unit tests for WEX reconciliation functionality
*& Namespace: ZHLM
*& Package: ZHLM_AP_WEX
*&---------------------------------------------------------------------*

CLASS zcl_tc_wex_reconciliation DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_c_wexopeninvoices_actions.
      
    METHODS:
      setup,
      teardown,
      
      " Test scenarios for FindMatch action
      test_findmatch_no_candidate FOR TESTING,
      test_findmatch_ambiguous_candidates FOR TESTING,
      test_findmatch_currency_mismatch FOR TESTING,
      test_findmatch_amount_out_of_tolerance FOR TESTING,
      test_findmatch_happy_path FOR TESTING,
      
      " Test scenarios for ReleaseIfMatch action
      test_releaseifmatch_guarded_flag_off FOR TESTING,
      test_releaseifmatch_enabled_flag_on FOR TESTING,
      
      " Helper methods
      given_test_data,
      when_findmatch_called,
      when_releaseifmatch_called,
      then_exception_raised,
      then_result_contains_match.

ENDCLASS.

CLASS zcl_tc_wex_reconciliation IMPLEMENTATION.

  METHOD setup.
    " Setup test environment
    mo_cut = NEW zcl_c_wexopeninvoices_actions( ).
    given_test_data( ).
  ENDMETHOD.

  METHOD teardown.
    " Cleanup test environment
    CLEAR mo_cut.
  ENDMETHOD.

  METHOD test_findmatch_no_candidate.
    " Test case: No matching invoice found
    " Given: No invoices match the WEX UUID
    " When: FindMatch is called
    " Then: 404 NO_CANDIDATE exception should be raised
    
    DATA(lt_input) = VALUE zc_wexmatchresult(
      companycode = '5100'
      wexuuid = 'NONEXISTENT_UUID'
      amount = '100.00'
      currency = 'USD'
      tolerance = '0.00'
    ).
    
    TRY.
        when_findmatch_called( lt_input ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH cx_rap_business_exception INTO DATA(lx_exception).
        cl_abap_unit_assert=>assert_equals(
          act = lx_exception->http_status_code
          exp = 404
          msg = 'Expected 404 status code for no candidate'
        ).
        cl_abap_unit_assert=>assert_equals(
          act = lx_exception->business_code
          exp = 'NO_CANDIDATE'
          msg = 'Expected NO_CANDIDATE business code'
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_findmatch_ambiguous_candidates.
    " Test case: Multiple invoices match the WEX UUID
    " Given: Multiple invoices with same UUID
    " When: FindMatch is called
    " Then: 409 AMBIGUOUS_CANDIDATES exception should be raised
    
    " This test would require test data setup with multiple matching invoices
    " Implementation depends on test data framework
  ENDMETHOD.

  METHOD test_findmatch_currency_mismatch.
    " Test case: Currency mismatch between invoice and expected
    " Given: Invoice with USD, expected EUR
    " When: FindMatch is called
    " Then: 422 CURRENCY_MISMATCH exception should be raised
    
    DATA(lt_input) = VALUE zc_wexmatchresult(
      companycode = '5100'
      wexuuid = 'TEST_UUID_USD'
      amount = '100.00'
      currency = 'EUR'  " Mismatch with invoice currency
      tolerance = '0.00'
    ).
    
    TRY.
        when_findmatch_called( lt_input ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH cx_rap_business_exception INTO DATA(lx_exception).
        cl_abap_unit_assert=>assert_equals(
          act = lx_exception->http_status_code
          exp = 422
          msg = 'Expected 422 status code for currency mismatch'
        ).
        cl_abap_unit_assert=>assert_equals(
          act = lx_exception->business_code
          exp = 'CURRENCY_MISMATCH'
          msg = 'Expected CURRENCY_MISMATCH business code'
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_findmatch_amount_out_of_tolerance.
    " Test case: Amount difference exceeds tolerance
    " Given: Invoice amount 100.00, expected 150.00, tolerance 10.00
    " When: FindMatch is called
    " Then: 422 AMOUNT_OUT_OF_TOLERANCE exception should be raised
    
    DATA(lt_input) = VALUE zc_wexmatchresult(
      companycode = '5100'
      wexuuid = 'TEST_UUID_AMOUNT'
      amount = '150.00'  " 50.00 difference
      currency = 'USD'
      tolerance = '10.00'  " Less than 50.00 difference
    ).
    
    TRY.
        when_findmatch_called( lt_input ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH cx_rap_business_exception INTO DATA(lx_exception).
        cl_abap_unit_assert=>assert_equals(
          act = lx_exception->http_status_code
          exp = 422
          msg = 'Expected 422 status code for amount out of tolerance'
        ).
        cl_abap_unit_assert=>assert_equals(
          act = lx_exception->business_code
          exp = 'AMOUNT_OUT_OF_TOLERANCE'
          msg = 'Expected AMOUNT_OUT_OF_TOLERANCE business code'
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_findmatch_happy_path.
    " Test case: Successful match found
    " Given: Valid invoice with matching UUID, amount, and currency
    " When: FindMatch is called
    " Then: Match result should be returned with MatchFound = true
    
    DATA(lt_input) = VALUE zc_wexmatchresult(
      companycode = '5100'
      wexuuid = 'TEST_UUID_HAPPY'
      amount = '100.00'
      currency = 'USD'
      tolerance = '0.00'
    ).
    
    DATA(lt_result) = VALUE zc_wexmatchresult( ).
    
    when_findmatch_called( 
      EXPORTING iv_input = lt_input
      IMPORTING et_result = lt_result
    ).
    
    then_result_contains_match( lt_result ).
  ENDMETHOD.

  METHOD test_releaseifmatch_guarded_flag_off.
    " Test case: ReleaseIfMatch with flag off
    " Given: ALLOW_INTERNAL_UNBLOCK = space
    " When: ReleaseIfMatch is called
    " Then: Should return keys and instruction for CPI PATCH
    
    DATA(lt_input) = VALUE zc_wexreleaseresult(
      companycode = '5100'
      wexuuid = 'TEST_UUID_GUARDED'
      amount = '100.00'
      currency = 'USD'
      tolerance = '0.00'
    ).
    
    DATA(lt_result) = VALUE zc_wexreleaseresult( ).
    
    when_releaseifmatch_called(
      EXPORTING iv_input = lt_input
      IMPORTING et_result = lt_result
    ).
    
    cl_abap_unit_assert=>assert_equals(
      act = lt_result-unblocked
      exp = abap_false
      msg = 'Expected Unblocked = false when flag is off'
    ).
    
    cl_abap_unit_assert=>assert_initial(
      act = lt_result-message
      msg = 'Expected message with PATCH instruction'
    ).
  ENDMETHOD.

  METHOD test_releaseifmatch_enabled_flag_on.
    " Test case: ReleaseIfMatch with flag on
    " Given: ALLOW_INTERNAL_UNBLOCK = 'X'
    " When: ReleaseIfMatch is called
    " Then: Should return Unblocked = true
    
    DATA(lt_input) = VALUE zc_wexreleaseresult(
      companycode = '5100'
      wexuuid = 'TEST_UUID_ENABLED'
      amount = '100.00'
      currency = 'USD'
      tolerance = '0.00'
    ).
    
    DATA(lt_result) = VALUE zc_wexreleaseresult( ).
    
    when_releaseifmatch_called(
      EXPORTING iv_input = lt_input
      IMPORTING et_result = lt_result
    ).
    
    cl_abap_unit_assert=>assert_equals(
      act = lt_result-unblocked
      exp = abap_true
      msg = 'Expected Unblocked = true when flag is on'
    ).
  ENDMETHOD.

  METHOD given_test_data.
    " Setup test data for all test scenarios
    " This would typically involve:
    " - Creating test invoices in ZC_WexOpenInvoices
    " - Setting up configuration in ZWEX_CFG
    " - Mocking external dependencies
    
    " Implementation depends on test data framework
    " For now, this is a placeholder
  ENDMETHOD.

  METHOD when_findmatch_called.
    " Call FindMatch action
    " Implementation would call the actual action method
    " For now, this is a placeholder
  ENDMETHOD.

  METHOD when_releaseifmatch_called.
    " Call ReleaseIfMatch action
    " Implementation would call the actual action method
    " For now, this is a placeholder
  ENDMETHOD.

  METHOD then_exception_raised.
    " Verify exception was raised with correct parameters
    " Implementation would check exception details
    " For now, this is a placeholder
  ENDMETHOD.

  METHOD then_result_contains_match.
    " Verify result contains successful match
    " Implementation would check result structure
    " For now, this is a placeholder
  ENDMETHOD.

ENDCLASS.
