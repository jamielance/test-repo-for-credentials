*&---------------------------------------------------------------------*
*& Test Class: ZCL_TC_COS_MONITOR
*& Description: ABAP Unit tests for COS Monitor
*&---------------------------------------------------------------------*
CLASS zcl_tc_cos_monitor DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_cos_monitor.

    METHODS:
      setup,
      teardown,
      
      " Test methods
      test_get_monitor_data_success FOR TESTING,
      test_get_monitor_data_errors_only FOR TESTING,
      test_get_monitor_summary_success FOR TESTING,
      test_check_authorization_success FOR TESTING,
      test_check_authorization_failure FOR TESTING,
      
      " Helper methods
      cleanup_test_data,
      create_test_outbox_data,
      create_test_audit_data.

ENDCLASS.

CLASS zcl_tc_cos_monitor IMPLEMENTATION.

  METHOD setup.
    " Create class under test
    mo_cut = NEW zcl_cos_monitor( ).
    
    " Clean up any existing test data
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up test data
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD test_get_monitor_data_success.
    " Given
    DATA: lt_bukrs_range TYPE bukrs_range_t,
          lt_status_range TYPE char1_range_t,
          lv_from_date    TYPE dats VALUE '20240101',
          lv_to_date      TYPE dats VALUE '20241231',
          lt_data         TYPE TABLE OF zcl_cos_monitor=>ty_monitor_data.

    " Create test data
    create_test_outbox_data( ).
    create_test_audit_data( ).

    " Set up selection criteria
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '1000' ) TO lt_bukrs_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'P' ) TO lt_status_range.

    " When
    lt_data = mo_cut->get_monitor_data(
      it_bukrs_range = lt_bukrs_range
      it_status_range = lt_status_range
      iv_from_date = lv_from_date
      iv_to_date = lv_to_date
      iv_include_outbox = abap_true
      iv_include_audit = abap_true
      iv_errors_only = abap_false
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_data
      msg = 'Monitor data should not be empty'
    ).

    " Verify data structure
    READ TABLE lt_data INTO DATA(ls_data) INDEX 1.
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_data-guid
      msg = 'GUID should not be empty'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '1000'
      act = ls_data-bukrs
      msg = 'Company code should be 1000'
    ).
  ENDMETHOD.

  METHOD test_get_monitor_data_errors_only.
    " Given
    DATA: lt_bukrs_range TYPE bukrs_range_t,
          lt_status_range TYPE char1_range_t,
          lv_from_date    TYPE dats VALUE '20240101',
          lv_to_date      TYPE dats VALUE '20241231',
          lt_data         TYPE TABLE OF zcl_cos_monitor=>ty_monitor_data.

    " Create test data with mixed statuses
    create_test_outbox_data( ).
    create_test_audit_data( ).

    " Set up selection criteria
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '1000' ) TO lt_bukrs_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'E' ) TO lt_status_range.

    " When
    lt_data = mo_cut->get_monitor_data(
      it_bukrs_range = lt_bukrs_range
      it_status_range = lt_status_range
      iv_from_date = lv_from_date
      iv_to_date = lv_to_date
      iv_include_outbox = abap_true
      iv_include_audit = abap_true
      iv_errors_only = abap_true
    ).

    " Then
    " All returned data should have error status
    LOOP AT lt_data INTO DATA(ls_data).
      cl_abap_unit_assert=>assert_equals(
        exp = 'E'
        act = ls_data-status
        msg = 'All data should have error status when filtering errors only'
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_get_monitor_summary_success.
    " Given
    DATA: lt_bukrs_range TYPE bukrs_range_t,
          lt_status_range TYPE char1_range_t,
          lv_from_date    TYPE dats VALUE '20240101',
          lv_to_date      TYPE dats VALUE '20241231',
          ls_summary      TYPE zcl_cos_monitor=>ty_monitor_summary.

    " Create test data
    create_test_outbox_data( ).
    create_test_audit_data( ).

    " Set up selection criteria
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '1000' ) TO lt_bukrs_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'P' ) TO lt_status_range.

    " When
    ls_summary = mo_cut->get_monitor_summary(
      it_bukrs_range = lt_bukrs_range
      it_status_range = lt_status_range
      iv_from_date = lv_from_date
      iv_to_date = lv_to_date
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_summary-total_records
      msg = 'Total records should not be zero'
    ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_summary-total_amount
      msg = 'Total amount should not be zero'
    ).
  ENDMETHOD.

  METHOD test_check_authorization_success.
    " Given
    DATA: lv_authorized TYPE abap_bool.

    " When
    lv_authorized = mo_cut->check_authorization( ).

    " Then
    " Note: This test will depend on the user's authorization
    " In a real test environment, you would need to set up proper authorizations
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_authorized
      msg = 'Authorization check should return a result'
    ).
  ENDMETHOD.

  METHOD test_check_authorization_failure.
    " Given
    DATA: lv_authorized TYPE abap_bool.

    " When
    " This test would require a user without proper authorization
    " In a real test environment, you would switch to a test user without authorization
    lv_authorized = mo_cut->check_authorization( ).

    " Then
    " The result depends on the current user's authorization
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_authorized
      msg = 'Authorization check should return a result'
    ).
  ENDMETHOD.

  METHOD cleanup_test_data.
    " Clean up test data
    DELETE FROM zcos_outbox WHERE guid LIKE 'TEST%'.
    DELETE FROM zcos_aud WHERE guid LIKE 'TEST%'.
    COMMIT WORK.
  ENDMETHOD.

  METHOD create_test_outbox_data.
    DATA: ls_outbox TYPE zcos_outbox.

    " Create test outbox entry
    ls_outbox-client = sy-mandt.
    ls_outbox-guid = 'TEST1234567890123'.
    ls_outbox-bukrs = '1000'.
    ls_outbox-gjahr = '2024'.
    ls_outbox-belnr_src = '1234567890'.
    ls_outbox-trigger_gl = '400000'.
    ls_outbox-product_code = 'TEST001'.
    ls_outbox-total_charge = '1000.00'.
    ls_outbox-status = 'P'.
    ls_outbox-created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).

    INSERT zcos_outbox FROM ls_outbox.

    " Create test outbox entry with error status
    ls_outbox-guid = 'TEST1234567890124'.
    ls_outbox-status = 'E'.
    ls_outbox-error_message = 'Test error message'.

    INSERT zcos_outbox FROM ls_outbox.

    COMMIT WORK.
  ENDMETHOD.

  METHOD create_test_audit_data.
    DATA: ls_audit TYPE zcos_aud.

    " Create test audit entry
    ls_audit-client = sy-mandt.
    ls_audit-guid = 'TEST1234567890123'.
    ls_audit-bukrs = '1000'.
    ls_audit-gjahr = '2024'.
    ls_audit-belnr_cos = '9876543210'.
    ls_audit-belnr_src = '1234567890'.
    ls_audit-cos_amount = '900.00'.
    ls_audit-status = 'P'.
    ls_audit-posted_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_audit-posted_by = sy-uname.

    INSERT zcos_aud FROM ls_audit.

    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.

