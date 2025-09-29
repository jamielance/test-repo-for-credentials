*&---------------------------------------------------------------------*
*& Test Class: ZCL_TC_COS_INTEGRATION
*& Description: Integration tests for COS Auto Posting
*&---------------------------------------------------------------------*
CLASS zcl_tc_cos_integration DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_feature_toggle TYPE REF TO zcl_cos_feature_toggle,
      mo_document_processor TYPE REF TO zcl_cos_document_processor,
      mo_qrfc_worker TYPE REF TO zcl_cos_qrfc_worker,
      mo_monitor TYPE REF TO zcl_cos_monitor.

    METHODS:
      setup,
      teardown,
      
      " Integration test methods
      test_end_to_end_cos_posting FOR TESTING,
      test_feature_toggle_integration FOR TESTING,
      test_monitor_integration FOR TESTING,
      
      " Helper methods
      cleanup_test_data,
      create_test_mapping_data,
      create_test_document_data
        RETURNING
          VALUE(rs_document) TYPE acdoca.

ENDCLASS.

CLASS zcl_tc_cos_integration IMPLEMENTATION.

  METHOD setup.
    " Create class instances
    mo_feature_toggle = NEW zcl_cos_feature_toggle( ).
    mo_document_processor = NEW zcl_cos_document_processor( ).
    mo_qrfc_worker = NEW zcl_cos_qrfc_worker( ).
    mo_monitor = NEW zcl_cos_monitor( ).
    
    " Clean up any existing test data
    cleanup_test_data( ).
    
    " Setup test data
    create_test_mapping_data( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up test data
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD test_end_to_end_cos_posting.
    " Given
    DATA: ls_document TYPE acdoca,
          ls_result   TYPE zcl_cos_document_processor=>ty_processing_result.

    " Create test document
    ls_document = create_test_document_data( ).

    " When - Process document
    ls_result = mo_document_processor->process_document( ls_document ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = ls_result-success
      msg = 'Document processing should succeed'
    ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-guid
      msg = 'GUID should be generated'
    ).

    " Verify outbox entry was created
    SELECT SINGLE guid FROM zcos_outbox
      INTO @DATA(lv_guid)
      WHERE guid = @ls_result-guid.
    
    cl_abap_unit_assert=>assert_equals(
      exp = ls_result-guid
      act = lv_guid
      msg = 'Outbox entry should be created'
    ).
  ENDMETHOD.

  METHOD test_feature_toggle_integration.
    " Given
    DATA: lv_feature_name TYPE string VALUE 'ZCOS_TEST_INTEGRATION',
          lv_success      TYPE abap_bool.

    " When - Setup feature toggle
    lv_success = mo_feature_toggle->setup_feature(
      iv_feature_name = lv_feature_name
      iv_is_active = abap_true
      iv_description = 'Integration Test Feature'
    ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = lv_success
      msg = 'Feature setup should succeed'
    ).

    " Verify feature is active
    DATA(lv_is_active) = mo_feature_toggle->is_feature_active( lv_feature_name ).
    cl_abap_unit_assert=>assert_true(
      act = lv_is_active
      msg = 'Feature should be active'
    ).

    " When - Deactivate feature
    lv_success = mo_feature_toggle->deactivate_feature( lv_feature_name ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = lv_success
      msg = 'Feature deactivation should succeed'
    ).

    " Verify feature is inactive
    lv_is_active = mo_feature_toggle->is_feature_active( lv_feature_name ).
    cl_abap_unit_assert=>assert_false(
      act = lv_is_active
      msg = 'Feature should be inactive'
    ).
  ENDMETHOD.

  METHOD test_monitor_integration.
    " Given
    DATA: lt_bukrs_range TYPE bukrs_range_t,
          lt_status_range TYPE char1_range_t,
          lv_from_date    TYPE dats VALUE '20240101',
          lv_to_date      TYPE dats VALUE '20241231',
          lt_data         TYPE TABLE OF zcl_cos_monitor=>ty_monitor_data,
          ls_summary      TYPE zcl_cos_monitor=>ty_monitor_summary.

    " Create test data
    DATA: ls_outbox TYPE zcos_outbox.
    ls_outbox-client = sy-mandt.
    ls_outbox-guid = 'INTEGRATION_TEST_001'.
    ls_outbox-bukrs = '1000'.
    ls_outbox-gjahr = '2024'.
    ls_outbox-belnr_src = '1234567890'.
    ls_outbox-trigger_gl = '400000'.
    ls_outbox-product_code = 'INTEGRATION001'.
    ls_outbox-total_charge = '1500.00'.
    ls_outbox-status = 'P'.
    ls_outbox-created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).

    INSERT zcos_outbox FROM ls_outbox.

    " Set up selection criteria
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '1000' ) TO lt_bukrs_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'P' ) TO lt_status_range.

    " When - Get monitor data
    lt_data = mo_monitor->get_monitor_data(
      it_bukrs_range = lt_bukrs_range
      it_status_range = lt_status_range
      iv_from_date = lv_from_date
      iv_to_date = lv_to_date
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_data
      msg = 'Monitor data should not be empty'
    ).

    " When - Get summary
    ls_summary = mo_monitor->get_monitor_summary(
      it_bukrs_range = lt_bukrs_range
      it_status_range = lt_status_range
      iv_from_date = lv_from_date
      iv_to_date = lv_to_date
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_summary-total_records
      msg = 'Summary should have records'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = '1500.00'
      act = ls_summary-total_amount
      msg = 'Total amount should match test data'
    ).
  ENDMETHOD.

  METHOD cleanup_test_data.
    " Clean up test data
    DELETE FROM zcos_outbox WHERE guid LIKE 'INTEGRATION%' OR guid LIKE 'TEST%'.
    DELETE FROM zcos_map WHERE product_code LIKE 'INTEGRATION%' OR product_code LIKE 'TEST%'.
    DELETE FROM zcos_audit WHERE guid LIKE 'INTEGRATION%' OR guid LIKE 'TEST%'.
    DELETE FROM tvarvc WHERE name LIKE 'ZCOS_TEST_%'.
    COMMIT WORK.
  ENDMETHOD.

  METHOD create_test_mapping_data.
    DATA: ls_mapping TYPE zcos_map.

    " Create test mapping entry
    ls_mapping-client = sy-mandt.
    ls_mapping-bukrs = '1000'.
    ls_mapping-trigger_gl = '400000'.
    ls_mapping-product_code = 'INTEGRATION001'.
    ls_mapping-valid_from = sy-datum.
    ls_mapping-valid_to = '99991231'.
    ls_mapping-sales_gl = '700000'.
    ls_mapping-cos_gl = '600000'.
    ls_mapping-margin_pct = '10.00'.
    ls_mapping-created_by = sy-uname.
    ls_mapping-created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_mapping-deleted = abap_false.

    INSERT zcos_map FROM ls_mapping.
    COMMIT WORK.
  ENDMETHOD.

  METHOD create_test_document_data.
    " Create test document structure
    rs_document-header-bukrs = '1000'.
    rs_document-header-gjahr = '2024'.
    rs_document-header-belnr = '1234567890'.

    " Create test line items
    DATA: ls_accit TYPE acdoca_line.
    ls_accit-hkont = '400000'. " Trigger G/L
    ls_accit-zuonr = 'INTEGRATION001'. " Product code
    ls_accit-dmbtr = '1500.00'.
    APPEND ls_accit TO rs_document-accit.

    ls_accit-hkont = '200000'. " Other G/L
    ls_accit-zuonr = 'OTHER'.
    ls_accit-dmbtr = '500.00'.
    APPEND ls_accit TO rs_document-accit.
  ENDMETHOD.

ENDCLASS.

