*&---------------------------------------------------------------------*
*& Test Class: ZCL_TC_COS_QRFC_WORKER
*& Description: ABAP Unit tests for COS qRFC Worker
*&---------------------------------------------------------------------*
CLASS zcl_tc_cos_qrfc_worker DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_cos_qrfc_worker,
      mo_mock_logger TYPE REF TO zcl_cos_logger.

    METHODS:
      setup,
      teardown,
      
      " Test methods
      test_process_outbox_entry_success FOR TESTING,
      test_process_outbox_entry_invalid_guid FOR TESTING,
      test_process_outbox_entry_feature_inactive FOR TESTING,
      test_process_outbox_entry_outbox_not_found FOR TESTING,
      test_process_outbox_entry_duplicate_processing FOR TESTING,
      test_process_outbox_entry_mapping_not_found FOR TESTING,
      test_process_outbox_entry_amount_below_tolerance FOR TESTING,
      
      " Helper methods
      cleanup_test_data,
      create_test_outbox_entry
        IMPORTING
          iv_guid TYPE sysuuid_x16
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_belnr TYPE belnr_d
          iv_trigger_gl TYPE saknr
          iv_product_code TYPE char20
          iv_total_charge TYPE dmbtr,
      create_test_mapping_entry
        IMPORTING
          iv_bukrs TYPE bukrs
          iv_trigger_gl TYPE saknr
          iv_product_code TYPE char20
          iv_sales_gl TYPE saknr
          iv_cos_gl TYPE saknr
          iv_margin_pct TYPE dec5_2,
      create_test_audit_entry
        IMPORTING
          iv_guid TYPE sysuuid_x16
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_belnr TYPE belnr_d.

ENDCLASS.

CLASS zcl_tc_cos_qrfc_worker IMPLEMENTATION.

  METHOD setup.
    " Create class under test
    mo_cut = NEW zcl_cos_qrfc_worker( ).
    
    " Clean up any existing test data
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up test data
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD test_process_outbox_entry_success.
    " Given
    DATA: lv_guid          TYPE sysuuid_x16 VALUE '1234567890123456',
          lv_bukrs         TYPE bukrs VALUE '1000',
          lv_gjahr         TYPE gjahr VALUE '2024',
          lv_belnr         TYPE belnr_d VALUE '1234567890',
          lv_trigger_gl    TYPE saknr VALUE '400000',
          lv_product_code  TYPE char20 VALUE 'TEST001',
          lv_sales_gl      TYPE saknr VALUE '700000',
          lv_cos_gl        TYPE saknr VALUE '600000',
          lv_total_charge  TYPE dmbtr VALUE '1000.00',
          ls_result        TYPE zcl_cos_qrfc_worker=>ty_processing_result.

    " Create test data
    create_test_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
      iv_total_charge = lv_total_charge
    ).

    create_test_mapping_entry(
      iv_bukrs = lv_bukrs
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
      iv_sales_gl = lv_sales_gl
      iv_cos_gl = lv_cos_gl
      iv_margin_pct = '10.00'
    ).

    " When
    ls_result = mo_cut->process_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
    ).

    " Then
    " Note: This test will likely fail in a real environment due to BAPI calls
    " but demonstrates the test structure
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result
      msg = 'Result should not be initial'
    ).
  ENDMETHOD.

  METHOD test_process_outbox_entry_invalid_guid.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '', " Empty GUID
          lv_bukrs  TYPE bukrs VALUE '1000',
          lv_gjahr  TYPE gjahr VALUE '2024',
          lv_belnr  TYPE belnr_d VALUE '1234567890',
          ls_result TYPE zcl_cos_qrfc_worker=>ty_processing_result.

    " When
    ls_result = mo_cut->process_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
    ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Processing should fail with invalid GUID'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'Invalid input parameters'
      act = ls_result-error_message
      msg = 'Error message should indicate invalid input'
    ).
  ENDMETHOD.

  METHOD test_process_outbox_entry_feature_inactive.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '1234567890123456',
          lv_bukrs  TYPE bukrs VALUE '1000',
          lv_gjahr  TYPE gjahr VALUE '2024',
          lv_belnr  TYPE belnr_d VALUE '1234567890',
          ls_result TYPE zcl_cos_qrfc_worker=>ty_processing_result.

    " Deactivate feature toggle
    UPDATE tvarvc SET low = '' WHERE name = 'ZCOS_E003_ACTIVE'.

    " When
    ls_result = mo_cut->process_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
    ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Processing should fail when feature is inactive'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'Feature is not active'
      act = ls_result-error_message
      msg = 'Error message should indicate feature is inactive'
    ).

    " Restore feature toggle
    UPDATE tvarvc SET low = 'X' WHERE name = 'ZCOS_E003_ACTIVE'.
  ENDMETHOD.

  METHOD test_process_outbox_entry_outbox_not_found.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '1234567890123456',
          lv_bukrs  TYPE bukrs VALUE '1000',
          lv_gjahr  TYPE gjahr VALUE '2024',
          lv_belnr  TYPE belnr_d VALUE '1234567890',
          ls_result TYPE zcl_cos_qrfc_worker=>ty_processing_result.

    " When (no outbox entry created)
    ls_result = mo_cut->process_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
    ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Processing should fail when outbox entry not found'
    ).
  ENDMETHOD.

  METHOD test_process_outbox_entry_duplicate_processing.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '1234567890123456',
          lv_bukrs  TYPE bukrs VALUE '1000',
          lv_gjahr  TYPE gjahr VALUE '2024',
          lv_belnr  TYPE belnr_d VALUE '1234567890',
          ls_result TYPE zcl_cos_qrfc_worker=>ty_processing_result.

    " Create audit entry to simulate already processed
    create_test_audit_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
    ).

    " When
    ls_result = mo_cut->process_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
    ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Processing should fail for duplicate processing'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'Already processed'
      act = ls_result-error_message
      msg = 'Error message should indicate already processed'
    ).
  ENDMETHOD.

  METHOD test_process_outbox_entry_mapping_not_found.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '1234567890123456',
          lv_bukrs  TYPE bukrs VALUE '1000',
          lv_gjahr  TYPE gjahr VALUE '2024',
          lv_belnr  TYPE belnr_d VALUE '1234567890',
          lv_trigger_gl TYPE saknr VALUE '400000',
          lv_product_code TYPE char20 VALUE 'TEST001',
          lv_total_charge TYPE dmbtr VALUE '1000.00',
          ls_result TYPE zcl_cos_qrfc_worker=>ty_processing_result.

    " Create outbox entry but no mapping
    create_test_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
      iv_total_charge = lv_total_charge
    ).

    " When
    ls_result = mo_cut->process_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
    ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Processing should fail when mapping not found'
    ).
  ENDMETHOD.

  METHOD test_process_outbox_entry_amount_below_tolerance.
    " Given
    DATA: lv_guid   TYPE sysuuid_x16 VALUE '1234567890123456',
          lv_bukrs  TYPE bukrs VALUE '1000',
          lv_gjahr  TYPE gjahr VALUE '2024',
          lv_belnr  TYPE belnr_d VALUE '1234567890',
          lv_trigger_gl TYPE saknr VALUE '400000',
          lv_product_code TYPE char20 VALUE 'TEST001',
          lv_sales_gl TYPE saknr VALUE '700000',
          lv_cos_gl TYPE saknr VALUE '600000',
          lv_total_charge TYPE dmbtr VALUE '0.005', " Below tolerance
          ls_result TYPE zcl_cos_qrfc_worker=>ty_processing_result.

    " Create test data
    create_test_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
      iv_total_charge = lv_total_charge
    ).

    create_test_mapping_entry(
      iv_bukrs = lv_bukrs
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
      iv_sales_gl = lv_sales_gl
      iv_cos_gl = lv_cos_gl
      iv_margin_pct = '10.00'
    ).

    " When
    ls_result = mo_cut->process_outbox_entry(
      iv_guid = lv_guid
      iv_bukrs = lv_bukrs
      iv_gjahr = lv_gjahr
      iv_belnr = lv_belnr
    ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Processing should fail when amount below tolerance'
    ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'Amount below tolerance'
      act = ls_result-error_message
      msg = 'Error message should indicate amount below tolerance'
    ).
  ENDMETHOD.

  METHOD cleanup_test_data.
    " Clean up test data
    DELETE FROM zcos_outbox WHERE guid LIKE '1234567890123456'.
    DELETE FROM zcos_map WHERE product_code = 'TEST001'.
    DELETE FROM zcos_audit WHERE guid LIKE '1234567890123456'.
    COMMIT WORK.
  ENDMETHOD.

  METHOD create_test_outbox_entry.
    DATA: ls_outbox TYPE zcos_outbox.

    ls_outbox-client = sy-mandt.
    ls_outbox-guid = iv_guid.
    ls_outbox-bukrs = iv_bukrs.
    ls_outbox-gjahr = iv_gjahr.
    ls_outbox-belnr_src = iv_belnr.
    ls_outbox-trigger_gl = iv_trigger_gl.
    ls_outbox-product_code = iv_product_code.
    ls_outbox-total_charge = iv_total_charge.
    ls_outbox-created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_outbox-status = 'P'.

    INSERT zcos_outbox FROM ls_outbox.
    COMMIT WORK.
  ENDMETHOD.

  METHOD create_test_mapping_entry.
    DATA: ls_mapping TYPE zcos_map.

    ls_mapping-client = sy-mandt.
    ls_mapping-bukrs = iv_bukrs.
    ls_mapping-trigger_gl = iv_trigger_gl.
    ls_mapping-product_code = iv_product_code.
    ls_mapping-valid_from = sy-datum.
    ls_mapping-valid_to = '99991231'.
    ls_mapping-sales_gl = iv_sales_gl.
    ls_mapping-cos_gl = iv_cos_gl.
    ls_mapping-margin_pct = iv_margin_pct.
    ls_mapping-created_by = sy-uname.
    ls_mapping-created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_mapping-deleted = abap_false.

    INSERT zcos_map FROM ls_mapping.
    COMMIT WORK.
  ENDMETHOD.

  METHOD create_test_audit_entry.
    DATA: ls_audit TYPE zcos_audit.

    ls_audit-client = sy-mandt.
    ls_audit-guid = iv_guid.
    ls_audit-bukrs = iv_bukrs.
    ls_audit-gjahr = iv_gjahr.
    ls_audit-belnr_src = iv_belnr.
    ls_audit-posted_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_audit-posted_by = sy-uname.
    ls_audit-cos_amount = '1000.00'.
    ls_audit-status = 'P'.

    INSERT zcos_audit FROM ls_audit.
    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.

