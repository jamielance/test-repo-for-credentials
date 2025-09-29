*&---------------------------------------------------------------------*
*& Test Class: ZCL_TC_COS_FEATURE_TOGGLE
*& Description: ABAP Unit tests for COS Feature Toggle
*&---------------------------------------------------------------------*
CLASS zcl_tc_cos_feature_toggle DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_cos_feature_toggle.

    METHODS:
      setup,
      teardown,
      
      " Test methods
      test_setup_feature_success FOR TESTING,
      test_setup_feature_invalid_name FOR TESTING,
      test_deactivate_feature_success FOR TESTING,
      test_deactivate_feature_invalid_name FOR TESTING,
      test_is_feature_active_true FOR TESTING,
      test_is_feature_active_false FOR TESTING,
      test_is_feature_active_invalid_name FOR TESTING,
      
      " Helper methods
      cleanup_test_data,
      create_test_tvarvc_entry.

ENDCLASS.

CLASS zcl_tc_cos_feature_toggle IMPLEMENTATION.

  METHOD setup.
    " Create class under test
    mo_cut = NEW zcl_cos_feature_toggle( ).
    
    " Clean up any existing test data
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD teardown.
    " Clean up test data
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD test_setup_feature_success.
    " Given
    DATA: lv_feature_name TYPE string VALUE 'ZCOS_TEST_FEATURE',
          lv_is_active    TYPE abap_bool VALUE abap_true,
          lv_description  TYPE string VALUE 'Test Feature',
          lv_success      TYPE abap_bool.

    " When
    lv_success = mo_cut->setup_feature(
      iv_feature_name = lv_feature_name
      iv_is_active = lv_is_active
      iv_description = lv_description
    ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = lv_success
      msg = 'Feature setup should succeed'
    ).

    " Verify TVARVC entry was created
    " Note: TVARVC has no standard VDM view, using direct table access
    SELECT SINGLE low FROM tvarvc
      INTO @DATA(lv_value)
      WHERE name = @lv_feature_name.
    
    cl_abap_unit_assert=>assert_equals(
      exp = 'X'
      act = lv_value
      msg = 'Feature should be active in TVARVC'
    ).
  ENDMETHOD.

  METHOD test_setup_feature_invalid_name.
    " Given
    DATA: lv_feature_name TYPE string VALUE '', " Empty name
          lv_is_active    TYPE abap_bool VALUE abap_true,
          lv_description  TYPE string VALUE 'Test Feature',
          lv_success      TYPE abap_bool.

    " When
    lv_success = mo_cut->setup_feature(
      iv_feature_name = lv_feature_name
      iv_is_active = lv_is_active
      iv_description = lv_description
    ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = lv_success
      msg = 'Feature setup should fail with empty name'
    ).
  ENDMETHOD.

  METHOD test_deactivate_feature_success.
    " Given
    DATA: lv_feature_name TYPE string VALUE 'ZCOS_TEST_FEATURE',
          lv_success      TYPE abap_bool.
    
    " Create test feature first
    _create_test_tvarvc_entry( lv_feature_name ).

    " When
    lv_success = mo_cut->deactivate_feature( lv_feature_name ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = lv_success
      msg = 'Feature deactivation should succeed'
    ).

    " Verify TVARVC entry was updated
    " Note: TVARVC has no standard VDM view, using direct table access
    SELECT SINGLE low FROM tvarvc
      INTO @DATA(lv_value)
      WHERE name = @lv_feature_name.
    
    cl_abap_unit_assert=>assert_equals(
      exp = ''
      act = lv_value
      msg = 'Feature should be inactive in TVARVC'
    ).
  ENDMETHOD.

  METHOD test_deactivate_feature_invalid_name.
    " Given
    DATA: lv_feature_name TYPE string VALUE '', " Empty name
          lv_success      TYPE abap_bool.

    " When
    lv_success = mo_cut->deactivate_feature( lv_feature_name ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = lv_success
      msg = 'Feature deactivation should fail with empty name'
    ).
  ENDMETHOD.

  METHOD test_is_feature_active_true.
    " Given
    DATA: lv_feature_name TYPE string VALUE 'ZCOS_TEST_FEATURE',
          lv_is_active    TYPE abap_bool.
    
    " Create active test feature
    _create_test_tvarvc_entry( lv_feature_name ).

    " When
    lv_is_active = mo_cut->is_feature_active( lv_feature_name ).

    " Then
    cl_abap_unit_assert=>assert_true(
      act = lv_is_active
      msg = 'Feature should be active'
    ).
  ENDMETHOD.

  METHOD test_is_feature_active_false.
    " Given
    DATA: lv_feature_name TYPE string VALUE 'ZCOS_TEST_FEATURE',
          lv_is_active    TYPE abap_bool.
    
    " Create inactive test feature
    _create_test_tvarvc_entry( lv_feature_name ).
    UPDATE tvarvc SET low = '' WHERE name = @lv_feature_name.

    " When
    lv_is_active = mo_cut->is_feature_active( lv_feature_name ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = lv_is_active
      msg = 'Feature should be inactive'
    ).
  ENDMETHOD.

  METHOD test_is_feature_active_invalid_name.
    " Given
    DATA: lv_feature_name TYPE string VALUE '', " Empty name
          lv_is_active    TYPE abap_bool.

    " When
    lv_is_active = mo_cut->is_feature_active( lv_feature_name ).

    " Then
    cl_abap_unit_assert=>assert_false(
      act = lv_is_active
      msg = 'Feature should be inactive with empty name'
    ).
  ENDMETHOD.

  METHOD cleanup_test_data.
    " Clean up test TVARVC entries
    DELETE FROM tvarvc WHERE name LIKE 'ZCOS_TEST_%'.
    COMMIT WORK.
  ENDMETHOD.

  METHOD create_test_tvarvc_entry.
    DATA: ls_tvarvc TYPE tvarvc.

    ls_tvarvc-name = iv_feature_name.
    ls_tvarvc-type = 'P'.
    ls_tvarvc-numb = '000000'.
    ls_tvarvc-low = 'X'.
    ls_tvarvc-text = 'Test Feature'.

    MODIFY tvarvc FROM ls_tvarvc.
    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.

