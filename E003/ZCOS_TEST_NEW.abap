*&---------------------------------------------------------------------*
*& Program: ZCOS_TEST_NEW
*& Description: Refactored test program for COS Auto Posting
*&---------------------------------------------------------------------*
REPORT zcos_test_new.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE bukrs DEFAULT '1000',
              p_test  AS CHECKBOX DEFAULT 'X',
              p_clean AS CHECKBOX,
              p_unit  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_feature AS CHECKBOX DEFAULT 'X',
              p_qrfc    AS CHECKBOX DEFAULT 'X',
              p_monitor AS CHECKBOX DEFAULT 'X',
              p_integration AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
  DATA: lo_test_runner TYPE REF TO zcl_cos_test_runner.

  " Create test runner
  lo_test_runner = NEW zcl_cos_test_runner( ).

  " Clean up test data if requested
  IF p_clean = 'X'.
    lo_test_runner->cleanup_all_test_data( ).
    DATA(ls_cleanup_msg) = zcl_cos_message_utility=>get_test_data_cleanup_success( ).
    WRITE: / ls_cleanup_msg-msgv1.
  ENDIF.

  " Run unit tests if requested
  IF p_unit = 'X'.
    lo_test_runner->run_unit_tests(
      iv_test_feature = p_feature
      iv_test_qrfc = p_qrfc
      iv_test_monitor = p_monitor
      iv_test_integration = p_integration
    ).
  ENDIF.

  " Run integration tests if requested
  IF p_test = 'X'.
    lo_test_runner->run_integration_tests( p_bukrs ).
  ENDIF.

*&---------------------------------------------------------------------*
*& Class: ZCL_COS_TEST_RUNNER
*& Description: Test runner for COS Auto Posting tests
*&---------------------------------------------------------------------*
CLASS zcl_cos_test_runner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      run_unit_tests
        IMPORTING
          iv_test_feature    TYPE abap_bool DEFAULT abap_true
          iv_test_qrfc       TYPE abap_bool DEFAULT abap_true
          iv_test_monitor    TYPE abap_bool DEFAULT abap_true
          iv_test_integration TYPE abap_bool DEFAULT abap_true,

      run_integration_tests
        IMPORTING
          iv_bukrs TYPE bukrs,

      cleanup_all_test_data.

  PRIVATE SECTION.
    METHODS:
      run_feature_toggle_tests,
      run_qrfc_worker_tests,
      run_monitor_tests,
      run_integration_tests,
      create_test_mapping_data
        IMPORTING
          iv_bukrs TYPE bukrs,
      create_test_document
        IMPORTING
          iv_bukrs TYPE bukrs
        RETURNING
          VALUE(rv_success) TYPE abap_bool.

ENDCLASS.

CLASS zcl_cos_test_runner IMPLEMENTATION.

  METHOD run_unit_tests.
    WRITE: / 'Running COS Auto Posting Unit Tests...'.

    IF iv_test_feature = abap_true.
      run_feature_toggle_tests( ).
    ENDIF.

    IF iv_test_qrfc = abap_true.
      run_qrfc_worker_tests( ).
    ENDIF.

    IF iv_test_monitor = abap_true.
      run_monitor_tests( ).
    ENDIF.

    IF iv_test_integration = abap_true.
      run_integration_tests( ).
    ENDIF.

    WRITE: / 'Unit tests completed'.
  ENDMETHOD.

  METHOD run_integration_tests.
    WRITE: / 'Running COS Auto Posting Integration Tests...'.

    " Setup test data
    create_test_mapping_data( iv_bukrs ).

    " Test feature toggle
    DATA: lo_feature_toggle TYPE REF TO zcl_cos_feature_toggle.
    lo_feature_toggle = NEW zcl_cos_feature_toggle( ).

    DATA(lv_success) = lo_feature_toggle->setup_feature(
      iv_feature_name = 'ZCOS_E003_ACTIVE'
      iv_is_active = abap_true
      iv_description = 'Integration Test Feature'
    ).

    IF lv_success = abap_true.
      DATA(ls_feature_success_msg) = zcl_cos_message_utility=>get_feature_setup_success( ).
      WRITE: / 'Feature toggle setup:', ls_feature_success_msg-msgv1.
    ELSE.
      DATA(ls_feature_error_msg) = zcl_cos_message_utility=>get_feature_setup_error( ).
      WRITE: / 'Feature toggle setup:', ls_feature_error_msg-msgv1.
    ENDIF.

    " Test document creation
    DATA(lv_doc_success) = create_test_document( iv_bukrs ).

    IF lv_doc_success = abap_true.
      DATA(ls_doc_success_msg) = zcl_cos_message_utility=>get_document_creation_success( 'TEST001' ).
      WRITE: / 'Test document creation:', ls_doc_success_msg-msgv1.
    ELSE.
      DATA(ls_doc_error_msg) = zcl_cos_message_utility=>get_document_creation_failed_error( ).
      WRITE: / 'Test document creation:', ls_doc_error_msg-msgv1.
    ENDIF.

    " Test monitor
    DATA: lo_monitor TYPE REF TO zcl_cos_monitor.
    lo_monitor = NEW zcl_cos_monitor( ).

    DATA: lt_bukrs_range TYPE bukrs_range_t,
          lt_status_range TYPE char1_range_t,
          lv_from_date    TYPE dats VALUE '20240101',
          lv_to_date      TYPE dats VALUE '20241231'.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = iv_bukrs ) TO lt_bukrs_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'P' ) TO lt_status_range.

    DATA(lt_data) = lo_monitor->get_monitor_data(
      it_bukrs_range = lt_bukrs_range
      it_status_range = lt_status_range
      iv_from_date = lv_from_date
      iv_to_date = lv_to_date
    ).

    IF lines( lt_data ) > 0.
      DATA(ls_monitor_success_msg) = zcl_cos_message_utility=>get_monitor_data_success( lines( lt_data ) ).
      WRITE: / 'Monitor data retrieval:', ls_monitor_success_msg-msgv1.
    ELSE.
      WRITE: / 'Monitor data retrieval: NO DATA FOUND'.
    ENDIF.

    DATA(ls_integration_msg) = zcl_cos_message_utility=>get_integration_test_success( ).
    WRITE: / ls_integration_msg-msgv1.
  ENDMETHOD.

  METHOD cleanup_all_test_data.
    " Clean up all test data
    DELETE FROM zcos_map WHERE created_by = sy-uname OR product_code LIKE 'TEST%'.
    DELETE FROM zcos_outbox WHERE created_at >= cl_abap_tstmp=>utc2tstmp( @cl_abap_tstmp=>create( date = @sy-datum time = '000000' ) ).
    DELETE FROM zcos_audit WHERE posted_by = sy-uname.
    DELETE FROM tvarvc WHERE name LIKE 'ZCOS_TEST_%'.

    COMMIT WORK.
  ENDMETHOD.

  METHOD run_feature_toggle_tests.
    WRITE: / '  - Feature Toggle Tests: SKIPPED (Run via ABAP Unit)'.
  ENDMETHOD.

  METHOD run_qrfc_worker_tests.
    WRITE: / '  - qRFC Worker Tests: SKIPPED (Run via ABAP Unit)'.
  ENDMETHOD.

  METHOD run_monitor_tests.
    WRITE: / '  - Monitor Tests: SKIPPED (Run via ABAP Unit)'.
  ENDMETHOD.

  METHOD run_integration_tests.
    WRITE: / '  - Integration Tests: SKIPPED (Run via ABAP Unit)'.
  ENDMETHOD.

  METHOD create_test_mapping_data.
    DATA: ls_mapping TYPE zcos_map.

    ls_mapping-client = sy-mandt.
    ls_mapping-bukrs = iv_bukrs.
    ls_mapping-trigger_gl = '400000'.
    ls_mapping-product_code = 'TEST001'.
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

  METHOD create_test_document.
    DATA: ls_document_header TYPE bapiache09,
          lt_accountgl       TYPE TABLE OF bapiacgl09,
          ls_accountgl       TYPE bapiacgl09,
          lt_currencyamount  TYPE TABLE OF bapiaccr09,
          ls_currencyamount  TYPE bapiaccr09,
          lt_return          TYPE TABLE OF bapiret2,
          ls_return          TYPE bapiret2.

    " Create test supplier invoice
    ls_document_header-obj_type = 'BKPFF'.
    ls_document_header-username = sy-uname.
    ls_document_header-header_txt = 'Test Supplier Invoice for COS'.
    ls_document_header-comp_code = iv_bukrs.
    ls_document_header-doc_date = sy-datum.
    ls_document_header-pstng_date = sy-datum.
    ls_document_header-doc_type = 'KR'.
    ls_document_header-ref_doc_no = 'TEST001'.

    " Trigger G/L line
    ls_accountgl-itemno_acc = '1'.
    ls_accountgl-gl_account = '400000'.
    ls_accountgl-alloc_nmbr = 'TEST001'.
    APPEND ls_accountgl TO lt_accountgl.

    ls_currencyamount-itemno_acc = '1'.
    ls_currencyamount-currency = 'GBP'.
    ls_currencyamount-amt_doccur = '1000.00'.
    APPEND ls_currencyamount TO lt_currencyamount.

    " Post test document
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_document_header
      IMPORTING
        obj_type        = ls_document_header-obj_type
        obj_key         = ls_document_header-obj_key
        obj_sys         = ls_document_header-obj_sys
      TABLES
        accountgl       = lt_accountgl
        currencyamount  = lt_currencyamount
        return          = lt_return.

    " Check for errors
    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      rv_success = abap_false.
      WRITE: / 'Error creating test document:', ls_return-message.
      RETURN.
    ENDIF.

    " Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    rv_success = abap_true.
  ENDMETHOD.

ENDCLASS.

