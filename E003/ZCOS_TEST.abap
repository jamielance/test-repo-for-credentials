*&---------------------------------------------------------------------*
*& Program: ZCOS_TEST
*& Description: Test program for COS Auto Posting
*&---------------------------------------------------------------------*
REPORT zcos_test.

TYPES: BEGIN OF ty_test_data,
         bukrs        TYPE bukrs,
         trigger_gl   TYPE saknr,
         product_code TYPE char20,
         sales_gl     TYPE saknr,
         cos_gl       TYPE saknr,
         amount       TYPE dmbtr,
       END OF ty_test_data.

DATA: gt_test_data TYPE TABLE OF ty_test_data,
      gs_test_data TYPE ty_test_data.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE bukrs DEFAULT '1000',
              p_test  AS CHECKBOX DEFAULT 'X',
              p_clean AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  " Setup test data
  gs_test_data-bukrs = '1000'.
  gs_test_data-trigger_gl = '400000'.
  gs_test_data-product_code = 'TEST001'.
  gs_test_data-sales_gl = '700000'.
  gs_test_data-cos_gl = '600000'.
  gs_test_data-amount = '1000.00'.
  APPEND gs_test_data TO gt_test_data.

START-OF-SELECTION.
  IF p_clean = 'X'.
    PERFORM cleanup_test_data.
  ENDIF.

  IF p_test = 'X'.
    PERFORM setup_test_data.
    PERFORM test_cos_posting.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form SETUP_TEST_DATA
*&---------------------------------------------------------------------*
FORM setup_test_data.
  DATA: ls_mapping TYPE zcos_map.

  " Create test mapping entries
  LOOP AT gt_test_data INTO gs_test_data.
    ls_mapping-client = sy-mandt.
    ls_mapping-bukrs = gs_test_data-bukrs.
    ls_mapping-trigger_gl = gs_test_data-trigger_gl.
    ls_mapping-product_code = gs_test_data-product_code.
    ls_mapping-valid_from = sy-datum.
    ls_mapping-valid_to = '99991231'.
    ls_mapping-sales_gl = gs_test_data-sales_gl.
    ls_mapping-cos_gl = gs_test_data-cos_gl.
    ls_mapping-margin_pct = '10.00'.
    ls_mapping-created_by = sy-uname.
    ls_mapping-created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_mapping-deleted = abap_false.

    INSERT zcos_map FROM ls_mapping.
  ENDLOOP.

  COMMIT WORK.

  WRITE: / 'Test data setup completed'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TEST_COS_POSTING
*&---------------------------------------------------------------------*
FORM test_cos_posting.
  DATA: ls_document_header TYPE bapiache09,
        lt_accountgl       TYPE TABLE OF bapiacgl09,
        ls_accountgl       TYPE bapiacgl09,
        lt_currencyamount  TYPE TABLE OF bapiaccr09,
        ls_currencyamount  TYPE bapiaccr09,
        lt_return          TYPE TABLE OF bapiret2,
        ls_return          TYPE bapiret2,
        lv_belnr           TYPE belnr_d,
        lv_gjahr           TYPE gjahr.

  " Create test supplier invoice
  ls_document_header-obj_type = 'BKPFF'.
  ls_document_header-username = sy-uname.
  ls_document_header-header_txt = 'Test Supplier Invoice for COS'.
  ls_document_header-comp_code = p_bukrs.
  ls_document_header-doc_date = sy-datum.
  ls_document_header-pstng_date = sy-datum.
  ls_document_header-doc_type = 'KR'. " Vendor invoice
  ls_document_header-ref_doc_no = 'TEST001'.

  " Trigger G/L line
  CLEAR ls_accountgl.
  ls_accountgl-itemno_acc = '1'.
  ls_accountgl-gl_account = '400000'. " Test trigger G/L
  ls_accountgl-alloc_nmbr = 'TEST001'.
  APPEND ls_accountgl TO lt_accountgl.

  CLEAR ls_currencyamount.
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
    WRITE: / 'Error creating test document:', ls_return-message.
    RETURN.
  ENDIF.

  " Extract document number
  lv_belnr = ls_document_header-obj_key+0(10).
  lv_gjahr = sy-datum(4).

  WRITE: / 'Test document created:', lv_belnr, lv_gjahr.

  " Commit
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  " Wait for qRFC processing
  WAIT UP TO 5 SECONDS.

  " Check if COS document was created
  SELECT SINGLE * FROM zcos_aud INTO @DATA(ls_audit)
    WHERE belnr_src = @lv_belnr
      AND gjahr = @lv_gjahr.

  IF sy-subrc = 0.
    WRITE: / 'COS document created:', ls_audit-belnr_cos, ls_audit-gjahr.
    WRITE: / 'COS amount:', ls_audit-cos_amount CURRENCY 'GBP'.
  ELSE.
    WRITE: / 'No COS document found - check logs'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CLEANUP_TEST_DATA
*&---------------------------------------------------------------------*
FORM cleanup_test_data.
  " Clean up test data
  DELETE FROM zcos_map WHERE created_by = sy-uname.
  DELETE FROM zcos_outbox WHERE created_at >= cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>create( date = sy-datum time = '000000' ) ).
  DELETE FROM zcos_aud WHERE posted_by = sy-uname.

  COMMIT WORK.

  WRITE: / 'Test data cleanup completed'.

ENDFORM.
