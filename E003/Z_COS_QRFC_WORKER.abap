*&---------------------------------------------------------------------*
*& Function Module: Z_COS_QRFC_WORKER
*& Description: qRFC Worker for Cost of Sales Auto Posting
*&---------------------------------------------------------------------*
FUNCTION z_cos_qrfc_worker.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_GUID) TYPE SYSUUID_X16
*"     VALUE(IV_BUKRS) TYPE BUKRS
*"     VALUE(IV_GJAHR) TYPE GJAHR
*"     VALUE(IV_BELNR) TYPE BELNR_D
*"----------------------------------------------------------------------

  DATA: ls_outbox     TYPE zcos_outbox,
        ls_audit      TYPE zcos_aud,
        ls_mapping    TYPE zcos_map,
        lv_cos_amount TYPE dmbtr,
        lv_total_charge TYPE dmbtr,
        lv_guid       TYPE sysuuid_x16,
        lv_belnr_cos  TYPE belnr_d,
        lv_gjahr_cos  TYPE gjahr,
        lv_message    TYPE char255.

  " Check feature toggle
  IF check_feature_active( ) = abap_false.
    RETURN.
  ENDIF.

  " Load outbox entry
  SELECT SINGLE * FROM zcos_outbox INTO ls_outbox
    WHERE guid = iv_guid.
  
  IF sy-subrc <> 0.
    " Log error
    MESSAGE e001(zcos) WITH 'Outbox entry not found' iv_guid.
    RETURN.
  ENDIF.

  " Check for duplicates in audit table
  SELECT SINGLE * FROM zcos_aud INTO ls_audit
    WHERE guid = iv_guid.
  
  IF sy-subrc = 0.
    " Already processed, skip
    RETURN.
  ENDIF.

  " Get mapping
  SELECT SINGLE * FROM zcos_map INTO ls_mapping
    WHERE bukrs = iv_bukrs
      AND trigger_gl = ls_outbox-trigger_gl
      AND product_code = ls_outbox-product_code
      AND valid_from <= sy-datum
      AND valid_to >= sy-datum
      AND deleted = abap_false.

  IF sy-subrc <> 0.
    " Log error and update outbox status
    MESSAGE e002(zcos) WITH 'Mapping not found' ls_outbox-trigger_gl ls_outbox-product_code.
    UPDATE zcos_outbox SET status = 'E', error_message = 'Mapping not found'
      WHERE guid = iv_guid.
    COMMIT WORK.
    RETURN.
  ENDIF.

  " Calculate COS amount
  lv_cos_amount = calculate_cos_amount(
    iv_outbox = ls_outbox
    iv_mapping = ls_mapping
  ).

  " Check tolerance
  IF abs( lv_cos_amount ) < '0.01'.
    " Skip posting due to tolerance
    UPDATE zcos_outbox SET status = 'S', error_message = 'Amount below tolerance'
      WHERE guid = iv_guid.
    COMMIT WORK.
    RETURN.
  ENDIF.

  " Create COS document
  lv_guid = create_cos_document(
    iv_outbox = ls_outbox
    iv_mapping = ls_mapping
    iv_cos_amount = lv_cos_amount
    ev_belnr = lv_belnr_cos
    ev_gjahr = lv_gjahr_cos
  ).

  IF lv_guid IS INITIAL.
    " Error creating document
    UPDATE zcos_outbox SET status = 'E', error_message = 'Error creating COS document'
      WHERE guid = iv_guid.
    COMMIT WORK.
    RETURN.
  ENDIF.

  " Create audit entry
  ls_audit-client = sy-mandt.
  ls_audit-guid = iv_guid.
  ls_audit-bukrs = iv_bukrs.
  ls_audit-belnr_cos = lv_belnr_cos.
  ls_audit-gjahr = lv_gjahr_cos.
  ls_audit-belnr_src = iv_belnr.
  ls_audit-posted_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
  ls_audit-posted_by = sy-uname.
  ls_audit-cos_amount = lv_cos_amount.
  ls_audit-status = 'P'. " Posted

  INSERT zcos_aud FROM ls_audit.

  " Update outbox status
  UPDATE zcos_outbox SET status = 'C', processed_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) )
    WHERE guid = iv_guid.

  COMMIT WORK.

  " Log success
  MESSAGE s003(zcos) WITH 'COS document created' lv_belnr_cos lv_gjahr_cos.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*& Form CHECK_FEATURE_ACTIVE
*&---------------------------------------------------------------------*
FORM check_feature_active
  CHANGING cv_active TYPE abap_bool.

  DATA: lv_value TYPE char1.
  
  SELECT SINGLE low FROM tvarvc INTO lv_value
    WHERE name = 'ZCOS_E003_ACTIVE'.
  
  cv_active = COND #( WHEN lv_value = 'X' THEN abap_true ELSE abap_false ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATE_COS_AMOUNT
*&---------------------------------------------------------------------*
FORM calculate_cos_amount
  USING    iv_outbox TYPE zcos_outbox
           iv_mapping TYPE zcos_map
  CHANGING cv_cos_amount TYPE dmbtr.

  DATA: lv_total_charge TYPE dmbtr,
        lv_cos_amount   TYPE dmbtr.

  " Simple calculation: use direct cost from invoice
  " For multi-line, would need to prorate based on line charges
  lv_total_charge = iv_outbox-total_charge.
  
  " Apply margin if specified
  IF iv_mapping-margin_pct IS NOT INITIAL.
    lv_cos_amount = lv_total_charge * ( 100 - iv_mapping-margin_pct ) / 100.
  ELSE.
    lv_cos_amount = lv_total_charge.
  ENDIF.

  " Round to 2 decimals
  lv_cos_amount = round( val = lv_cos_amount dec = 2 ).
  
  cv_cos_amount = lv_cos_amount.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CREATE_COS_DOCUMENT
*&---------------------------------------------------------------------*
FORM create_cos_document
  USING    iv_outbox TYPE zcos_outbox
           iv_mapping TYPE zcos_map
           iv_cos_amount TYPE dmbtr
  CHANGING cv_guid TYPE sysuuid_x16
           cv_belnr TYPE belnr_d
           cv_gjahr TYPE gjahr.

  DATA: ls_document_header TYPE bapiache09,
        lt_accountgl       TYPE TABLE OF bapiacgl09,
        ls_accountgl       TYPE bapiacgl09,
        lt_currencyamount  TYPE TABLE OF bapiaccr09,
        ls_currencyamount  TYPE bapiaccr09,
        lt_return          TYPE TABLE OF bapiret2,
        ls_return          TYPE bapiret2,
        lv_belnr           TYPE belnr_d,
        lv_gjahr           TYPE gjahr.

  " Generate GUID
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_16 = cv_guid.

  " Document header
  ls_document_header-obj_type = 'BKPFF'.
  ls_document_header-username = sy-uname.
  ls_document_header-header_txt = |COS Auto Posting for { iv_outbox-belnr_src }|.
  ls_document_header-comp_code = iv_outbox-bukrs.
  ls_document_header-doc_date = sy-datum.
  ls_document_header-pstng_date = sy-datum.
  ls_document_header-doc_type = 'AB'. " Accounting document
  ls_document_header-ref_doc_no = iv_outbox-belnr_src.

  " COS line (Debit)
  CLEAR ls_accountgl.
  ls_accountgl-itemno_acc = '1'.
  ls_accountgl-gl_account = iv_mapping-cos_gl.
  ls_accountgl-alloc_nmbr = 'COS'.
  APPEND ls_accountgl TO lt_accountgl.

  CLEAR ls_currencyamount.
  ls_currencyamount-itemno_acc = '1'.
  ls_currencyamount-currency = 'GBP'. " Assuming GBP for UK GAAP
  ls_currencyamount-amt_doccur = iv_cos_amount.
  APPEND ls_currencyamount TO lt_currencyamount.

  " Sales line (Credit)
  CLEAR ls_accountgl.
  ls_accountgl-itemno_acc = '2'.
  ls_accountgl-gl_account = iv_mapping-sales_gl.
  ls_accountgl-alloc_nmbr = 'COS'.
  APPEND ls_accountgl TO lt_accountgl.

  CLEAR ls_currencyamount.
  ls_currencyamount-itemno_acc = '2'.
  ls_currencyamount-currency = 'GBP'.
  ls_currencyamount-amt_doccur = -iv_cos_amount. " Credit
  APPEND ls_currencyamount TO lt_currencyamount.

  " Post document
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
    " Error occurred
    MESSAGE e003(zcos) WITH ls_return-message.
    CLEAR cv_guid.
    RETURN.
  ENDIF.

  " Extract document number and year
  lv_belnr = ls_document_header-obj_key+0(10).
  lv_gjahr = sy-datum(4).

  cv_belnr = lv_belnr.
  cv_gjahr = lv_gjahr.

  " Commit
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.
