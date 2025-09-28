*&---------------------------------------------------------------------*
*& Function Module: Z_COS_QRFC_WORKER
*& Description: qRFC Worker for Cost of Sales Auto Posting
*&---------------------------------------------------------------------*
*& Purpose: Processes outbox entries for Cost of Sales auto posting
*&          This function module is called asynchronously via qRFC
*&          to create COS documents based on supplier invoices
*&---------------------------------------------------------------------*
*& Parameters:
*&   IV_GUID  - Unique identifier for the outbox entry
*&   IV_BUKRS - Company code
*&   IV_GJAHR - Fiscal year
*&   IV_BELNR - Source document number
*&---------------------------------------------------------------------*
*& Author: System Generated
*& Date:   &SY-DATUM
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

  " Constants
  CONSTANTS: lc_tolerance     TYPE dmbtr VALUE '0.01',
             lc_currency      TYPE waers VALUE 'GBP',
             lc_doc_type      TYPE blart VALUE 'AB',
             TYPES: BEGIN OF ty_cos_status,
                      pending   TYPE char1 VALUE 'P',
                      error     TYPE char1 VALUE 'E',
                      skip      TYPE char1 VALUE 'S',
                      complete  TYPE char1 VALUE 'C',
                    END OF ty_cos_status.



  DATA: ls_outbox         TYPE zcos_outbox,
        ls_audit          TYPE zcos_aud,
        ls_mapping        TYPE zcos_map,
        lv_cos_amount     TYPE dmbtr,
        lv_total_charge   TYPE dmbtr,
        lv_guid           TYPE sysuuid_x16,
        lv_belnr_cos      TYPE belnr_d,
        lv_gjahr_cos      TYPE gjahr,
        lv_message        TYPE char255,
        lv_feature_active TYPE abap_bool.

  " Input parameter validation
  IF iv_guid IS INITIAL.
    MESSAGE e009(zcos) WITH 'GUID is required'.
    RETURN.
  ENDIF.
  
  IF iv_bukrs IS INITIAL.
    MESSAGE e009(zcos) WITH 'Company Code is required'.
    RETURN.
  ENDIF.
  
  IF iv_gjahr IS INITIAL.
    MESSAGE e009(zcos) WITH 'Fiscal Year is required'.
    RETURN.
  ENDIF.
  
  IF iv_belnr IS INITIAL.
    MESSAGE e009(zcos) WITH 'Document Number is required'.
    RETURN.
  ENDIF.

  " Check feature toggle
  PERFORM check_feature_active CHANGING lv_feature_active.
  IF lv_feature_active = abap_false.
    RETURN.
  ENDIF.

  " Check authorization for COS posting
  AUTHORITY-CHECK OBJECT 'ZCOS_POST'
    ID 'ACTVT' FIELD '01'
    ID 'BUKRS' FIELD iv_bukrs
    ID 'SAKNR' FIELD '0000000000'. " Check for any G/L account access
  
  IF sy-subrc <> 0.
    " Log authorization error
    DATA: lv_log_handle_auth TYPE balloghndl.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = VALUE bal_s_log( object = 'ZCOS' subobject = 'QRFC' )
      IMPORTING
        e_log_handle = lv_log_handle_auth.
    
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle_auth
        i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '008' 
                                   msgv1 = 'Authorization check failed for COS posting' ).
    
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_log_handle = lv_log_handle_auth.
    
    MESSAGE e008(zcos) WITH 'Authorization check failed for COS posting'.
    RETURN.
  ENDIF.

  " Load outbox entry
  SELECT SINGLE client, guid, bukrs, gjahr, belnr_src, trigger_gl, product_code, 
                total_charge, status, error_message, created_at, processed_at
    FROM zcos_outbox INTO @ls_outbox
    WHERE guid = @iv_guid.
  
  IF sy-subrc <> 0.
    " Log error to application log
    DATA: lv_log_handle TYPE balloghndl.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = VALUE bal_s_log( object = 'ZCOS' subobject = 'QRFC' )
      IMPORTING
        e_log_handle = lv_log_handle.
    
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle
        i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '001' 
                                   msgv1 = 'Outbox entry not found' msgv2 = iv_guid ).
    
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_log_handle = lv_log_handle.
    
    MESSAGE e001(zcos) WITH 'Outbox entry not found' iv_guid.
    RETURN.
  ENDIF.

  " Check for duplicates in audit table
  SELECT SINGLE client, guid, bukrs, gjahr, belnr_cos, belnr_src, cos_amount, 
                status, posted_at, posted_by, reversal_doc, reversal_gjahr
    FROM zcos_aud INTO @ls_audit
    WHERE guid = @iv_guid.
  
  IF sy-subrc = 0.
    " Already processed, skip
    RETURN.
  ENDIF.

  " Get mapping
  SELECT SINGLE client, bukrs, trigger_gl, product_code, valid_from, valid_to,
                sales_gl, cos_gl, margin_pct, created_by, created_at, deleted
    FROM zcos_map INTO @ls_mapping
    WHERE bukrs = @iv_bukrs
      AND trigger_gl = @ls_outbox-trigger_gl
      AND product_code = @ls_outbox-product_code
      AND valid_from <= @sy-datum
      AND valid_to >= @sy-datum
      AND deleted = @abap_false.

  IF sy-subrc <> 0.
    " Log error to application log
    DATA: lv_log_handle2 TYPE balloghndl.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = VALUE bal_s_log( object = 'ZCOS' subobject = 'QRFC' )
      IMPORTING
        e_log_handle = lv_log_handle2.
    
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle2
        i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '002' 
                                   msgv1 = 'Mapping not found' 
                                   msgv2 = ls_outbox-trigger_gl 
                                   msgv3 = ls_outbox-product_code ).
    
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_log_handle = lv_log_handle2.
    
    " Update outbox status
    UPDATE zcos_outbox SET status = 'E', error_message = 'Mapping not found'
      WHERE guid = @iv_guid.
    
    " Check update result
    IF sy-subrc <> 0.
      " Log database error
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_log_handle2
          i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '008' 
                                     msgv1 = 'Database update failed for outbox status' ).
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_log_handle = lv_log_handle2.
    ELSE.
      COMMIT WORK.
    ENDIF.
    
    MESSAGE e002(zcos) WITH 'Mapping not found' ls_outbox-trigger_gl ls_outbox-product_code.
    RETURN.
  ENDIF.

  " Calculate COS amount
  PERFORM calculate_cos_amount
    USING ls_outbox ls_mapping
    CHANGING lv_cos_amount.

  " Check tolerance
  IF abs( lv_cos_amount ) < lc_tolerance.
    " Skip posting due to tolerance
    UPDATE zcos_outbox SET status = @lc_status_skip, error_message = 'Amount below tolerance'
      WHERE guid = @iv_guid.
    
    " Check update result
    IF sy-subrc <> 0.
      " Log database error
      DATA: lv_log_handle_tol TYPE balloghndl.
      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log = VALUE bal_s_log( object = 'ZCOS' subobject = 'QRFC' )
        IMPORTING
          e_log_handle = lv_log_handle_tol.
      
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_log_handle_tol
          i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '008' 
                                     msgv1 = 'Database update failed for tolerance check' ).
      
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_log_handle = lv_log_handle_tol.
    ELSE.
      COMMIT WORK.
    ENDIF.
    RETURN.
  ENDIF.

  " Create COS document
  PERFORM create_cos_document
    USING ls_outbox ls_mapping lv_cos_amount
    CHANGING lv_guid lv_belnr_cos lv_gjahr_cos.

  IF lv_guid IS INITIAL.
    " Error creating document
    UPDATE zcos_outbox SET status = @lc_status_error, error_message = 'Error creating COS document'
      WHERE guid = @iv_guid.
    
    " Check update result
    IF sy-subrc <> 0.
      " Log database error
      DATA: lv_log_handle_doc TYPE balloghndl.
      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log = VALUE bal_s_log( object = 'ZCOS' subobject = 'QRFC' )
        IMPORTING
          e_log_handle = lv_log_handle_doc.
      
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_log_handle_doc
          i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '008' 
                                     msgv1 = 'Database update failed for document creation error' ).
      
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
        i_log_handle = lv_log_handle_doc.
    ELSE.
      COMMIT WORK.
    ENDIF.
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
  ls_audit-status = lc_status_pending. " Posted

  INSERT zcos_aud FROM ls_audit.
  
  " Check insert result
  IF sy-subrc <> 0.
    " Log database error
    DATA: lv_log_handle_audit TYPE balloghndl.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = VALUE bal_s_log( object = 'ZCOS' subobject = 'QRFC' )
      IMPORTING
        e_log_handle = lv_log_handle_audit.
    
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle_audit
        i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '008' 
                                   msgv1 = 'Database insert failed for audit entry' ).
    
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_log_handle = lv_log_handle_audit.
    
    MESSAGE e008(zcos) WITH 'Database insert failed for audit entry'.
    RETURN.
  ENDIF.

  " Update outbox status
  UPDATE zcos_outbox SET status = @lc_status_complete, processed_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) )
    WHERE guid = @iv_guid.

  " Check update result
  IF sy-subrc <> 0.
    " Log database error
    DATA: lv_log_handle_outbox TYPE balloghndl.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = VALUE bal_s_log( object = 'ZCOS' subobject = 'QRFC' )
      IMPORTING
        e_log_handle = lv_log_handle_outbox.
    
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle_outbox
        i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '008' 
                                   msgv1 = 'Database update failed for outbox status' ).
    
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_log_handle = lv_log_handle_outbox.
    
    MESSAGE e008(zcos) WITH 'Database update failed for outbox status'.
    RETURN.
  ENDIF.

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
  ls_document_header-doc_type = lc_doc_type. " Accounting document
  ls_document_header-ref_doc_no = iv_outbox-belnr_src.

  " COS line (Debit)
  CLEAR ls_accountgl.
  ls_accountgl-itemno_acc = '1'.
  ls_accountgl-gl_account = iv_mapping-cos_gl.
  ls_accountgl-alloc_nmbr = 'COS'.
  APPEND ls_accountgl TO lt_accountgl.

  CLEAR ls_currencyamount.
  ls_currencyamount-itemno_acc = '1'.
  ls_currencyamount-currency = lc_currency. " Assuming GBP for UK GAAP
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
  ls_currencyamount-currency = lc_currency.
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
    " Log error to application log
    DATA: lv_log_handle3 TYPE balloghndl.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = VALUE bal_s_log( object = 'ZCOS' subobject = 'QRFC' )
      IMPORTING
        e_log_handle = lv_log_handle3.
    
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle3
        i_s_msg = VALUE bal_s_msg( msgty = 'E' msgid = 'ZCOS' msgno = '004' 
                                   msgv1 = 'Error creating COS document' 
                                   msgv2 = ls_return-message ).
    
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_log_handle = lv_log_handle3.
    
    MESSAGE e004(zcos) WITH 'Error creating COS document' ls_return-message.
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
