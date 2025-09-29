*&---------------------------------------------------------------------*
*& Class: ZCL_COS_QRFC_WORKER
*& Description: qRFC Worker for Cost of Sales Auto Posting
*&---------------------------------------------------------------------*
CLASS zcl_cos_qrfc_worker DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">COS processing status constants</p>
    "! <p>Defines the possible status values for COS processing operations.
    "! Used throughout the system to track the state of outbox entries.</p>
    TYPES: BEGIN OF ty_cos_status,
             "! <p class="shorttext synchronized">Pending status</p>
             "! <p>Entry is waiting to be processed</p>
             pending   TYPE char1 VALUE 'P',
             "! <p class="shorttext synchronized">Error status</p>
             "! <p>Processing failed with an error</p>
             error     TYPE char1 VALUE 'E',
             "! <p class="shorttext synchronized">Skip status</p>
             "! <p>Entry was skipped due to business rules</p>
             skip      TYPE char1 VALUE 'S',
             "! <p class="shorttext synchronized">Complete status</p>
             "! <p>Processing completed successfully</p>
             complete  TYPE char1 VALUE 'C',
           END OF ty_cos_status.

    "! <p class="shorttext synchronized">Processing result structure</p>
    "! <p>Contains the result of COS processing operations including
    "! success status, document information, and error details.</p>
    TYPES: BEGIN OF ty_processing_result,
             "! <p class="shorttext synchronized">Processing success flag</p>
             "! <p>True if processing completed successfully</p>
             success       TYPE abap_bool,
             "! <p class="shorttext synchronized">COS document number</p>
             "! <p>Generated COS document number</p>
             cos_document  TYPE belnr_d,
             "! <p class="shorttext synchronized">COS document year</p>
             "! <p>Fiscal year of the COS document</p>
             cos_year      TYPE gjahr,
             "! <p class="shorttext synchronized">COS amount</p>
             "! <p>Amount posted for COS</p>
             cos_amount    TYPE dmbtr,
             "! <p class="shorttext synchronized">Error message</p>
             "! <p>Error message if processing failed</p>
             error_message TYPE string,
           END OF ty_processing_result.

    "! <p class="shorttext synchronized">Constructor</p>
    "! <p>Creates a new instance of the qRFC worker with optional dependencies.
    "! Uses dependency injection for logger and validator for better testability.</p>
    "! @parameter io_logger | <p class="shorttext synchronized">Logger instance (optional)</p>
    "! @parameter io_validator | <p class="shorttext synchronized">Validator instance (optional)</p>
    METHODS:
      constructor
        IMPORTING
          io_logger TYPE REF TO zif_cos_logger OPTIONAL
          io_validator TYPE REF TO zif_cos_validator OPTIONAL,

      "! <p class="shorttext synchronized">Process outbox entry</p>
      "! <p>Main processing method that handles a single outbox entry.
      "! Performs validation, feature checks, authorization, and COS document creation.
      "! This is the core method called by the qRFC framework.</p>
      "! @parameter iv_guid | <p class="shorttext synchronized">Outbox entry GUID</p>
      "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code</p>
      "! @parameter iv_gjahr | <p class="shorttext synchronized">Fiscal year</p>
      "! @parameter iv_belnr | <p class="shorttext synchronized">Source document number</p>
      "! @parameter rv_result | <p class="shorttext synchronized">Processing result</p>
      process_outbox_entry
        IMPORTING
          iv_guid  TYPE sysuuid_x16
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_belnr TYPE belnr_d
        RETURNING
          VALUE(rv_result) TYPE ty_processing_result.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">COS mapping structure</p>
    "! <p>Contains the mapping configuration for COS processing including
    "! G/L accounts, product codes, and margin percentages.</p>
    TYPES: BEGIN OF ty_cos_mapping,
             "! <p class="shorttext synchronized">Company code</p>
             "! <p>Company code for the mapping</p>
             bukrs        TYPE bukrs,
             "! <p class="shorttext synchronized">Trigger G/L account</p>
             "! <p>G/L account that triggers COS processing</p>
             trigger_gl   TYPE saknr,
             "! <p class="shorttext synchronized">Product code</p>
             "! <p>Product code for the mapping</p>
             product_code TYPE char20,
             "! <p class="shorttext synchronized">Sales G/L account</p>
             "! <p>G/L account for sales revenue</p>
             sales_gl     TYPE saknr,
             "! <p class="shorttext synchronized">COS G/L account</p>
             "! <p>G/L account for cost of sales</p>
             cos_gl       TYPE saknr,
             "! <p class="shorttext synchronized">Margin percentage</p>
             "! <p>Margin percentage for COS calculation</p>
             margin_pct   TYPE dec5_2,
           END OF ty_cos_mapping.

    "! <p class="shorttext synchronized">Logger instance</p>
    "! <p>Dependency injected logger for application logging</p>
    DATA:
      mo_logger     TYPE REF TO zif_cos_logger,
      "! <p class="shorttext synchronized">Validator instance</p>
      "! <p>Dependency injected validator for business rule validation</p>
      mo_validator  TYPE REF TO zif_cos_validator.

    "! <p class="shorttext synchronized">Tolerance constant</p>
    "! <p>Minimum amount threshold for COS processing</p>
    CONSTANTS:
      c_tolerance     TYPE dmbtr VALUE '0.01',
      "! <p class="shorttext synchronized">Currency constant</p>
      "! <p>Default currency for COS processing</p>
      c_currency      TYPE waers VALUE 'GBP',
      "! <p class="shorttext synchronized">Document type constant</p>
      "! <p>Document type for COS documents</p>
      c_doc_type      TYPE blart VALUE 'AB'.

    "! <p class="shorttext synchronized">Validate input parameters</p>
    "! <p>Validates all input parameters for the processing method.
    "! Ensures GUID is not empty and all required fields are provided.</p>
    "! @parameter iv_guid | <p class="shorttext synchronized">Outbox entry GUID</p>
    "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code</p>
    "! @parameter iv_gjahr | <p class="shorttext synchronized">Fiscal year</p>
    "! @parameter iv_belnr | <p class="shorttext synchronized">Source document number</p>
    "! @parameter rv_valid | <p class="shorttext synchronized">True if all parameters are valid</p>
    METHODS:
      validate_input_parameters
        IMPORTING
          iv_guid  TYPE sysuuid_x16
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_belnr TYPE belnr_d
        RETURNING
          VALUE(rv_valid) TYPE abap_bool,

      "! <p class="shorttext synchronized">Check if feature is active</p>
      "! <p>Checks if the COS Auto Posting feature is currently active
      "! by querying the feature toggle configuration.</p>
      "! @parameter rv_active | <p class="shorttext synchronized">True if feature is active</p>
      check_feature_active
        RETURNING
          VALUE(rv_active) TYPE abap_bool,

      "! <p class="shorttext synchronized">Check user authorization</p>
      "! <p>Validates that the current user has authorization to process
      "! COS entries for the specified company code.</p>
      "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code to check</p>
      "! @parameter rv_authorized | <p class="shorttext synchronized">True if user is authorized</p>
      check_authorization
        IMPORTING
          iv_bukrs TYPE bukrs
        RETURNING
          VALUE(rv_authorized) TYPE abap_bool,

      "! <p class="shorttext synchronized">Load outbox entry</p>
      "! <p>Retrieves the outbox entry from the database using the provided GUID.
      "! Raises exception if entry is not found.</p>
      "! @parameter iv_guid | <p class="shorttext synchronized">Outbox entry GUID</p>
      "! @parameter rs_outbox | <p class="shorttext synchronized">Outbox entry data</p>
      "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If entry not found</p>
      load_outbox_entry
        IMPORTING
          iv_guid TYPE sysuuid_x16
        RETURNING
          VALUE(rs_outbox) TYPE zcos_outbox
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Check for duplicate processing</p>
      "! <p>Checks if the outbox entry has already been processed
      "! by looking for existing audit entries.</p>
      "! @parameter iv_guid | <p class="shorttext synchronized">Outbox entry GUID</p>
      "! @parameter rv_duplicate | <p class="shorttext synchronized">True if already processed</p>
      check_duplicate_processing
        IMPORTING
          iv_guid TYPE sysuuid_x16
        RETURNING
          VALUE(rv_duplicate) TYPE abap_bool,

      "! <p class="shorttext synchronized">Get COS mapping configuration</p>
      "! <p>Retrieves the COS mapping configuration for the specified
      "! company code, trigger G/L account, and product code.</p>
      "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code</p>
      "! @parameter iv_trigger_gl | <p class="shorttext synchronized">Trigger G/L account</p>
      "! @parameter iv_product_code | <p class="shorttext synchronized">Product code</p>
      "! @parameter rs_mapping | <p class="shorttext synchronized">COS mapping configuration</p>
      "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If mapping not found</p>
      get_cos_mapping
        IMPORTING
          iv_bukrs        TYPE bukrs
          iv_trigger_gl   TYPE saknr
          iv_product_code TYPE char20
        RETURNING
          VALUE(rs_mapping) TYPE ty_cos_mapping
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Calculate COS amount</p>
      "! <p>Calculates the COS amount based on the outbox data and mapping configuration.
      "! Applies margin percentage if configured, otherwise uses full amount.</p>
      "! @parameter iv_outbox | <p class="shorttext synchronized">Outbox entry data</p>
      "! @parameter iv_mapping | <p class="shorttext synchronized">COS mapping configuration</p>
      "! @parameter rv_amount | <p class="shorttext synchronized">Calculated COS amount</p>
      calculate_cos_amount
        IMPORTING
          iv_outbox  TYPE zcos_outbox
          iv_mapping TYPE ty_cos_mapping
        RETURNING
          VALUE(rv_amount) TYPE dmbtr,

      "! <p class="shorttext synchronized">Check amount tolerance</p>
      "! <p>Checks if the calculated amount meets the minimum tolerance threshold.
      "! Amounts below tolerance are skipped to avoid processing noise.</p>
      "! @parameter iv_amount | <p class="shorttext synchronized">Amount to check</p>
      "! @parameter rv_skip | <p class="shorttext synchronized">True if amount should be skipped</p>
      check_tolerance
        IMPORTING
          iv_amount TYPE dmbtr
        RETURNING
          VALUE(rv_skip) TYPE abap_bool,

      "! <p class="shorttext synchronized">Create COS document</p>
      "! <p>Creates the actual COS document using BAPI_ACC_DOCUMENT_POST.
      "! Generates a journal entry with COS and sales G/L accounts.</p>
      "! @parameter iv_outbox | <p class="shorttext synchronized">Outbox entry data</p>
      "! @parameter iv_mapping | <p class="shorttext synchronized">COS mapping configuration</p>
      "! @parameter iv_cos_amount | <p class="shorttext synchronized">COS amount to post</p>
      "! @parameter rs_result | <p class="shorttext synchronized">Processing result with document info</p>
      "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If document creation fails</p>
      create_cos_document
        IMPORTING
          iv_outbox     TYPE zcos_outbox
          iv_mapping    TYPE ty_cos_mapping
          iv_cos_amount TYPE dmbtr
        RETURNING
          VALUE(rs_result) TYPE ty_processing_result
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Create audit entry</p>
      "! <p>Creates an audit entry in the audit table to track the processing
      "! and ensure idempotency for duplicate processing prevention.</p>
      "! @parameter iv_guid | <p class="shorttext synchronized">Outbox entry GUID</p>
      "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code</p>
      "! @parameter iv_gjahr | <p class="shorttext synchronized">Fiscal year</p>
      "! @parameter iv_belnr | <p class="shorttext synchronized">Source document number</p>
      "! @parameter iv_cos_amount | <p class="shorttext synchronized">COS amount posted</p>
      "! @parameter iv_cos_currency | <p class="shorttext synchronized">COS currency</p>
      "! @parameter iv_belnr_cos | <p class="shorttext synchronized">COS document number</p>
      "! @parameter iv_gjahr_cos | <p class="shorttext synchronized">COS document year</p>
      "! @parameter rv_success | <p class="shorttext synchronized">True if audit entry created successfully</p>
      create_audit_entry
        IMPORTING
          iv_guid       TYPE sysuuid_x16
          iv_bukrs      TYPE bukrs
          iv_gjahr      TYPE gjahr
          iv_belnr      TYPE belnr_d
          iv_cos_amount TYPE dmbtr
          iv_cos_currency TYPE waers
          iv_belnr_cos  TYPE belnr_d
          iv_gjahr_cos  TYPE gjahr
        RETURNING
          VALUE(rv_success) TYPE abap_bool,

      "! <p class="shorttext synchronized">Update outbox status</p>
      "! <p>Updates the status of the outbox entry to reflect the processing result.
      "! Used to track pending, complete, error, or skip status.</p>
      "! @parameter iv_guid | <p class="shorttext synchronized">Outbox entry GUID</p>
      "! @parameter iv_status | <p class="shorttext synchronized">New status to set</p>
      "! @parameter iv_error_msg | <p class="shorttext synchronized">Error message (optional)</p>
      "! @parameter rv_success | <p class="shorttext synchronized">True if status updated successfully</p>
      update_outbox_status
        IMPORTING
          iv_guid        TYPE sysuuid_x16
          iv_status      TYPE char1
          iv_error_msg   TYPE string OPTIONAL
        RETURNING
          VALUE(rv_success) TYPE abap_bool.

ENDCLASS.

CLASS zcl_cos_qrfc_worker IMPLEMENTATION.

  METHOD constructor.
    " Initialize logger
    IF io_logger IS BOUND.
      mo_logger = io_logger.
    ELSE.
      mo_logger = NEW zcl_cos_logger( ).
    ENDIF.

    " Initialize validator
    mo_validator = NEW zcl_cos_validator( ).
  ENDMETHOD.

  METHOD process_outbox_entry.
    DATA: ls_outbox  TYPE zcos_outbox,
          ls_mapping TYPE ty_cos_mapping,
          lv_amount  TYPE dmbtr.

    " Initialize result
    CLEAR rv_result.

    " Validate input parameters
    IF validate_input_parameters(
      iv_guid = iv_guid
      iv_bukrs = iv_bukrs
      iv_gjahr = iv_gjahr
      iv_belnr = iv_belnr
    ) = abap_false.
      DATA(ls_invalid_input_msg) = zcl_cos_message_utility=>get_invalid_input_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_invalid_input_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Check feature toggle
    IF check_feature_active( ) = abap_false.
      DATA(ls_feature_msg) = zcl_cos_message_utility=>get_feature_not_active_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_feature_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Check authorization
    IF check_authorization( iv_bukrs ) = abap_false.
      DATA(ls_auth_msg) = zcl_cos_message_utility=>get_authorization_error( ).
      mo_logger->log_error( iv_message_id = 'ZCOS' iv_message_no = ls_auth_msg-msgno ).
      mo_logger->save_log( ).
      rv_result = VALUE #( success = abap_false error_message = ls_auth_msg-msgv1 ).
      RETURN.
    ENDIF.

    TRY.
        " Load outbox entry
        ls_outbox = load_outbox_entry( iv_guid ).

        " Check for duplicate processing
        IF check_duplicate_processing( iv_guid ) = abap_true.
          DATA(ls_duplicate_msg) = zcl_cos_message_utility=>get_already_processed_error( ).
          rv_result = VALUE #( success = abap_false error_message = ls_duplicate_msg-msgv1 ).
          RETURN.
        ENDIF.

        " Get COS mapping
        ls_mapping = get_cos_mapping(
          iv_bukrs = iv_bukrs
          iv_trigger_gl = ls_outbox-trigger_gl
          iv_product_code = ls_outbox-product_code
        ).

        " Calculate COS amount
        lv_amount = calculate_cos_amount(
          iv_outbox = ls_outbox
          iv_mapping = ls_mapping
        ).

        " Check tolerance
        IF check_tolerance( lv_amount ) = abap_true.
          DATA(ls_tolerance_msg) = zcl_cos_message_utility=>get_amount_below_tolerance_warning(
            iv_amount = lv_amount
            iv_tolerance = c_tolerance
          ).
          update_outbox_status(
            iv_guid = iv_guid
            iv_status = 'S'
            iv_error_msg = ls_tolerance_msg-msgv1
          ).
          rv_result = VALUE #( success = abap_false error_message = ls_tolerance_msg-msgv1 ).
          RETURN.
        ENDIF.

        " Create COS document
        rv_result = create_cos_document(
          iv_outbox = ls_outbox
          iv_mapping = ls_mapping
          iv_cos_amount = lv_amount
        ).

        IF rv_result-success = abap_true.
          " Create audit entry
          create_audit_entry(
            iv_guid = iv_guid
            iv_bukrs = iv_bukrs
            iv_gjahr = iv_gjahr
            iv_belnr = iv_belnr
            iv_cos_amount = lv_amount
            iv_cos_currency = ls_outbox-cos_amount_currency
            iv_belnr_cos = rv_result-cos_document
            iv_gjahr_cos = rv_result-cos_year
          ).

          " Update outbox status
          update_outbox_status(
            iv_guid = iv_guid
            iv_status = 'C'
          ).

          mo_logger->log_success(
            iv_message_id = 'ZCOS'
            iv_message_no = '003'
            iv_message_v1 = rv_result-cos_document
            iv_message_v2 = rv_result-cos_year
          ).
        ELSE.
          " Update outbox status with error
          update_outbox_status(
            iv_guid = iv_guid
            iv_status = 'E'
            iv_error_msg = rv_result-error_message
          ).
        ENDIF.

        " Save log
        mo_logger->save_log( ).

      CATCH zcx_cos_processing_error INTO DATA(lx_error).
        rv_result = VALUE #(
          success = abap_false
          error_message = lx_error->get_text( )
        ).
        mo_logger->log_error(
          iv_message_id = 'ZCOS'
          iv_message_no = '008'
          iv_message_v1 = lx_error->get_text( )
        ).
        mo_logger->save_log( ).
    ENDTRY.

  ENDMETHOD.

  METHOD validate_input_parameters.
    DATA: lv_result TYPE zif_cos_validator=>ty_validation_result.

    " Validate GUID
    lv_result = mo_validator->validate_guid( iv_guid ).
    IF lv_result-is_valid = abap_false.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate company code
    lv_result = mo_validator->validate_company_code( iv_bukrs ).
    IF lv_result-is_valid = abap_false.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate fiscal year
    lv_result = mo_validator->validate_fiscal_year( iv_gjahr ).
    IF lv_result-is_valid = abap_false.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate document number
    lv_result = mo_validator->validate_document_number( iv_belnr ).
    IF lv_result-is_valid = abap_false.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD check_feature_active.
    DATA: lo_feature_toggle TYPE REF TO zcl_cos_feature_toggle.

    lo_feature_toggle = NEW zcl_cos_feature_toggle( ).
    rv_active = lo_feature_toggle->is_feature_active( 'ZCOS_E003_ACTIVE' ).
  ENDMETHOD.

  METHOD check_authorization.
    AUTHORITY-CHECK OBJECT 'ZCOS_POST'
      ID 'ACTVT' FIELD '01'
      ID 'BUKRS' FIELD iv_bukrs
      ID 'SAKNR' FIELD '0000000000'.

    rv_authorized = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD load_outbox_entry.
    SELECT SINGLE client, guid, bukrs, gjahr, belnr_src, trigger_gl, product_code, 
                  total_charge, status, error_message, created_at, processed_at
      FROM zcos_outbox INTO @rs_outbox
      WHERE guid = @iv_guid.
  
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cos_processing_error
        EXPORTING
          textid = zcx_cos_processing_error=>outbox_not_found
          guid = iv_guid.
    ENDIF.
  ENDMETHOD.

  METHOD check_duplicate_processing.
    " Note: ZCOS_AUD is custom table, no standard VDM view available
    SELECT SINGLE client FROM zcos_aud
      INTO @DATA(lv_client)
      WHERE guid = @iv_guid.
    
    rv_duplicate = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD get_cos_mapping.
    " Note: ZCOS_MAP is custom table, no standard VDM view available
    SELECT SINGLE bukrs, trigger_gl, product_code, sales_gl, cos_gl, margin_pct
      FROM zcos_map
      INTO CORRESPONDING FIELDS OF @rs_mapping
      WHERE bukrs = @iv_bukrs
        AND trigger_gl = @iv_trigger_gl
        AND product_code = @iv_product_code
        AND valid_from <= @sy-datum
        AND valid_to >= @sy-datum
        AND deleted = @abap_false.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cos_processing_error
        EXPORTING
          textid = zcx_cos_processing_error=>mapping_not_found
          trigger_gl = iv_trigger_gl
          product_code = iv_product_code.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_cos_amount.
    " Apply margin if specified
    IF iv_mapping-margin_pct IS NOT INITIAL.
      rv_amount = iv_outbox-total_charge * ( 100 - iv_mapping-margin_pct ) / 100.
    ELSE.
      rv_amount = iv_outbox-total_charge.
    ENDIF.

    " Round to 2 decimals
    rv_amount = round( val = rv_amount dec = 2 ).
  ENDMETHOD.

  METHOD check_tolerance.
    rv_skip = COND #( WHEN abs( iv_amount ) < c_tolerance THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD create_cos_document.
    DATA: ls_document_header TYPE bapiache09,
          lt_accountgl       TYPE TABLE OF bapiacgl09,
          ls_accountgl       TYPE bapiacgl09,
          lt_currencyamount  TYPE TABLE OF bapiaccr09,
          ls_currencyamount  TYPE bapiaccr09,
          lt_return          TYPE TABLE OF bapiret2,
          ls_return          TYPE bapiret2,
          lv_guid            TYPE sysuuid_x16.

    " Generate GUID
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = lv_guid.

    " Document header
    ls_document_header-obj_type = 'BKPFF'.
    ls_document_header-username = sy-uname.
    ls_document_header-header_txt = |COS Auto Posting for { iv_outbox-belnr_src }|.
    ls_document_header-comp_code = iv_outbox-bukrs.
    ls_document_header-doc_date = sy-datum.
    ls_document_header-pstng_date = sy-datum.
    ls_document_header-doc_type = c_doc_type.
    ls_document_header-ref_doc_no = iv_outbox-belnr_src.

    " COS line (Debit)
    ls_accountgl-itemno_acc = '1'.
    ls_accountgl-gl_account = iv_mapping-cos_gl.
    ls_accountgl-alloc_nmbr = 'COS'.
    APPEND ls_accountgl TO lt_accountgl.

    ls_currencyamount-itemno_acc = '1'.
    ls_currencyamount-currency = iv_outbox-total_charge_currency.
    ls_currencyamount-amt_doccur = iv_cos_amount.
    APPEND ls_currencyamount TO lt_currencyamount.

    " Sales line (Credit)
    ls_accountgl-itemno_acc = '2'.
    ls_accountgl-gl_account = iv_mapping-sales_gl.
    ls_accountgl-alloc_nmbr = 'COS'.
    APPEND ls_accountgl TO lt_accountgl.

    ls_currencyamount-itemno_acc = '2'.
    ls_currencyamount-currency = iv_outbox-total_charge_currency.
    ls_currencyamount-amt_doccur = -iv_cos_amount.
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
      RAISE EXCEPTION TYPE zcx_cos_processing_error
        EXPORTING
          textid = zcx_cos_processing_error=>document_creation_failed
          error_message = ls_return-message.
    ENDIF.

    " Extract document number and year
    rs_result-success = abap_true.
    rs_result-cos_document = ls_document_header-obj_key+0(10).
    rs_result-cos_year = sy-datum(4).
    rs_result-cos_amount = iv_cos_amount.

    " Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ENDMETHOD.

  METHOD create_audit_entry.
    DATA: ls_audit TYPE zcos_aud.

    ls_audit-client = sy-mandt.
    ls_audit-guid = iv_guid.
    ls_audit-bukrs = iv_bukrs.
    ls_audit-belnr_cos = iv_belnr_cos.
    ls_audit-gjahr = iv_gjahr_cos.
    ls_audit-belnr_src = iv_belnr.
    ls_audit-posted_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_audit-posted_by = sy-uname.
    ls_audit-cos_amount = iv_cos_amount.
    ls_audit-cos_amount_currency = iv_cos_currency.
    ls_audit-status = 'P'.

    INSERT zcos_aud FROM ls_audit.
    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    IF rv_success = abap_true.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD update_outbox_status.
    UPDATE zcos_outbox SET 
      status = @iv_status,
      error_message = @iv_error_msg,
      processed_at = @( cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ) )
    WHERE guid = @iv_guid.

    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    IF rv_success = abap_true.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

