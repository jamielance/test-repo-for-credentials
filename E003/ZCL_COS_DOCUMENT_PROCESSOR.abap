*&---------------------------------------------------------------------*
*& Class: ZCL_COS_DOCUMENT_PROCESSOR
*& Description: Document processor for COS auto posting
*&---------------------------------------------------------------------*
CLASS zcl_cos_document_processor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">COS mapping structure</p>
    "! <p>Contains the mapping configuration for COS processing including
    "! G/L accounts, product codes, and margin percentages.</p>
    TYPES: BEGIN OF ty_cos_mapping,
             "! <p class="shorttext synchronized">Source G/L account</p>
             "! <p>G/L account that triggers COS processing</p>
             source_gl    TYPE hkont,
             "! <p class="shorttext synchronized">Sales G/L account</p>
             "! <p>G/L account for sales revenue</p>
             sales_gl     TYPE hkont,
             "! <p class="shorttext synchronized">COS G/L account</p>
             "! <p>G/L account for cost of sales</p>
             cos_gl       TYPE hkont,
             "! <p class="shorttext synchronized">Net margin G/L account</p>
             "! <p>G/L account for net margin</p>
             netmargin_gl TYPE hkont,
             "! <p class="shorttext synchronized">Product code</p>
             "! <p>Product code for the mapping</p>
             productcode  TYPE matnr,
             "! <p class="shorttext synchronized">Valid from</p>
             "! <p>Valid from date</p>
             validfrom    TYPE datuv,
             "! <p class="shorttext synchronized">Valid to</p>
             "! <p>Valid to date</p>
             validto      TYPE datbi,
             "! <p class="shorttext synchronized">Active</p>
             "! <p>Active status</p>
             active       TYPE activ,
           END OF ty_cos_mapping.

    "! <p class="shorttext synchronized">Processing result structure</p>
    "! <p>Contains the result of document processing operations including
    "! success status, generated GUID, and error details.</p>
    TYPES: BEGIN OF ty_processing_result,
             "! <p class="shorttext synchronized">Processing success flag</p>
             "! <p>True if processing completed successfully</p>
             success       TYPE abap_bool,
             "! <p class="shorttext synchronized">Generated GUID</p>
             "! <p>GUID of the created outbox entry</p>
             guid          TYPE sysuuid_x16,
             "! <p class="shorttext synchronized">Error message</p>
             "! <p>Error message if processing failed</p>
             error_message TYPE string,
           END OF ty_processing_result.

    "! <p class="shorttext synchronized">Constructor</p>
    "! <p>Creates a new instance of the document processor with optional dependencies.
    "! Uses dependency injection for logger and validator for better testability.</p>
    "! @parameter io_logger | <p class="shorttext synchronized">Logger instance (optional)</p>
    METHODS:
      constructor
        IMPORTING
          io_logger TYPE REF TO zif_cos_logger OPTIONAL,

      "! <p class="shorttext synchronized">Process document</p>
    "! <p>Main processing method that handles a single FI document.
    "! Performs feature checks, validation, mapping lookup, and outbox creation.
    "! This is the core method called by the BAdI implementation.</p>
    "! @parameter iv_document | <p class="shorttext synchronized">FI document to process</p>
    "! @parameter rv_result | <p class="shorttext synchronized">Processing result with GUID</p>
      process_document
        IMPORTING
          iv_document TYPE accit
        RETURNING
          VALUE(rv_result) TYPE ty_processing_result.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Logger instance</p>
    "! <p>Dependency injected logger for application logging</p>
    DATA:
      mo_logger     TYPE REF TO zif_cos_logger,
      "! <p class="shorttext synchronized">Validator instance</p>
      "! <p>Dependency injected validator for business rule validation</p>
      mo_validator  TYPE REF TO zif_cos_validator.

    "! <p class="shorttext synchronized">Check if feature is active</p>
    "! <p>Checks if the COS Auto Posting feature is currently active
    "! by querying the feature toggle configuration.</p>
    "! @parameter rv_active | <p class="shorttext synchronized">True if feature is active</p>
    METHODS:
      check_feature_active
        RETURNING
          VALUE(rv_active) TYPE abap_bool,

      "! <p class="shorttext synchronized">Check E008 validation</p>
      "! <p>Validates that the document passes E008 business rules.
      "! This includes checking document status, posting period, and other validations.</p>
      "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code</p>
      "! @parameter iv_gjahr | <p class="shorttext synchronized">Fiscal year</p>
      "! @parameter iv_belnr | <p class="shorttext synchronized">Document number</p>
      "! @parameter rv_passed | <p class="shorttext synchronized">True if validation passed</p>
      check_e008_validation
        IMPORTING
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_belnr TYPE belnr_d
        RETURNING
          VALUE(rv_passed) TYPE abap_bool,

      "! <p class="shorttext synchronized">Find trigger G/L account</p>
      "! <p>Identifies the trigger G/L account from the FI document entries
      "! that should initiate COS processing based on business rules.</p>
      "! @parameter it_accit | <p class="shorttext synchronized">FI document entries to analyze</p>
      "! @parameter rv_trigger_gl | <p class="shorttext synchronized">Trigger G/L account found</p>
      find_trigger_gl
        IMPORTING
          it_accit TYPE accit_t
        RETURNING
          VALUE(rv_trigger_gl) TYPE saknr,

      "! <p class="shorttext synchronized">Extract product code</p>
      "! <p>Extracts the product code from the FI document entry
      "! for mapping lookup and COS calculation.</p>
      "! @parameter is_accit | <p class="shorttext synchronized">FI document entry to extract from</p>
      "! @parameter rv_product_code | <p class="shorttext synchronized">Extracted product code</p>
      extract_product_code
        IMPORTING
          is_accit TYPE accit
        RETURNING
          VALUE(rv_product_code) TYPE char20,

      "! <p class="shorttext synchronized">Calculate total charge</p>
      "! <p>Calculates the total charge amount from FI document entries
      "! for the specified trigger G/L account.</p>
      "! @parameter it_accit | <p class="shorttext synchronized">FI document entries to sum</p>
      "! @parameter iv_trigger_gl | <p class="shorttext synchronized">Trigger G/L account</p>
      "! @parameter rv_total_charge | <p class="shorttext synchronized">Total charge amount</p>
      calculate_total_charge
        IMPORTING
          it_accit TYPE accit_t
          iv_trigger_gl TYPE saknr
        RETURNING
          VALUE(rv_total_charge) TYPE dmbtr,

      "! <p class="shorttext synchronized">Get COS mapping configuration</p>
      "! <p>Retrieves the COS mapping configuration for the specified
      "! company code, trigger G/L account, and product code.</p>
      "! @parameter iv_trigger_gl | <p class="shorttext synchronized">Source G/L account</p>
      "! @parameter iv_product_code | <p class="shorttext synchronized">Product code</p>
      "! @parameter rs_mapping | <p class="shorttext synchronized">COS mapping configuration</p>
      get_cos_mapping
        IMPORTING
          iv_trigger_gl   TYPE hkont
          iv_product_code TYPE matnr
        RETURNING
          VALUE(rs_mapping) TYPE ty_cos_mapping,

      "! <p class="shorttext synchronized">Create outbox entry</p>
      "! <p>Creates a new outbox entry in the ZCOS_OUTBOX table
      "! with all required data for qRFC processing.</p>
      "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code</p>
      "! @parameter iv_gjahr | <p class="shorttext synchronized">Fiscal year</p>
      "! @parameter iv_belnr | <p class="shorttext synchronized">Source document number</p>
      "! @parameter iv_trigger_gl | <p class="shorttext synchronized">Trigger G/L account</p>
      "! @parameter iv_product_code | <p class="shorttext synchronized">Product code</p>
      "! @parameter iv_total_charge | <p class="shorttext synchronized">Total charge amount</p>
      "! @parameter rv_guid | <p class="shorttext synchronized">Generated GUID for outbox entry</p>
      create_outbox_entry
        IMPORTING
          iv_bukrs        TYPE bukrs
          iv_gjahr        TYPE gjahr
          iv_belnr        TYPE belnr_d
          iv_trigger_gl   TYPE saknr
          iv_product_code TYPE char20
          iv_total_charge TYPE dmbtr
        RETURNING
          VALUE(rv_guid) TYPE sysuuid_x16,

      "! <p class="shorttext synchronized">Enqueue qRFC unit</p>
      "! <p>Enqueues a qRFC unit for asynchronous processing
      "! of the created outbox entry.</p>
      "! @parameter iv_guid | <p class="shorttext synchronized">Outbox entry GUID</p>
      "! @parameter iv_bukrs | <p class="shorttext synchronized">Company code</p>
      "! @parameter iv_gjahr | <p class="shorttext synchronized">Fiscal year</p>
      "! @parameter iv_belnr | <p class="shorttext synchronized">Source document number</p>
      enqueue_qrfc_unit
        IMPORTING
          iv_guid  TYPE sysuuid_x16
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_belnr TYPE belnr_d.

ENDCLASS.

CLASS zcl_cos_document_processor IMPLEMENTATION.

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

  METHOD process_document.
    DATA: lv_active        TYPE abap_bool,
          lv_e008_passed   TYPE abap_bool,
          lv_trigger_gl    TYPE saknr,
          lv_product_code  TYPE char20,
          lv_total_charge  TYPE dmbtr,
          lv_guid          TYPE sysuuid_x16,
          ls_mapping       TYPE ty_cos_mapping.

    " Initialize result
    CLEAR rv_result.

    " Check if feature is active
    lv_active = check_feature_active( ).
    IF lv_active = abap_false.
      DATA(ls_feature_msg) = zcl_cos_message_utility=>get_feature_not_active_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_feature_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Find trigger G/L account
    lv_trigger_gl = find_trigger_gl( it_accit = iv_document-accit ).
    IF lv_trigger_gl IS INITIAL.
      DATA(ls_trigger_msg) = zcl_cos_message_utility=>get_no_trigger_gl_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_trigger_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Extract product code from first matching line
    LOOP AT iv_document-accit INTO DATA(ls_accit) WHERE hkont = lv_trigger_gl.
      lv_product_code = extract_product_code( ls_accit ).
      EXIT.
    ENDLOOP.

    IF lv_product_code IS INITIAL.
      DATA(ls_product_msg) = zcl_cos_message_utility=>get_product_code_not_found_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_product_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Calculate total charge
    lv_total_charge = calculate_total_charge(
      it_accit = iv_document-accit
      iv_trigger_gl = lv_trigger_gl
    ).

    " Check E008 validation passed
    lv_e008_passed = check_e008_validation(
      iv_bukrs = iv_document-header-bukrs
      iv_gjahr = iv_document-header-gjahr
      iv_belnr = iv_document-header-belnr
    ).
    IF lv_e008_passed = abap_false.
      DATA(ls_e008_msg) = zcl_cos_message_utility=>get_e008_validation_failed_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_e008_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Get COS mapping
    ls_mapping = get_cos_mapping(
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
    ).
    IF ls_mapping IS INITIAL.
      DATA(ls_mapping_msg) = zcl_cos_message_utility=>get_cos_mapping_not_found_error(
        iv_trigger_gl = lv_trigger_gl
        iv_product_code = lv_product_code
      ).
      rv_result = VALUE #( success = abap_false error_message = ls_mapping_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Create outbox entry
    lv_guid = create_outbox_entry(
      iv_bukrs = iv_document-header-bukrs
      iv_gjahr = iv_document-header-gjahr
      iv_belnr = iv_document-header-belnr
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
      iv_total_charge = lv_total_charge
    ).

    IF lv_guid IS INITIAL.
      DATA(ls_outbox_msg) = zcl_cos_message_utility=>get_outbox_creation_failed_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_outbox_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Enqueue qRFC unit
    enqueue_qrfc_unit(
      iv_guid = lv_guid
      iv_bukrs = iv_document-header-bukrs
      iv_gjahr = iv_document-header-gjahr
      iv_belnr = iv_document-header-belnr
    ).

    rv_result = VALUE #( success = abap_true guid = lv_guid ).

    " Log success
    mo_logger->log_success(
      iv_message_id = 'ZCOS'
      iv_message_no = '020'
      iv_message_v1 = lv_guid
    ).
    mo_logger->save_log( ).

  ENDMETHOD.

  METHOD check_feature_active.
    DATA: lo_feature_toggle TYPE REF TO zcl_cos_feature_toggle.

    lo_feature_toggle = NEW zcl_cos_feature_toggle( ).
    rv_active = lo_feature_toggle->is_feature_active( 'ZCOS_E003_ACTIVE' ).
  ENDMETHOD.

  METHOD check_e008_validation.
    " Check if E008 validation passed
    " This would typically check a custom table or field that E008 sets
    DATA: lv_e008_status TYPE char1.
    
    " Placeholder - implement based on E008's validation result storage
    " Note: ZCOS_E008_STATUS is custom table, no standard VDM view available
    SELECT SINGLE e008_status FROM zcos_e008_status INTO @lv_e008_status
      WHERE bukrs = @iv_bukrs
        AND gjahr = @iv_gjahr
        AND belnr = @iv_belnr.
    
    rv_passed = COND #( WHEN lv_e008_status = 'P' THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD find_trigger_gl.
    " Find trigger G/L account by checking mapping table
    LOOP AT it_accit INTO DATA(ls_accit).
      IF ls_accit-hkont IS NOT INITIAL.
        " Check if this is a trigger G/L
        " Note: ZMAP_COS_RULES is custom table, no standard VDM view available
        SELECT SINGLE source_gl FROM zmap_cos_rules
          INTO @rv_trigger_gl
          WHERE source_gl = @ls_accit-hkont
            AND validfrom <= @sy-datum
            AND validto >= @sy-datum
            AND active = @abap_true.
        
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_product_code.
    " Extract product code from line item (assuming it's in field assignment)
    rv_product_code = is_accit-zuonr(20).
  ENDMETHOD.

  METHOD calculate_total_charge.
    " Calculate total charge for trigger G/L lines
    LOOP AT it_accit INTO DATA(ls_accit) WHERE hkont = iv_trigger_gl.
      rv_total_charge = rv_total_charge + ls_accit-dmbtr.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_cos_mapping.
    SELECT SINGLE source_gl, sales_gl, cos_gl, netmargin_gl, productcode, validfrom, validto, active
      FROM zmap_cos_rules
      INTO CORRESPONDING FIELDS OF @rs_mapping
      WHERE source_gl = @iv_trigger_gl
        AND productcode = @iv_product_code
        AND validfrom <= @sy-datum
        AND validto >= @sy-datum
        AND active = @abap_true.
  ENDMETHOD.

  METHOD create_outbox_entry.
    DATA: ls_outbox TYPE zcos_outbox.
    
    " Generate GUID
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = rv_guid.
    
    " Fill outbox entry
    ls_outbox-client = sy-mandt.
    ls_outbox-guid = rv_guid.
    ls_outbox-bukrs = iv_bukrs.
    ls_outbox-gjahr = iv_gjahr.
    ls_outbox-belnr_src = iv_belnr.
    ls_outbox-trigger_gl = iv_trigger_gl.
    ls_outbox-product_code = iv_product_code.
    ls_outbox-total_charge = iv_total_charge.
    ls_outbox-total_charge_currency = 'GBP'. " Default currency
    ls_outbox-cos_amount_currency = 'GBP'. " Default currency
    ls_outbox-created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_outbox-status = 'P'. " Pending
    
    INSERT zcos_outbox FROM ls_outbox.
    COMMIT WORK.
  ENDMETHOD.

  METHOD enqueue_qrfc_unit.
    DATA: lv_queue_name TYPE trfcqnam,
          lv_unit_name  TYPE trfcqout-name.
    
    " Build queue name: COS_<BUKRS>_<YYYYMMDD>
    lv_queue_name = |COS_{ iv_bukrs }_{ sy-datum }|.
    
    " Build unit name
    lv_unit_name = |COS_{ iv_guid }|.
    
    " Enqueue qRFC unit
    CALL FUNCTION 'TRFC_SET_QUEUE_NAME'
      EXPORTING
        qname = lv_queue_name.
    
    CALL FUNCTION 'Z_COS_QRFC_WORKER'
      IN BACKGROUND TASK
      EXPORTING
        iv_guid  = iv_guid
        iv_bukrs = iv_bukrs
        iv_gjahr = iv_gjahr
        iv_belnr = iv_belnr.
  ENDMETHOD.

ENDCLASS.

