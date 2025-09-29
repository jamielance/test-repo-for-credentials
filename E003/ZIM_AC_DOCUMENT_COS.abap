*&---------------------------------------------------------------------*
*& Class: ZCL_COS_AC_DOCUMENT_HANDLER
*& Description: AC Document Handler for COS Auto Posting - Consuming Class
*&---------------------------------------------------------------------*
CLASS zcl_cos_ac_document_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Process AC document for COS</p>
    "! <p>Main entry point for processing AC documents to create COS outbox entries.
    "! This method should be called from the IF_EX_ACC_DOCUMENT change method
    "! to trigger COS processing for relevant documents.</p>
    "! @parameter it_accit | <p class="shorttext synchronized">AC document entries to process</p>
    "! @parameter rv_processed | <p class="shorttext synchronized">True if any entries were processed</p>
    "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If processing fails</p>
    METHODS:
      process_ac_document
        IMPORTING
          it_accit TYPE acdoca_t
        RETURNING
          VALUE(rv_processed) TYPE abap_bool
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Check if document should be processed</p>
      "! <p>Determines if the AC document should be processed for COS
      "! based on business rules and feature toggle status.</p>
      "! @parameter it_accit | <p class="shorttext synchronized">AC document entries to check</p>
      "! @parameter rv_should_process | <p class="shorttext synchronized">True if should be processed</p>
      should_process_document
        IMPORTING
          it_accit TYPE acdoca_t
        RETURNING
          VALUE(rv_should_process) TYPE abap_bool.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Document processor instance</p>
    "! <p>Instance of the COS document processor for handling AC document processing</p>
    DATA:
      mo_document_processor TYPE REF TO zcl_cos_document_processor,
      "! <p class="shorttext synchronized">Logger instance</p>
      "! <p>Logger for application logging and debugging</p>
      mo_logger             TYPE REF TO zif_cos_logger.

    "! <p class="shorttext synchronized">Get document processor</p>
    "! <p>Returns the document processor instance, creating it if necessary.
    "! Uses lazy initialization for better performance.</p>
    "! @parameter ro_processor | <p class="shorttext synchronized">Document processor instance</p>
    METHODS:
      get_document_processor
        RETURNING
          VALUE(ro_processor) TYPE REF TO zcl_cos_document_processor,

      "! <p class="shorttext synchronized">Get logger</p>
      "! <p>Returns the logger instance, creating it if necessary.
    "! Uses lazy initialization for better performance.</p>
    "! @parameter ro_logger | <p class="shorttext synchronized">Logger instance</p>
      get_logger
        RETURNING
          VALUE(ro_logger) TYPE REF TO zif_cos_logger,

      "! <p class="shorttext synchronized">Check feature toggle</p>
      "! <p>Checks if the COS Auto Posting feature is currently active.</p>
      "! @parameter rv_active | <p class="shorttext synchronized">True if feature is active</p>
      check_feature_active
        RETURNING
          VALUE(rv_active) TYPE abap_bool,

      "! <p class="shorttext synchronized">Find trigger G/L accounts</p>
      "! <p>Identifies trigger G/L accounts in the AC document that should
      "! initiate COS processing based on business rules.</p>
      "! @parameter it_accit | <p class="shorttext synchronized">AC document entries</p>
      "! @parameter rt_trigger_gls | <p class="shorttext synchronized">Trigger G/L accounts found</p>
      find_trigger_gl_accounts
        IMPORTING
          it_accit TYPE acdoca_t
        RETURNING
          VALUE(rt_trigger_gls) TYPE TABLE OF saknr.

ENDCLASS.

CLASS zcl_cos_ac_document_handler IMPLEMENTATION.

  "! <p class="shorttext synchronized">Process AC document for COS</p>
  "! <p>Main entry point for processing AC documents to create COS outbox entries.
  "! This method should be called from the IF_EX_ACC_DOCUMENT change method.</p>
  METHOD process_ac_document.
    DATA: lo_processor TYPE REF TO zcl_cos_document_processor,
          ls_result    TYPE zcl_cos_document_processor=>ty_processing_result,
          lt_trigger_gls TYPE TABLE OF saknr,
          lv_processed TYPE abap_bool.

    " Check if document should be processed
    IF should_process_document( it_accit ) = abap_false.
      rv_processed = abap_false.
      RETURN.
    ENDIF.

    " Get logger and log start of processing
    DATA(lo_logger) = get_logger( ).
    lo_logger->log_info(
      iv_message_id = 'ZCOS'
      iv_message_no = '010'
      iv_message_v1 = |Processing AC document with { lines( it_accit ) } entries|
    ).

    " Find trigger G/L accounts
    lt_trigger_gls = find_trigger_gl_accounts( it_accit ).

    " Process each trigger G/L account
    LOOP AT lt_trigger_gls INTO DATA(lv_trigger_gl).
      " Get document processor
      lo_processor = get_document_processor( ).

      " Process document for this trigger G/L account
      ls_result = lo_processor->process_document( VALUE #( 
        FOR ls_accit IN it_accit WHERE ( saknr = lv_trigger_gl )
        ( ls_accit )
      ) ).

      " Log result
      IF ls_result-success = abap_true.
        lo_logger->log_success(
          iv_message_id = 'ZCOS'
          iv_message_no = '011'
          iv_message_v1 = ls_result-guid
          iv_message_v2 = lv_trigger_gl
        ).
        lv_processed = abap_true.
      ELSE.
        lo_logger->log_error(
          iv_message_id = 'ZCOS'
          iv_message_no = '012'
          iv_message_v1 = ls_result-error_message
          iv_message_v2 = lv_trigger_gl
        ).
      ENDIF.
    ENDLOOP.

    " Save log
    lo_logger->save_log( ).

    rv_processed = lv_processed.
  ENDMETHOD.

  "! <p class="shorttext synchronized">Check if document should be processed</p>
  "! <p>Determines if the AC document should be processed for COS
  "! based on business rules and feature toggle status.</p>
  METHOD should_process_document.
    " Check feature toggle
    IF check_feature_active( ) = abap_false.
      rv_should_process = abap_false.
      RETURN.
    ENDIF.

    " Check if document has trigger G/L accounts
    DATA(lt_trigger_gls) = find_trigger_gl_accounts( it_accit ).
    rv_should_process = COND #( WHEN lines( lt_trigger_gls ) > 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  "! <p class="shorttext synchronized">Get document processor</p>
  "! <p>Returns the document processor instance, creating it if necessary.
  "! Uses lazy initialization for better performance and resource management.</p>
  METHOD get_document_processor.
    " Create processor instance if not already created
    IF mo_document_processor IS NOT BOUND.
      mo_document_processor = NEW zcl_cos_document_processor( ).
    ENDIF.

    ro_processor = mo_document_processor.
  ENDMETHOD.

  "! <p class="shorttext synchronized">Get logger</p>
  "! <p>Returns the logger instance, creating it if necessary.
  "! Uses lazy initialization for better performance and resource management.</p>
  METHOD get_logger.
    " Create logger instance if not already created
    IF mo_logger IS NOT BOUND.
      mo_logger = NEW zcl_cos_logger( ).
    ENDIF.

    ro_logger = mo_logger.
  ENDMETHOD.

  "! <p class="shorttext synchronized">Check feature toggle</p>
  "! <p>Checks if the COS Auto Posting feature is currently active.</p>
  METHOD check_feature_active.
    DATA: lo_feature_toggle TYPE REF TO zcl_cos_feature_toggle.

    " Create feature toggle instance
    lo_feature_toggle = NEW zcl_cos_feature_toggle( ).

    " Check if feature is active
    rv_active = lo_feature_toggle->is_feature_active( 'ZCOS_E003_ACTIVE' ).
  ENDMETHOD.

  "! <p class="shorttext synchronized">Find trigger G/L accounts</p>
  "! <p>Identifies trigger G/L accounts in the AC document that should
  "! initiate COS processing based on business rules.</p>
  METHOD find_trigger_gl_accounts.
    " This is a simplified implementation - in reality, you would have
    " business rules to determine which G/L accounts are triggers
    " For now, we'll look for specific G/L account patterns
    
    " Example: Look for G/L accounts that start with specific patterns
    " or are in specific ranges that indicate COS triggers
    
    " This should be replaced with actual business logic
    LOOP AT it_accit INTO DATA(ls_accit).
      " Example business rule: G/L accounts 400000-499999 are COS triggers
      IF ls_accit-saknr >= '400000' AND ls_accit-saknr <= '499999'.
        APPEND ls_accit-saknr TO rt_trigger_gls.
      ENDIF.
    ENDLOOP.

    " Remove duplicates
    SORT rt_trigger_gls.
    DELETE ADJACENT DUPLICATES FROM rt_trigger_gls.
  ENDMETHOD.

ENDCLASS.
