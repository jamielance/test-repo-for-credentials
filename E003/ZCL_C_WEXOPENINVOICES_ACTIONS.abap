*&---------------------------------------------------------------------*
*& Action Implementation: ZCL_C_WEXOPENINVOICES_ACTIONS
*& Purpose: Implementation of FindMatch and ReleaseIfMatch actions
*& Namespace: ZHLM
*& Package: ZHLM_AP_WEX
*&---------------------------------------------------------------------*

CLASS zcl_c_wexopeninvoices_actions DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_bo_action.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " Constants for error handling
    CONSTANTS:
      c_error_no_candidate TYPE string VALUE 'NO_CANDIDATE',
      c_error_ambiguous TYPE string VALUE 'AMBIGUOUS_CANDIDATES',
      c_error_currency_mismatch TYPE string VALUE 'CURRENCY_MISMATCH',
      c_error_amount_out_of_tolerance TYPE string VALUE 'AMOUNT_OUT_OF_TOLERANCE',
      c_error_not_blocked TYPE string VALUE 'NOT_BLOCKED'.
      
    " Helper methods
    METHODS:
      get_config_tolerance
        IMPORTING
          iv_company_code TYPE bukrs
        RETURNING
          VALUE(rv_tolerance) TYPE curr13_2,
          
      find_matching_invoices
        IMPORTING
          iv_company_code TYPE bukrs
          iv_wex_uuid TYPE zhlm_de_wex_uuid
        RETURNING
          VALUE(rt_invoices) TYPE TABLE,
          
      validate_match
        IMPORTING
          iv_invoice_amount TYPE curr13_2
          iv_expected_amount TYPE curr13_2
          iv_currency TYPE waers
          iv_expected_currency TYPE waers
          iv_tolerance TYPE curr13_2
        RAISING
          cx_rap_business_exception.

ENDCLASS.

CLASS zcl_c_wexopeninvoices_actions IMPLEMENTATION.

  METHOD if_rap_bo_action~findmatch.
    " Implementation of FindMatch action
    DATA:
      lv_company_code TYPE bukrs,
      lv_wex_uuid TYPE zhlm_de_wex_uuid,
      lv_amount TYPE curr13_2,
      lv_currency TYPE waers,
      lv_tolerance TYPE curr13_2,
      lt_invoices TYPE TABLE OF zc_wexopeninvoices,
      lv_tolerance_to_use TYPE curr13_2.
      
    " Extract input parameters
    lv_company_code = input-companycode.
    lv_wex_uuid = input-wexuuid.
    lv_amount = input-amount.
    lv_currency = input-currency.
    lv_tolerance = input-tolerance.
    
    " Get tolerance from config if not provided
    IF lv_tolerance IS INITIAL.
      lv_tolerance_to_use = get_config_tolerance( lv_company_code ).
    ELSE.
      lv_tolerance_to_use = lv_tolerance.
    ENDIF.
    
    " Find matching invoices
    lt_invoices = find_matching_invoices(
      iv_company_code = lv_company_code
      iv_wex_uuid = lv_wex_uuid
    ).
    
    " Check for no candidates
    IF lines( lt_invoices ) = 0.
      RAISE EXCEPTION TYPE cx_rap_business_exception
        EXPORTING
          http_status_code = 404
          business_code = c_error_no_candidate.
    ENDIF.
    
    " Check for ambiguous candidates
    IF lines( lt_invoices ) > 1.
      RAISE EXCEPTION TYPE cx_rap_business_exception
        EXPORTING
          http_status_code = 409
          business_code = c_error_ambiguous.
    ENDIF.
    
    " Validate the single match
    READ TABLE lt_invoices INDEX 1 INTO DATA(ls_invoice).
    validate_match(
      iv_invoice_amount = ls_invoice-invoicegrossamount
      iv_expected_amount = lv_amount
      iv_currency = ls_invoice-documentcurrency
      iv_expected_currency = lv_currency
      iv_tolerance = lv_tolerance_to_use
    ).
    
    " Set result
    result-matchfound = abap_true.
    result-companycode = ls_invoice-companycode.
    result-supplierinvoice = ls_invoice-supplierinvoice.
    result-fiscalyear = ls_invoice-fiscalyear.
    result-invoicegrossamount = ls_invoice-invoicegrossamount.
    result-documentcurrency = ls_invoice-documentcurrency.
    result-message = 'Match found successfully'.

  ENDMETHOD.

  METHOD if_rap_bo_action~releaseifmatch.
    " Implementation of ReleaseIfMatch action
    DATA:
      lv_company_code TYPE bukrs,
      lv_wex_uuid TYPE zhlm_de_wex_uuid,
      lv_amount TYPE curr13_2,
      lv_currency TYPE waers,
      lv_tolerance TYPE curr13_2,
      lt_invoices TYPE TABLE OF zc_wexopeninvoices,
      lv_tolerance_to_use TYPE curr13_2,
      lv_allow_internal_unblock TYPE flag.
      
    " Extract input parameters
    lv_company_code = input-companycode.
    lv_wex_uuid = input-wexuuid.
    lv_amount = input-amount.
    lv_currency = input-currency.
    lv_tolerance = input-tolerance.
    
    " Get tolerance from config if not provided
    IF lv_tolerance IS INITIAL.
      lv_tolerance_to_use = get_config_tolerance( lv_company_code ).
    ELSE.
      lv_tolerance_to_use = lv_tolerance.
    ENDIF.
    
    " Find matching invoices
    lt_invoices = find_matching_invoices(
      iv_company_code = lv_company_code
      iv_wex_uuid = lv_wex_uuid
    ).
    
    " Check for no candidates
    IF lines( lt_invoices ) = 0.
      RAISE EXCEPTION TYPE cx_rap_business_exception
        EXPORTING
          http_status_code = 404
          business_code = c_error_no_candidate.
    ENDIF.
    
    " Check for ambiguous candidates
    IF lines( lt_invoices ) > 1.
      RAISE EXCEPTION TYPE cx_rap_business_exception
        EXPORTING
          http_status_code = 409
          business_code = c_error_ambiguous.
    ENDIF.
    
    " Validate the single match
    READ TABLE lt_invoices INDEX 1 INTO DATA(ls_invoice).
    validate_match(
      iv_invoice_amount = ls_invoice-invoicegrossamount
      iv_expected_amount = lv_amount
      iv_currency = ls_invoice-documentcurrency
      iv_expected_currency = lv_currency
      iv_tolerance = lv_tolerance_to_use
    ).
    
    " Check if internal unblock is allowed
    SELECT SINGLE allow_internal_unblock
      FROM zwex_cfg
      INTO lv_allow_internal_unblock
      WHERE company_code = lv_company_code
        AND active = 'X'.
    
    IF lv_allow_internal_unblock = 'X'.
      " Internal unblock is allowed - implement update logic here
      " This would typically call a released API or update the invoice directly
      result-unblocked = abap_true.
      result-message = 'Invoice unblocked successfully'.
    ELSE.
      " Return keys for CPI to PATCH
      result-unblocked = abap_false.
      result-companycode = ls_invoice-companycode.
      result-supplierinvoice = ls_invoice-supplierinvoice.
      result-fiscalyear = ls_invoice-fiscalyear.
      result-message = 'Use SupplierInvoice PATCH with PaymentBlockingReason='''.
    ENDIF.

  ENDMETHOD.

  METHOD get_config_tolerance.
    " Read tolerance from configuration table
    SELECT SINGLE amount_tolerance
      FROM zwex_cfg
      INTO rv_tolerance
      WHERE company_code = iv_company_code
        AND active = 'X'.
        
    " Default to 0.00 if no configuration found
    IF rv_tolerance IS INITIAL.
      rv_tolerance = '0.00'.
    ENDIF.
  ENDMETHOD.

  METHOD find_matching_invoices.
    " Find invoices matching the WEX UUID
    SELECT *
      FROM zc_wexopeninvoices
      INTO TABLE rt_invoices
      WHERE companycode = iv_company_code
        AND ( reference = iv_wex_uuid OR zz1_thirdpartyref = iv_wex_uuid ).
  ENDMETHOD.

  METHOD validate_match.
    " Validate currency match
    IF iv_currency <> iv_expected_currency.
      RAISE EXCEPTION TYPE cx_rap_business_exception
        EXPORTING
          http_status_code = 422
          business_code = c_error_currency_mismatch.
    ENDIF.
    
    " Validate amount within tolerance
    DATA(lv_difference) = abs( iv_invoice_amount - iv_expected_amount ).
    IF lv_difference > iv_tolerance.
      RAISE EXCEPTION TYPE cx_rap_business_exception
        EXPORTING
          http_status_code = 422
          business_code = c_error_amount_out_of_tolerance.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
