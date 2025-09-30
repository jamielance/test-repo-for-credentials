*&---------------------------------------------------------------------*
*& RAP Behavior Implementation: ZBP_C_WEXOPENINVOICES
*& Purpose: Implementation of WEX reconciliation actions
*& Namespace: ZHLM
*& Package: ZHLM_AP_WEX
*&---------------------------------------------------------------------*

CLASS zbp_c_wexopeninvoices DEFINITION
  PUBLIC
  INHERITING FROM cl_rap_bo_generate
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_bo_generate.

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

CLASS zbp_c_wexopeninvoices IMPLEMENTATION.

  METHOD if_rap_bo_generate~get_global_authorizations.
    " Global authorization check - implement as needed
  ENDMETHOD.

  METHOD if_rap_bo_generate~get_instance_authorizations.
    " Instance authorization check - implement as needed
  ENDMETHOD.

  METHOD if_rap_bo_generate~get_instance_features.
    " Instance features - implement as needed
  ENDMETHOD.

  METHOD if_rap_bo_generate~get_global_features.
    " Global features - implement as needed
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
