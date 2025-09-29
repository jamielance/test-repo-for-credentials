*&---------------------------------------------------------------------*
*& Class: ZCL_IM_MRM_CHECK_INV_CLOUD
*& Description: BAdI Implementation for MRM_CHECK_INVOICE_CLOUD
*& Package: ZHLM_AP_INTF
*& Transport: Development Class
*&---------------------------------------------------------------------*
CLASS zcl_im_mrm_check_inv_cloud DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ex_mrm_check_invoice_cloud .

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_bank_rule,
             bukrs     TYPE bukrs,
             inv_cat   TYPE zhlm_de_inv_cat,
             paymeth   TYPE zhlm_de_paymeth,
             hbkid     TYPE zhlm_de_hbkid,
             hktid     TYPE zhlm_de_hktid,
             bvtyp     TYPE zhlm_de_bvtyp,
             paymentid TYPE char5,
           END OF ty_bank_rule.

    TYPES: BEGIN OF ty_wht_rule,
             bukrs   TYPE bukrs,
             inv_cat TYPE zhlm_de_inv_cat,
             whttype TYPE zhlm_de_whttype,
             whtcode TYPE zhlm_de_whtcode,
           END OF ty_wht_rule.

    METHODS:
      get_bank_rule
        IMPORTING
          iv_bukrs   TYPE bukrs
          iv_inv_cat TYPE zhlm_de_inv_cat
          iv_paymeth TYPE zhlm_de_paymeth
          iv_pdate   TYPE dats
        RETURNING
          VALUE(rs_rule) TYPE ty_bank_rule
        RAISING
          cx_mrm_check_invoice_cloud,

      get_wht_rule
        IMPORTING
          iv_bukrs   TYPE bukrs
          iv_inv_cat TYPE zhlm_de_inv_cat
          iv_pdate   TYPE dats
        RETURNING
          VALUE(rs_rule) TYPE ty_wht_rule
        RAISING
          cx_mrm_check_invoice_cloud,

      validate_bank_data
        IMPORTING
          is_rule     TYPE ty_bank_rule
          iv_hbkid    TYPE zhlm_de_hbkid
          iv_hktid    TYPE zhlm_de_hktid
          iv_bvtyp    TYPE zhlm_de_bvtyp
        RAISING
          cx_mrm_check_invoice_cloud,

      validate_wht_data
        IMPORTING
          is_rule     TYPE ty_wht_rule
          iv_whttype  TYPE zhlm_de_whttype
          iv_whtcode  TYPE zhlm_de_whtcode
        RAISING
          cx_mrm_check_invoice_cloud.

ENDCLASS.

CLASS zcl_im_mrm_check_inv_cloud IMPLEMENTATION.

  METHOD if_ex_mrm_check_invoice_cloud~check_invoice_cloud.
    " Validate-only BAdI implementation for MRM_CHECK_INVOICE_CLOUD
    " Re-derives both bank and WHT rules and raises clear messages on mismatch

    DATA: lv_bukrs     TYPE bukrs,
          lv_inv_cat   TYPE zhlm_de_inv_cat,
          lv_paymeth   TYPE zhlm_de_paymeth,
          lv_pdate     TYPE dats,
          lv_hbkid     TYPE zhlm_de_hbkid,
          lv_hktid     TYPE zhlm_de_hktid,
          lv_bvtyp     TYPE zhlm_de_bvtyp,
          lv_whttype   TYPE zhlm_de_whttype,
          lv_whtcode   TYPE zhlm_de_whtcode,
          ls_bank_rule TYPE ty_bank_rule,
          ls_wht_rule  TYPE ty_wht_rule.

    " Extract data from invoice context
    " Note: These field names are placeholders - adjust based on actual MRM structure
    lv_bukrs = is_invoice_context-company_code.
    lv_inv_cat = is_invoice_context-zz1_invoicecategory. " Custom field
    lv_paymeth = is_invoice_context-payment_method.
    lv_pdate = is_invoice_context-posting_date.
    lv_hbkid = is_invoice_context-house_bank.
    lv_hktid = is_invoice_context-house_bank_account.
    lv_bvtyp = is_invoice_context-partner_bank_type.
    lv_whttype = is_invoice_context-withholding_tax_type.
    lv_whtcode = is_invoice_context-withholding_tax_code.

    " Validate bank determination rule
    TRY.
        ls_bank_rule = get_bank_rule(
          iv_bukrs   = lv_bukrs
          iv_inv_cat = lv_inv_cat
          iv_paymeth = lv_paymeth
          iv_pdate   = lv_pdate
        ).

        " Validate bank data against rule
        validate_bank_data(
          is_rule  = ls_bank_rule
          iv_hbkid = lv_hbkid
          iv_hktid = lv_hktid
          iv_bvtyp = lv_bvtyp
        ).

      CATCH cx_mrm_check_invoice_cloud INTO DATA(lx_error).
        " Re-raise the error
        RAISE EXCEPTION lx_error.
    ENDTRY.

    " Validate WHT determination rule
    TRY.
        ls_wht_rule = get_wht_rule(
          iv_bukrs   = lv_bukrs
          iv_inv_cat = lv_inv_cat
          iv_pdate   = lv_pdate
        ).

        " Validate WHT data against rule
        validate_wht_data(
          is_rule    = ls_wht_rule
          iv_whttype = lv_whttype
          iv_whtcode = lv_whtcode
        ).

      CATCH cx_mrm_check_invoice_cloud INTO lx_error.
        " Re-raise the error
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.

  METHOD get_bank_rule.
    " Lookup bank determination rule
    " Concise, performant SELECT SINGLE with proper filtering

    SELECT SINGLE bukrs, inv_cat, paymeth, hbkid, hktid, bvtyp, paymentid
      FROM zinv_cat_bank
      INTO @rs_rule
      WHERE bukrs = @iv_bukrs
        AND inv_cat = @iv_inv_cat
        AND paymeth = @iv_paymeth
        AND isactive = 'X'
        AND @iv_pdate BETWEEN validfrom AND COALESCE(validto, '99991231'.

    IF sy-subrc <> 0.
      " No bank rule found - raise error with clear message
      MESSAGE e001(zhlm_inv_validation) WITH iv_bukrs iv_inv_cat iv_paymeth
        INTO DATA(lv_message).
      RAISE EXCEPTION TYPE cx_mrm_check_invoice_cloud
        EXPORTING
          textid = cx_mrm_check_invoice_cloud=>no_rule_found
          message = lv_message.
    ENDIF.

  ENDMETHOD.

  METHOD get_wht_rule.
    " Lookup WHT determination rule
    " Concise, performant SELECT SINGLE with proper filtering

    SELECT SINGLE bukrs, inv_cat, whttype, whtcode
      FROM zinv_cat_1099
      INTO @rs_rule
      WHERE bukrs = @iv_bukrs
        AND inv_cat = @iv_inv_cat
        AND isactive = 'X'
        AND @iv_pdate BETWEEN validfrom AND COALESCE(validto, '99991231'.

    IF sy-subrc <> 0.
      " No WHT rule found - this is optional, so just return empty
      CLEAR rs_rule.
    ENDIF.

  ENDMETHOD.

  METHOD validate_bank_data.
    " Validate incoming bank data against rule
    " Compare HBKID/HKTID (and BVTYP if populated) with rule

    IF is_rule-hbkid <> iv_hbkid.
      MESSAGE e002(zhlm_inv_validation) WITH iv_hbkid is_rule-hbkid
        INTO DATA(lv_message).
      RAISE EXCEPTION TYPE cx_mrm_check_invoice_cloud
        EXPORTING
          textid = cx_mrm_check_invoice_cloud=>bank_data_mismatch
          message = lv_message.
    ENDIF.

    IF is_rule-hktid <> iv_hktid.
      MESSAGE e003(zhlm_inv_validation) WITH iv_hktid is_rule-hktid
        INTO lv_message.
      RAISE EXCEPTION TYPE cx_mrm_check_invoice_cloud
        EXPORTING
          textid = cx_mrm_check_invoice_cloud=>bank_data_mismatch
          message = lv_message.
    ENDIF.

    " Validate BVTYP only if populated in rule
    IF is_rule-bvtyp IS NOT INITIAL AND is_rule-bvtyp <> iv_bvtyp.
      MESSAGE e004(zhlm_inv_validation) WITH iv_bvtyp is_rule-bvtyp
        INTO lv_message.
      RAISE EXCEPTION TYPE cx_mrm_check_invoice_cloud
        EXPORTING
          textid = cx_mrm_check_invoice_cloud=>bank_data_mismatch
          message = lv_message.
    ENDIF.

  ENDMETHOD.

  METHOD validate_wht_data.
    " Validate WHT data against rule (only if rule exists)
    " Compare assigned Withholding Tax Type/Code from payload/derivation

    IF is_rule IS INITIAL.
      " No WHT rule - validation passes
      RETURN.
    ENDIF.

    IF is_rule-whttype <> iv_whttype.
      MESSAGE e005(zhlm_inv_validation) WITH iv_whttype is_rule-whttype
        INTO DATA(lv_message).
      RAISE EXCEPTION TYPE cx_mrm_check_invoice_cloud
        EXPORTING
          textid = cx_mrm_check_invoice_cloud=>wht_data_mismatch
          message = lv_message.
    ENDIF.

    IF is_rule-whtcode <> iv_whtcode.
      MESSAGE e006(zhlm_inv_validation) WITH iv_whtcode is_rule-whtcode
        INTO lv_message.
      RAISE EXCEPTION TYPE cx_mrm_check_invoice_cloud
        EXPORTING
          textid = cx_mrm_check_invoice_cloud=>wht_data_mismatch
          message = lv_message.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
