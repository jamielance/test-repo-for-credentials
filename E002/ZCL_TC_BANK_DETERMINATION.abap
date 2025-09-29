*&---------------------------------------------------------------------*
*& Test Class: ZCL_TC_BANK_DETERMINATION
*& Description: ABAP Unit tests for Bank Determination Rules
*& Package: ZHLM_AP_INTF
*& Transport: Development Class
*&---------------------------------------------------------------------*
CLASS zcl_tc_bank_determination DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA: f_cut TYPE REF TO zcl_im_mrm_check_inv_cloud.

    METHODS:
      setup,
      teardown,
      " Happy path tests
      test_bank_rule_found FOR TESTING,
      test_wht_rule_found FOR TESTING,
      " Error path tests
      test_no_bank_rule FOR TESTING,
      test_bank_data_mismatch FOR TESTING,
      test_wht_data_mismatch FOR TESTING,
      " Negative tests
      test_ambiguous_config_prevention FOR TESTING.

    METHODS:
      " Helper methods
      insert_test_bank_rule
        IMPORTING
          iv_bukrs   TYPE bukrs
          iv_inv_cat TYPE zhlm_de_inv_cat
          iv_paymeth TYPE zhlm_de_paymeth
          iv_hbkid   TYPE zhlm_de_hbkid
          iv_hktid   TYPE zhlm_de_hktid
          iv_bvtyp   TYPE zhlm_de_bvtyp,

      insert_test_wht_rule
        IMPORTING
          iv_bukrs   TYPE bukrs
          iv_inv_cat TYPE zhlm_de_inv_cat
          iv_whttype TYPE zhlm_de_whttype
          iv_whtcode TYPE zhlm_de_whtcode,

      cleanup_test_data.

ENDCLASS.

CLASS zcl_tc_bank_determination IMPLEMENTATION.

  METHOD setup.
    " Setup test environment
    f_cut = NEW zcl_im_mrm_check_inv_cloud( ).
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD teardown.
    " Cleanup test data
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD test_bank_rule_found.
    " Happy path: Bank rule exists and data matches

    " Given: Test bank rule exists
    insert_test_bank_rule(
      iv_bukrs   = '1000'
      iv_inv_cat = '001'
      iv_paymeth = 'A'
      iv_hbkid   = 'HBK01'
      iv_hktid   = 'HKT01'
      iv_bvtyp   = 'BV01'
    ).

    " When: Validating invoice with matching data
    DATA(ls_invoice_context) = VALUE mrm_check_invoice_cloud_context(
      company_code = '1000'
      zz1_invoicecategory = '001'
      payment_method = 'A'
      posting_date = sy-datum
      house_bank = 'HBK01'
      house_bank_account = 'HKT01'
      partner_bank_type = 'BV01'
    ).

    " Then: No exception should be raised
    TRY.
        f_cut->if_ex_mrm_check_invoice_cloud~check_invoice_cloud( ls_invoice_context ).
        cl_abap_unit_assert=>assert_equals(
          exp = 0
          act = 0
          msg = 'Bank rule validation should pass'
        ).
      CATCH cx_mrm_check_invoice_cloud.
        cl_abap_unit_assert=>fail( 'Unexpected exception raised' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_wht_rule_found.
    " Happy path: WHT rule exists and data matches

    " Given: Test WHT rule exists
    insert_test_wht_rule(
      iv_bukrs   = '1000'
      iv_inv_cat = '001'
      iv_whttype = 'WT'
      iv_whtcode = 'WTC1'
    ).

    " When: Validating invoice with matching WHT data
    DATA(ls_invoice_context) = VALUE mrm_check_invoice_cloud_context(
      company_code = '1000'
      zz1_invoicecategory = '001'
      posting_date = sy-datum
      withholding_tax_type = 'WT'
      withholding_tax_code = 'WTC1'
    ).

    " Then: No exception should be raised
    TRY.
        f_cut->if_ex_mrm_check_invoice_cloud~check_invoice_cloud( ls_invoice_context ).
        cl_abap_unit_assert=>assert_equals(
          exp = 0
          act = 0
          msg = 'WHT rule validation should pass'
        ).
      CATCH cx_mrm_check_invoice_cloud.
        cl_abap_unit_assert=>fail( 'Unexpected exception raised' ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_no_bank_rule.
    " Error path: No bank rule found

    " Given: No bank rule exists
    " When: Validating invoice
    DATA(ls_invoice_context) = VALUE mrm_check_invoice_cloud_context(
      company_code = '1000'
      zz1_invoicecategory = '999'
      payment_method = 'A'
      posting_date = sy-datum
    ).

    " Then: Exception should be raised
    TRY.
        f_cut->if_ex_mrm_check_invoice_cloud~check_invoice_cloud( ls_invoice_context ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH cx_mrm_check_invoice_cloud INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_bound(
          act = lx_error
          msg = 'Exception should be raised for missing bank rule'
        ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_bank_data_mismatch.
    " Error path: Bank data doesn't match rule

    " Given: Bank rule exists
    insert_test_bank_rule(
      iv_bukrs   = '1000'
      iv_inv_cat = '001'
      iv_paymeth = 'A'
      iv_hbkid   = 'HBK01'
      iv_hktid   = 'HKT01'
      iv_bvtyp   = 'BV01'
    ).

    " When: Validating invoice with mismatched bank data
    DATA(ls_invoice_context) = VALUE mrm_check_invoice_cloud_context(
      company_code = '1000'
      zz1_invoicecategory = '001'
      payment_method = 'A'
      posting_date = sy-datum
      house_bank = 'HBK02'  " Different from rule
      house_bank_account = 'HKT01'
      partner_bank_type = 'BV01'
    ).

    " Then: Exception should be raised
    TRY.
        f_cut->if_ex_mrm_check_invoice_cloud~check_invoice_cloud( ls_invoice_context ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH cx_mrm_check_invoice_cloud INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_bound(
          act = lx_error
          msg = 'Exception should be raised for bank data mismatch'
        ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_wht_data_mismatch.
    " Error path: WHT data doesn't match rule

    " Given: WHT rule exists
    insert_test_wht_rule(
      iv_bukrs   = '1000'
      iv_inv_cat = '001'
      iv_whttype = 'WT'
      iv_whtcode = 'WTC1'
    ).

    " When: Validating invoice with mismatched WHT data
    DATA(ls_invoice_context) = VALUE mrm_check_invoice_cloud_context(
      company_code = '1000'
      zz1_invoicecategory = '001'
      posting_date = sy-datum
      withholding_tax_type = 'WT'
      withholding_tax_code = 'WTC2'  " Different from rule
    ).

    " Then: Exception should be raised
    TRY.
        f_cut->if_ex_mrm_check_invoice_cloud~check_invoice_cloud( ls_invoice_context ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH cx_mrm_check_invoice_cloud INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_bound(
          act = lx_error
          msg = 'Exception should be raised for WHT data mismatch'
        ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_ambiguous_config_prevention.
    " Negative test: Ambiguous config prevented by unique index

    " Given: First bank rule exists
    insert_test_bank_rule(
      iv_bukrs   = '1000'
      iv_inv_cat = '001'
      iv_paymeth = 'A'
      iv_hbkid   = 'HBK01'
      iv_hktid   = 'HKT01'
      iv_bvtyp   = 'BV01'
    ).

    " When: Attempting to insert duplicate rule
    " Then: Should fail due to unique index constraint
    TRY.
        INSERT zinv_cat_bank FROM VALUE zinv_cat_bank(
          bukrs = '1000'
          inv_cat = '001'
          paymeth = 'A'
          validfrom = sy-datum
          validto = '99991231'
          isactive = 'X'
          hbkid = 'HBK02'
          hktid = 'HKT02'
          bvtyp = 'BV02'
          last_changed_at = cl_abap_tstmp=>utclong2tstmp( cl_abap_tstmp=>utclong( ) )
        ).
        cl_abap_unit_assert=>fail( 'Duplicate insert should fail' ).
      CATCH cx_sy_open_sql_db.
        " Expected - unique constraint violation
        cl_abap_unit_assert=>assert_equals(
          exp = 1
          act = 1
          msg = 'Unique constraint should prevent duplicate records'
        ).
    ENDTRY.

  ENDMETHOD.

  METHOD insert_test_bank_rule.
    " Helper method to insert test bank rule

    INSERT zinv_cat_bank FROM VALUE zinv_cat_bank(
      bukrs = iv_bukrs
      inv_cat = iv_inv_cat
      paymeth = iv_paymeth
      validfrom = sy-datum
      validto = '99991231'
      isactive = 'X'
      hbkid = iv_hbkid
      hktid = iv_hktid
      bvtyp = iv_bvtyp
      last_changed_at = cl_abap_tstmp=>utclong2tstmp( cl_abap_tstmp=>utclong( ) )
    ).

  ENDMETHOD.

  METHOD insert_test_wht_rule.
    " Helper method to insert test WHT rule

    INSERT zinv_cat_1099 FROM VALUE zinv_cat_1099(
      bukrs = iv_bukrs
      inv_cat = iv_inv_cat
      validfrom = sy-datum
      validto = '99991231'
      isactive = 'X'
      whttype = iv_whttype
      whtcode = iv_whtcode
      last_changed_at = cl_abap_tstmp=>utclong2tstmp( cl_abap_tstmp=>utclong( ) )
    ).

  ENDMETHOD.

  METHOD cleanup_test_data.
    " Helper method to cleanup test data

    DELETE FROM zinv_cat_bank WHERE bukrs = '1000'.
    DELETE FROM zinv_cat_1099 WHERE bukrs = '1000'.

  ENDMETHOD.

ENDCLASS.
