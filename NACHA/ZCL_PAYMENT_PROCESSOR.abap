"! BTE_1030 Payment Processor Class
"! This class handles payment run processing including validation,
"! invoice category filtering, and processing limits for BTE_1030.
CLASS zcl_bte_1030_payment_processor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_bte_1030_payment_processor.

  PRIVATE SECTION.
    "! Internal table for document header data
    DATA: mt_bkpf  TYPE STANDARD TABLE OF bkpf,
          "! Internal table for invoice document data
          mt_rbkp  TYPE STANDARD TABLE OF rbkp,
          "! Internal table for invoice category configuration
          mt_inv_cat TYPE STANDARD TABLE OF zfi_inv_pymt_run.

    "! Validates payment run configuration and data consistency
    "! @parameter iv_laufi | Payment run ID
    "! @parameter it_regup | Payment run line items
    "! @parameter rv_valid | Validation result
    "! @raising zcx_payment_processing_error | When validation fails
    METHODS validate_payment_run
      IMPORTING
        iv_laufi TYPE reguh-laufi
        it_regup TYPE regup
      RETURNING
        VALUE(rv_valid) TYPE abap_bool
      RAISING
        zcx_payment_processing_error.

    "! Retrieves invoice categories for the payment run
    "! @parameter iv_laufi | Payment run ID
    "! @parameter rt_categories | Invoice categories configuration
    "! @raising zcx_payment_processing_error | When no categories found
    METHODS get_invoice_categories
      IMPORTING
        iv_laufi TYPE reguh-laufi
      RETURNING
        VALUE(rt_categories) TYPE STANDARD TABLE OF zfi_inv_pymt_run
      RAISING
        zcx_payment_processing_error.

    "! Validates payment method consistency across all line items
    "! @parameter iv_laufi | Payment run ID
    "! @parameter it_regup | Payment run line items
    "! @parameter rv_valid | Consistency validation result
    "! @raising zcx_payment_processing_error | When validation fails
    METHODS validate_payment_method_consistency
      IMPORTING
        iv_laufi TYPE reguh-laufi
        it_regup TYPE regup
      RETURNING
        VALUE(rv_valid) TYPE abap_bool
      RAISING
        zcx_payment_processing_error.

    "! Retrieves document header data for payment run items
    "! @parameter it_regup | Payment run line items
    "! @parameter rt_bkpf | Document header data
    "! @raising zcx_payment_processing_error | When no document data found
    METHODS get_document_data
      IMPORTING
        it_regup TYPE regup
      RETURNING
        VALUE(rt_bkpf) TYPE STANDARD TABLE OF bkpf
      RAISING
        zcx_payment_processing_error.

    "! Retrieves invoice document data for processing
    "! @parameter it_bkpf | Document header data
    "! @parameter rt_rbkp | Invoice document data
    "! @raising zcx_payment_processing_error | When no invoice data found
    METHODS get_invoice_data
      IMPORTING
        it_bkpf TYPE STANDARD TABLE OF bkpf
      RETURNING
        VALUE(rt_rbkp) TYPE STANDARD TABLE OF rbkp
      RAISING
        zcx_payment_processing_error.

    "! Filters invoice data by valid invoice categories
    "! @parameter ct_rbkp | Invoice document data (modified)
    "! @raising zcx_payment_processing_error | When filtering fails
    METHODS filter_by_invoice_categories
      CHANGING
        ct_rbkp TYPE STANDARD TABLE OF rbkp
      RAISING
        zcx_payment_processing_error.

    "! Applies processing limits based on company configuration
    "! @parameter iv_bukrs | Company code
    "! @parameter iv_payment_method | Payment method
    "! @parameter it_bkpf | Document header data
    "! @parameter ct_regup | Payment run line items (modified)
    "! @raising zcx_payment_processing_error | When limit processing fails
    METHODS apply_processing_limits
      IMPORTING
        iv_bukrs TYPE bukrs
        iv_payment_method TYPE zlsch
        it_bkpf TYPE STANDARD TABLE OF bkpf
      CHANGING
        ct_regup TYPE regup
      RAISING
        zcx_payment_processing_error.

    "! Retrieves processing limit for company and payment method
    "! @parameter iv_bukrs | Company code
    "! @parameter iv_payment_method | Payment method
    "! @parameter rv_limit | Processing limit (default 10000 if not configured)
    "! @raising zcx_payment_processing_error | When limit retrieval fails
    METHODS get_processing_limit
      IMPORTING
        iv_bukrs TYPE bukrs
        iv_payment_method TYPE zlsch
      RETURNING
        VALUE(rv_limit) TYPE i
      RAISING
        zcx_payment_processing_error.

    "! Marks items beyond processing limit for exclusion
    "! @parameter it_bkpf | Document header data
    "! @parameter iv_start_index | Starting index for exclusion
    "! @parameter ct_regup | Payment run line items (modified)
    "! @raising zcx_payment_processing_error | When exclusion processing fails
    METHODS mark_items_for_exclusion
      IMPORTING
        it_bkpf TYPE STANDARD TABLE OF bkpf
        iv_start_index TYPE i
      CHANGING
        ct_regup TYPE regup
      RAISING
        zcx_payment_processing_error.

    "! Processes document exclusions based on invoice categories
    "! @parameter it_bkpf | Document header data
    "! @parameter it_rbkp | Invoice document data
    "! @parameter ct_regup | Payment run line items (modified)
    "! @raising zcx_payment_processing_error | When exclusion processing fails
    METHODS process_document_exclusions
      IMPORTING
        it_bkpf TYPE STANDARD TABLE OF bkpf
        it_rbkp TYPE STANDARD TABLE OF rbkp
      CHANGING
        ct_regup TYPE regup
      RAISING
        zcx_payment_processing_error.

ENDCLASS.

CLASS zcl_bte_1030_payment_processor IMPLEMENTATION.

  METHOD zif_bte_1030_payment_processor~process_payment_run.
    DATA: lv_result TYPE ty_processing_result,
          lo_logger TYPE REF TO zif_cos_logger.

    lo_logger = NEW zcl_cos_logger( iv_object = 'ZPAYMENT' iv_subobject = 'BTE1030' ).

    TRY.
        lo_logger->log_info( 
          iv_message_id = 'ZPAYMENT'
          iv_message_no = '001'
          iv_message_v1 = cs_reguh-laufi
          iv_message_v2 = cs_reguh-zbukr
        ).

        " Validate payment run
        IF validate_payment_run( iv_laufi = cs_reguh-laufi it_regup = ct_regup ) = abap_true.
          " Get invoice categories
          mt_inv_cat = get_invoice_categories( iv_laufi = cs_reguh-laufi ).
          lo_logger->log_info( 
            iv_message_id = 'ZPAYMENT'
            iv_message_no = '003'
            iv_message_v1 = |{ lines( mt_inv_cat ) }|
          ).

          " Get document data
          mt_bkpf = get_document_data( it_regup = ct_regup ).
          lo_logger->log_info( 
            iv_message_id = 'ZPAYMENT'
            iv_message_no = '004'
            iv_message_v1 = |{ lines( mt_bkpf ) }|
          ).

          " Get invoice data
          mt_rbkp = get_invoice_data( it_bkpf = mt_bkpf ).
          lo_logger->log_info( 
            iv_message_id = 'ZPAYMENT'
            iv_message_no = '005'
            iv_message_v1 = |{ lines( mt_rbkp ) }|
          ).

          " Filter by invoice categories
          filter_by_invoice_categories( CHANGING ct_rbkp = mt_rbkp ).
          lo_logger->log_info( 
            iv_message_id = 'ZPAYMENT'
            iv_message_no = '006'
            iv_message_v1 = |{ lines( mt_rbkp ) }|
          ).

          " Process documents and mark exclusions
          process_document_exclusions(
            EXPORTING
              it_bkpf = mt_bkpf
              it_rbkp = mt_rbkp
            CHANGING
              ct_regup = ct_regup
          ).

          " Apply processing limits
          apply_processing_limits(
            EXPORTING
              iv_bukrs = cs_reguh-zbukr
              iv_payment_method = ct_regup[ 1 ]-zlsch
              it_bkpf = mt_bkpf
            CHANGING
              ct_regup = ct_regup
          ).

          " Count excluded items
          lv_result-excluded_count = lines( VALUE #( FOR <regup> IN ct_regup WHERE ( xigno = 'X' ) ( <regup> ) ) ).

          lv_result-success = abap_true.
          lv_result-message = |Processing completed successfully. { lv_result-excluded_count } items excluded.|.

          lo_logger->log_success( 
            iv_message_id = 'ZPAYMENT'
            iv_message_no = '007'
            iv_message_v1 = lv_result-message
            iv_message_v2 = cs_reguh-laufi
          ).
        ELSE.
          lv_result-success = abap_false.
          lv_result-message = 'Payment run validation failed'.
          lo_logger->log_error( 
            iv_message_id = 'ZPAYMENT'
            iv_message_no = '002'
            iv_message_v1 = lv_result-message
            iv_message_v2 = cs_reguh-laufi
          ).
        ENDIF.

      CATCH zcx_payment_processing_error INTO DATA(lx_error).
        lv_result-success = abap_false.
        lv_result-message = lx_error->get_text( ).
        lo_logger->log_error( 
          iv_message_id = 'ZPAYMENT'
          iv_message_no = '008'
          iv_message_v1 = lv_result-message
          iv_message_v2 = cs_reguh-laufi
        ).
    ENDTRY.

    rv_result = lv_result.
  ENDMETHOD.

  METHOD validate_payment_run.
    " Check if payment run exists in invoice category codes
    IF get_invoice_categories( iv_laufi = iv_laufi ) IS NOT INITIAL.
      " Validate payment method consistency
      IF validate_payment_method_consistency( iv_laufi = iv_laufi it_regup = it_regup ) = abap_true.
        rv_valid = abap_true.
      ELSE.
        rv_valid = abap_false.
      ENDIF.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_invoice_categories.
    " Use direct table access for custom table
    SELECT *
      FROM zfi_inv_pymt_run
     WHERE payment_run_id = @iv_laufi
      INTO TABLE @rt_categories.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_payment_processing_error
        EXPORTING
          textid = zcx_payment_processing_error=>no_invoice_categories
          payment_run_id = iv_laufi.
    ENDIF.
  ENDMETHOD.

  METHOD validate_payment_method_consistency.
    DATA: lv_payment_method TYPE zlsch,
          lv_consistent     TYPE abap_bool.

    " Get payment method from first line item
    READ TABLE it_regup INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_regup>).
    IF sy-subrc = 0.
      lv_payment_method = <fs_regup>-zlsch.
      
      " Check if payment method matches run ID
      IF lv_payment_method = iv_laufi+3(1).
        " Check consistency across all line items
        lv_consistent = abap_true.
        LOOP AT it_regup ASSIGNING <fs_regup>.
          IF lv_payment_method NE <fs_regup>-zlsch.
            lv_consistent = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
        rv_valid = lv_consistent.
      ELSE.
        rv_valid = abap_false.
      ENDIF.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD get_document_data.
    " Use standard SAP CDS view for accounting documents
    SELECT *
      FROM i_accountingdocument
       FOR ALL ENTRIES IN @it_regup
     WHERE companycode = @it_regup-bukrs
       AND accountingdocument = @it_regup-belnr
       AND fiscalyear = @it_regup-gjahr
      INTO TABLE @rt_bkpf.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_payment_processing_error
        EXPORTING
          textid = zcx_payment_processing_error=>no_document_data.
    ENDIF.

    SORT rt_bkpf BY bukrs belnr gjahr.
  ENDMETHOD.

  METHOD get_invoice_data.
    " Use standard SAP CDS view for invoice documents
    SELECT *
      FROM i_invoicedocument
       FOR ALL ENTRIES IN @it_bkpf
     WHERE accountingdocument = @it_bkpf-awkey+0(10)
       AND fiscalyear = @it_bkpf-gjahr
     ORDER BY PRIMARY KEY
      INTO TABLE @rt_rbkp.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_payment_processing_error
        EXPORTING
          textid = zcx_payment_processing_error=>no_invoice_data.
    ENDIF.

    SORT rt_rbkp BY belnr gjahr.
  ENDMETHOD.

  METHOD filter_by_invoice_categories.
    DATA: lt_filtered_rbkp TYPE STANDARD TABLE OF rbkp.

    " Build filtered list instead of deleting from original
    LOOP AT ct_rbkp ASSIGNING FIELD-SYMBOL(<fs_rbkp>).
      READ TABLE mt_inv_cat WITH KEY invoice_category = <fs_rbkp>-zz1_invoicecategory_mih TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND <fs_rbkp> TO lt_filtered_rbkp.
      ENDIF.
    ENDLOOP.

    " Replace original table with filtered results
    ct_rbkp = lt_filtered_rbkp.
  ENDMETHOD.

  METHOD apply_processing_limits.
    DATA: lv_max_limit TYPE i,
          lv_lines TYPE i.

    " Get processing limit
    lv_max_limit = get_processing_limit(
      iv_bukrs = iv_bukrs
      iv_payment_method = iv_payment_method
    ).

    lv_lines = lines( it_bkpf ).

    " Only apply limits if we exceed the maximum
    IF lv_lines > lv_max_limit.
      " Mark items beyond limit for exclusion
      mark_items_for_exclusion(
        EXPORTING
          it_bkpf = it_bkpf
          iv_start_index = lv_max_limit
        CHANGING
          ct_regup = ct_regup
      ).
    ENDIF.
  ENDMETHOD.

  METHOD get_processing_limit.
    " Use direct table access for custom table
    DATA: ls_limit TYPE ztb_appaylimit.

    SELECT SINGLE *
      FROM ztb_appaylimit
     WHERE bukrs = @iv_bukrs
       AND zlsch = @iv_payment_method
      INTO @ls_limit.

    IF sy-subrc IS NOT INITIAL.
      " Use default limit if not configured
      rv_limit = 10000.
    ELSE.
      rv_limit = ls_limit-invoice_limit.
    ENDIF.
  ENDMETHOD.

  METHOD mark_items_for_exclusion.
    DATA: lt_bkpf_sorted TYPE STANDARD TABLE OF bkpf.

    " Sort for binary search
    lt_bkpf_sorted = it_bkpf.
    SORT lt_bkpf_sorted BY bukrs belnr gjahr.

    " Mark items from limit onwards as excluded
    LOOP AT lt_bkpf_sorted ASSIGNING FIELD-SYMBOL(<bkpf_cap>) FROM iv_start_index.
      " Note: BKPF doesn't have xigno field, we'll handle this in REGUP directly
    ENDLOOP.

    " Propagate exclusion back to REGUP
    SORT ct_regup BY bukrs belnr gjahr.

    LOOP AT lt_bkpf_sorted ASSIGNING <bkpf_cap> FROM iv_start_index.
      READ TABLE ct_regup ASSIGNING FIELD-SYMBOL(<regup>)
        WITH KEY bukrs = <bkpf_cap>-bukrs
                 belnr = <bkpf_cap>-belnr
                 gjahr = <bkpf_cap>-gjahr
        BINARY SEARCH.
      IF sy-subrc = 0.
        <regup>-xigno = 'X'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_document_exclusions.
    " Process each REGUP item and mark for exclusion based on invoice category
    LOOP AT ct_regup ASSIGNING FIELD-SYMBOL(<regup>).
      READ TABLE it_bkpf ASSIGNING FIELD-SYMBOL(<bkpf>)
           WITH KEY bukrs = <regup>-bukrs
                    belnr = <regup>-belnr
                    gjahr = <regup>-gjahr
           BINARY SEARCH.
      
      IF sy-subrc = 0.
        READ TABLE it_rbkp ASSIGNING FIELD-SYMBOL(<rbkp>)
             WITH KEY belnr = <bkpf>-awkey+0(10)
                      gjahr = <bkpf>-gjahr
             BINARY SEARCH.
        
        IF sy-subrc IS NOT INITIAL.
          " If not in invoice category, mark for exclusion
          <regup>-xigno = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
