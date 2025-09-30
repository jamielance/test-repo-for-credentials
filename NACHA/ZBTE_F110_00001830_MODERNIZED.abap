"! Modernized BTE Function for Payment Processing
"! This function provides a clean wrapper around the modernized payment processor
"! maintaining backward compatibility with the original BTE interface
"! @parameter i_budat | Posting date for the payment run
"! @parameter i_nedat | Next execution date
"! @parameter i_fdebi | First debit date
"! @parameter i_trace | Trace flag for debugging
"! @parameter c_reguh | Payment run header data (modified)
"! @parameter t_regup | Payment run line items (modified)
FUNCTION zbte_f110_00001830_modernized
  IMPORTING
    i_budat LIKE f110c-budat OPTIONAL
    i_nedat LIKE f110v-nedat OPTIONAL
    i_fdebi LIKE f110v-fdebi OPTIONAL
    i_trace LIKE trcopt OPTIONAL
  CHANGING
    c_reguh TYPE reguh
  TABLES
    t_regup LIKE regup.

  DATA: lo_processor TYPE REF TO zcl_bte_1030_payment_processor,
        lv_result    TYPE zif_bte_1030_payment_processor=>ty_processing_result.

  " Create processor instance
  lo_processor = NEW zcl_bte_1030_payment_processor( ).

  " Process payment run using modern class-based approach
  lv_result = lo_processor->zif_bte_1030_payment_processor~process_payment_run(
    iv_budat = i_budat
    iv_nedat = i_nedat
    iv_fdebi = i_fdebi
    iv_trace = i_trace
    CHANGING
      cs_reguh = c_reguh
      ct_regup = t_regup
  ).

  " Log result if trace is enabled
  IF i_trace = 'X' AND lv_result-success = abap_false.
    MESSAGE lv_result-message TYPE 'E'.
  ENDIF.

ENDFUNCTION.
