INTERFACE zif_bte_1030_payment_processor
  PUBLIC.

  TYPES: BEGIN OF ty_processing_result,
           success TYPE abap_bool,
           message TYPE string,
           excluded_count TYPE i,
         END OF ty_processing_result.

  "! Main processing method for payment runs
  "! @parameter iv_budat | Posting date for the payment run
  "! @parameter iv_nedat | Next execution date
  "! @parameter iv_fdebi | First debit date
  "! @parameter iv_trace | Trace flag for debugging
  "! @parameter cs_reguh | Payment run header data (modified)
  "! @parameter ct_regup | Payment run line items (modified)
  "! @parameter rv_result | Processing result with success status and message
  METHODS process_payment_run
    IMPORTING
      iv_budat TYPE f110c-budat OPTIONAL
      iv_nedat TYPE f110v-nedat OPTIONAL
      iv_fdebi TYPE f110v-fdebi OPTIONAL
      iv_trace TYPE trcopt OPTIONAL
    CHANGING
      cs_reguh TYPE reguh
      ct_regup TYPE regup
    RETURNING
      VALUE(rv_result) TYPE ty_processing_result.

ENDINTERFACE.
