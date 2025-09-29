*&---------------------------------------------------------------------*
*& Program: ZCOS_MONITOR
*& Description: Monitoring program for COS Auto Posting
*&---------------------------------------------------------------------*
REPORT zcos_monitor.

DATA: gv_bukrs        TYPE bukrs,
      gv_status       TYPE char1,
      gv_from_date    TYPE dats,
      gv_to_date      TYPE dats.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR gv_bukrs,
                  s_status FOR gv_status.
  SELECTION-SCREEN SKIP.
  SELECT-OPTIONS: s_date FOR gv_from_date.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_outbox AS CHECKBOX DEFAULT 'X',
              p_audit  AS CHECKBOX DEFAULT 'X',
              p_error  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  gv_from_date = sy-datum - 30.
  gv_to_date = sy-datum.

AT SELECTION-SCREEN.
  " Validate date range
  IF gv_from_date > gv_to_date.
    DATA(ls_date_msg) = zcl_cos_message_utility=>get_date_validation_error( ).
    MESSAGE ls_date_msg-msgty(zcos) WITH ls_date_msg-msgv1.
  ENDIF.

START-OF-SELECTION.
  DATA: lo_monitor TYPE REF TO zcl_cos_monitor,
        lt_data    TYPE TABLE OF zcl_cos_monitor=>ty_monitor_data,
        ls_summary TYPE zcl_cos_monitor=>ty_monitor_summary.

  " Create monitor instance
  lo_monitor = NEW zcl_cos_monitor( ).

  " Check authorization
  IF lo_monitor->check_authorization( ) = abap_false.
    DATA(ls_auth_msg) = zcl_cos_message_utility=>get_authorization_error( 'Authorization check failed for monitoring' ).
    MESSAGE ls_auth_msg-msgty(zcos) WITH ls_auth_msg-msgv1.
    RETURN.
  ENDIF.

  " Get monitor data
  lt_data = lo_monitor->get_monitor_data(
    it_bukrs_range = s_bukrs[]
    it_status_range = s_status[]
    iv_from_date = gv_from_date
    iv_to_date = gv_to_date
    iv_include_outbox = p_outbox
    iv_include_audit = p_audit
    iv_errors_only = p_error
  ).

  " Get summary
  ls_summary = lo_monitor->get_monitor_summary(
    it_bukrs_range = s_bukrs[]
    it_status_range = s_status[]
    iv_from_date = gv_from_date
    iv_to_date = gv_to_date
  ).

  " Display results
  PERFORM display_results USING lt_data ls_summary.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Form DISPLAY_RESULTS
*&---------------------------------------------------------------------*
FORM display_results
  USING it_data TYPE TABLE OF zcl_cos_monitor=>ty_monitor_data
        is_summary TYPE zcl_cos_monitor=>ty_monitor_summary.

  DATA: ls_data TYPE zcl_cos_monitor=>ty_monitor_data.

  " Display summary
  WRITE: / 'COS Auto Posting Monitor',
         / 'Records found:', is_summary-total_records,
         / 'Total COS amount:', is_summary-total_amount CURRENCY 'GBP',
         / 'Pending:', is_summary-pending_count,
         / 'Complete:', is_summary-complete_count,
         / 'Errors:', is_summary-error_count,
         / 'Skipped:', is_summary-skip_count,
         /.

  " Display details
  LOOP AT it_data INTO ls_data.
    WRITE: / ls_data-guid,
             ls_data-bukrs,
             ls_data-belnr_src,
             ls_data-gjahr,
             ls_data-belnr_cos,
             ls_data-cos_amount CURRENCY ls_data-cos_amount_currency,
             ls_data-status,
             ls_data-posted_at,
             ls_data-posted_by.
    
    IF ls_data-error_message IS NOT INITIAL.
      WRITE: / 'Error:', ls_data-error_message.
    ENDIF.
    
    IF ls_data-reversal_doc IS NOT INITIAL.
      WRITE: / 'Reversed by:', ls_data-reversal_doc, ls_data-reversal_gjahr.
    ENDIF.
    
    WRITE: /.
  ENDLOOP.

ENDFORM.
