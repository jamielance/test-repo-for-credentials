*&---------------------------------------------------------------------*
*& Program: ZCOS_MONITOR
*& Description: Monitoring program for COS Auto Posting
*&---------------------------------------------------------------------*
REPORT zcos_monitor.

TYPES: BEGIN OF ty_monitor_data,
         guid              TYPE sysuuid_x16,
         bukrs             TYPE bukrs,
         belnr_src         TYPE belnr_d,
         gjahr             TYPE gjahr,
         belnr_cos         TYPE belnr_d,
         cos_amount        TYPE dmbtr,
         status            TYPE char1,
         posted_at         TYPE timestampl,
         posted_by         TYPE syuname,
         error_message     TYPE char255,
         reversal_doc      TYPE belnr_d,
         reversal_gjahr    TYPE gjahr,
       END OF ty_monitor_data.

DATA: gt_monitor_data TYPE TABLE OF ty_monitor_data,
      gs_monitor_data TYPE ty_monitor_data,
      gv_bukrs        TYPE bukrs,
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
    MESSAGE e001(zcos) WITH 'From date cannot be greater than to date'.
  ENDIF.

START-OF-SELECTION.
  PERFORM get_monitor_data.
  PERFORM display_results.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Form GET_MONITOR_DATA
*&---------------------------------------------------------------------*
FORM get_monitor_data.
  DATA: lt_outbox TYPE TABLE OF zcos_outbox,
        lt_audit  TYPE TABLE OF zcos_aud.

  " Get outbox data
  IF p_outbox = 'X'.
    SELECT * FROM zcos_outbox INTO TABLE lt_outbox
      WHERE bukrs IN s_bukrs
        AND status IN s_status
        AND created_at >= cl_abap_tstmp=>tstmp2utc( cl_abap_tstmp=>create( date = gv_from_date time = '000000' ) )
        AND created_at <= cl_abap_tstmp=>tstmp2utc( cl_abap_tstmp=>create( date = gv_to_date time = '235959' ) ).
  ENDIF.

  " Get audit data
  IF p_audit = 'X'.
    SELECT * FROM zcos_aud INTO TABLE lt_audit
      WHERE bukrs IN s_bukrs
        AND status IN s_status
        AND posted_at >= cl_abap_tstmp=>tstmp2utc( cl_abap_tstmp=>create( date = gv_from_date time = '000000' ) )
        AND posted_at <= cl_abap_tstmp=>tstmp2utc( cl_abap_tstmp=>create( date = gv_to_date time = '235959' ) ).
  ENDIF.

  " Combine data
  LOOP AT lt_outbox INTO DATA(ls_outbox).
    CLEAR gs_monitor_data.
    gs_monitor_data-guid = ls_outbox-guid.
    gs_monitor_data-bukrs = ls_outbox-bukrs.
    gs_monitor_data-belnr_src = ls_outbox-belnr_src.
    gs_monitor_data-gjahr = ls_outbox-gjahr.
    gs_monitor_data-status = ls_outbox-status.
    gs_monitor_data-error_message = ls_outbox-error_message.
    APPEND gs_monitor_data TO gt_monitor_data.
  ENDLOOP.

  LOOP AT lt_audit INTO DATA(ls_audit).
    " Update existing entry or create new one
    READ TABLE gt_monitor_data INTO gs_monitor_data
      WITH KEY guid = ls_audit-guid.
    IF sy-subrc = 0.
      gs_monitor_data-belnr_cos = ls_audit-belnr_cos.
      gs_monitor_data-cos_amount = ls_audit-cos_amount.
      gs_monitor_data-posted_at = ls_audit-posted_at.
      gs_monitor_data-posted_by = ls_audit-posted_by.
      gs_monitor_data-reversal_doc = ls_audit-reversal_doc.
      gs_monitor_data-reversal_gjahr = ls_audit-reversal_gjahr.
      MODIFY gt_monitor_data FROM gs_monitor_data INDEX sy-tabix.
    ELSE.
      CLEAR gs_monitor_data.
      gs_monitor_data-guid = ls_audit-guid.
      gs_monitor_data-bukrs = ls_audit-bukrs.
      gs_monitor_data-belnr_cos = ls_audit-belnr_cos.
      gs_monitor_data-gjahr = ls_audit-gjahr.
      gs_monitor_data-cos_amount = ls_audit-cos_amount.
      gs_monitor_data-status = ls_audit-status.
      gs_monitor_data-posted_at = ls_audit-posted_at.
      gs_monitor_data-posted_by = ls_audit-posted_by.
      gs_monitor_data-reversal_doc = ls_audit-reversal_doc.
      gs_monitor_data-reversal_gjahr = ls_audit-reversal_gjahr.
      APPEND gs_monitor_data TO gt_monitor_data.
    ENDIF.
  ENDLOOP.

  " Filter errors if requested
  IF p_error = 'X'.
    DELETE gt_monitor_data WHERE status <> 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_RESULTS
*&---------------------------------------------------------------------*
FORM display_results.
  DATA: lv_count TYPE i,
        lv_total_amount TYPE dmbtr.

  " Count records
  DESCRIBE TABLE gt_monitor_data LINES lv_count.

  " Calculate total amount
  LOOP AT gt_monitor_data INTO gs_monitor_data.
    lv_total_amount = lv_total_amount + gs_monitor_data-cos_amount.
  ENDLOOP.

  " Display summary
  WRITE: / 'COS Auto Posting Monitor',
         / 'Records found:', lv_count,
         / 'Total COS amount:', lv_total_amount CURRENCY 'GBP',
         /.

  " Display details
  LOOP AT gt_monitor_data INTO gs_monitor_data.
    WRITE: / gs_monitor_data-guid,
             gs_monitor_data-bukrs,
             gs_monitor_data-belnr_src,
             gs_monitor_data-gjahr,
             gs_monitor_data-belnr_cos,
             gs_monitor_data-cos_amount CURRENCY 'GBP',
             gs_monitor_data-status,
             gs_monitor_data-posted_at,
             gs_monitor_data-posted_by.
    
    IF gs_monitor_data-error_message IS NOT INITIAL.
      WRITE: / 'Error:', gs_monitor_data-error_message.
    ENDIF.
    
    IF gs_monitor_data-reversal_doc IS NOT INITIAL.
      WRITE: / 'Reversed by:', gs_monitor_data-reversal_doc, gs_monitor_data-reversal_gjahr.
    ENDIF.
    
    WRITE: /.
  ENDLOOP.

ENDFORM.
