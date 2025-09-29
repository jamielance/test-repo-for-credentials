*&---------------------------------------------------------------------*
*& Class: ZCL_COS_MONITOR
*& Description: Monitor class for COS Auto Posting
*&---------------------------------------------------------------------*
CLASS zcl_cos_monitor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_monitor_data,
             guid              TYPE sysuuid_x16,
             bukrs             TYPE bukrs,
             belnr_src         TYPE belnr_d,
             gjahr             TYPE gjahr,
             belnr_cos         TYPE belnr_d,
             cos_amount        TYPE dmbtr,
             cos_amount_currency TYPE waers,
             status            TYPE char1,
             posted_at         TYPE timestampl,
             posted_by         TYPE syuname,
             error_message     TYPE char255,
             reversal_doc      TYPE belnr_d,
             reversal_gjahr    TYPE gjahr,
           END OF ty_monitor_data.

    TYPES: BEGIN OF ty_monitor_summary,
             total_records    TYPE i,
             total_amount     TYPE dmbtr,
             pending_count    TYPE i,
             error_count      TYPE i,
             complete_count   TYPE i,
             skip_count       TYPE i,
           END OF ty_monitor_summary.

    METHODS:
      constructor
        IMPORTING
          io_logger TYPE REF TO zif_cos_logger OPTIONAL,

      get_monitor_data
        IMPORTING
          it_bukrs_range    TYPE bukrs_range_t OPTIONAL
          it_status_range   TYPE char1_range_t OPTIONAL
          iv_from_date      TYPE dats
          iv_to_date        TYPE dats
          iv_include_outbox TYPE abap_bool DEFAULT abap_true
          iv_include_audit  TYPE abap_bool DEFAULT abap_true
          iv_errors_only    TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rt_data) TYPE TABLE OF ty_monitor_data,

      get_monitor_summary
        IMPORTING
          it_bukrs_range    TYPE bukrs_range_t OPTIONAL
          it_status_range   TYPE char1_range_t OPTIONAL
          iv_from_date      TYPE dats
          iv_to_date        TYPE dats
        RETURNING
          VALUE(rs_summary) TYPE ty_monitor_summary,

      check_authorization
        RETURNING
          VALUE(rv_authorized) TYPE abap_bool.

  PRIVATE SECTION.
    DATA:
      mo_logger TYPE REF TO zif_cos_logger.

    METHODS:
      get_outbox_data
        IMPORTING
          it_bukrs_range  TYPE bukrs_range_t
          it_status_range TYPE char1_range_t
          iv_from_date    TYPE dats
          iv_to_date      TYPE dats
        RETURNING
          VALUE(rt_data) TYPE TABLE OF ty_monitor_data,

      get_audit_data
        IMPORTING
          it_bukrs_range  TYPE bukrs_range_t
          it_status_range TYPE char1_range_t
          iv_from_date    TYPE dats
          iv_to_date      TYPE dats
        RETURNING
          VALUE(rt_data) TYPE TABLE OF ty_monitor_data,

      merge_monitor_data
        IMPORTING
          it_outbox_data TYPE TABLE OF ty_monitor_data
          it_audit_data  TYPE TABLE OF ty_monitor_data
        RETURNING
          VALUE(rt_data) TYPE TABLE OF ty_monitor_data,

      filter_errors_only
        CHANGING
          ct_data TYPE TABLE OF ty_monitor_data,

      calculate_summary
        IMPORTING
          it_data TYPE TABLE OF ty_monitor_data
        RETURNING
          VALUE(rs_summary) TYPE ty_monitor_summary.

ENDCLASS.

CLASS zcl_cos_monitor IMPLEMENTATION.

  METHOD constructor.
    " Initialize logger
    IF io_logger IS BOUND.
      mo_logger = io_logger.
    ELSE.
      mo_logger = NEW zcl_cos_logger( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_monitor_data.
    DATA: lt_outbox_data TYPE TABLE OF ty_monitor_data,
          lt_audit_data  TYPE TABLE OF ty_monitor_data.

    " Get outbox data
    IF iv_include_outbox = abap_true.
      lt_outbox_data = get_outbox_data(
        it_bukrs_range = it_bukrs_range
        it_status_range = it_status_range
        iv_from_date = iv_from_date
        iv_to_date = iv_to_date
      ).
    ENDIF.

    " Get audit data
    IF iv_include_audit = abap_true.
      lt_audit_data = get_audit_data(
        it_bukrs_range = it_bukrs_range
        it_status_range = it_status_range
        iv_from_date = iv_from_date
        iv_to_date = iv_to_date
      ).
    ENDIF.

    " Merge data
    rt_data = merge_monitor_data(
      it_outbox_data = lt_outbox_data
      it_audit_data = lt_audit_data
    ).

    " Filter errors only if requested
    IF iv_errors_only = abap_true.
      filter_errors_only( CHANGING ct_data = rt_data ).
    ENDIF.

  ENDMETHOD.

  METHOD get_monitor_summary.
    DATA: lt_data TYPE TABLE OF ty_monitor_data.

    " Get monitor data
    lt_data = get_monitor_data(
      it_bukrs_range = it_bukrs_range
      it_status_range = it_status_range
      iv_from_date = iv_from_date
      iv_to_date = iv_to_date
    ).

    " Calculate summary
    rs_summary = calculate_summary( lt_data ).

  ENDMETHOD.

  METHOD check_authorization.
    AUTHORITY-CHECK OBJECT 'ZCOS_MONITOR'
      ID 'ACTVT' FIELD '03'
      ID 'APPL' FIELD 'ZCOS'.
    
    rv_authorized = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD get_outbox_data.
    SELECT guid, bukrs, gjahr, belnr_src, trigger_gl, product_code, 
           total_charge, status, error_message, created_at, processed_at
      FROM zcos_outbox INTO TABLE @DATA(lt_outbox)
      WHERE bukrs IN @it_bukrs_range
        AND status IN @it_status_range
        AND created_at >= @cl_abap_tstmp=>tstmp2utc( @cl_abap_tstmp=>create( date = @iv_from_date time = '000000' ) )
        AND created_at <= @cl_abap_tstmp=>tstmp2utc( @cl_abap_tstmp=>create( date = @iv_to_date time = '235959' ) ).

    " Convert to monitor data format
    LOOP AT lt_outbox INTO DATA(ls_outbox).
      APPEND VALUE #(
        guid = ls_outbox-guid
        bukrs = ls_outbox-bukrs
        belnr_src = ls_outbox-belnr_src
        gjahr = ls_outbox-gjahr
        status = ls_outbox-status
        error_message = ls_outbox-error_message
      ) TO rt_data.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_audit_data.
    SELECT guid, bukrs, gjahr, belnr_cos, belnr_src, cos_amount, cos_amount_currency,
           status, posted_at, posted_by, reversal_doc, reversal_gjahr
      FROM zcos_aud INTO TABLE @DATA(lt_audit)
      WHERE bukrs IN @it_bukrs_range
        AND status IN @it_status_range
        AND posted_at >= @cl_abap_tstmp=>tstmp2utc( @cl_abap_tstmp=>create( date = @iv_from_date time = '000000' ) )
        AND posted_at <= @cl_abap_tstmp=>tstmp2utc( @cl_abap_tstmp=>create( date = @iv_to_date time = '235959' ) ).

    " Convert to monitor data format
    LOOP AT lt_audit INTO DATA(ls_audit).
      APPEND VALUE #(
        guid = ls_audit-guid
        bukrs = ls_audit-bukrs
        belnr_cos = ls_audit-belnr_cos
        gjahr = ls_audit-gjahr
        cos_amount = ls_audit-cos_amount
        cos_amount_currency = ls_audit-cos_amount_currency
        status = ls_audit-status
        posted_at = ls_audit-posted_at
        posted_by = ls_audit-posted_by
        reversal_doc = ls_audit-reversal_doc
        reversal_gjahr = ls_audit-reversal_gjahr
      ) TO rt_data.
    ENDLOOP.

  ENDMETHOD.

  METHOD merge_monitor_data.
    DATA: ls_monitor_data TYPE ty_monitor_data.

    " Start with outbox data
    LOOP AT it_outbox_data INTO ls_monitor_data.
      APPEND ls_monitor_data TO rt_data.
    ENDLOOP.

    " Merge audit data
    LOOP AT it_audit_data INTO DATA(ls_audit).
      " Check if entry already exists
      READ TABLE rt_data INTO ls_monitor_data
        WITH KEY guid = ls_audit-guid.
      
      IF sy-subrc = 0.
        " Update existing entry
        ls_monitor_data-belnr_cos = ls_audit-belnr_cos.
        ls_monitor_data-cos_amount = ls_audit-cos_amount.
        ls_monitor_data-posted_at = ls_audit-posted_at.
        ls_monitor_data-posted_by = ls_audit-posted_by.
        ls_monitor_data-reversal_doc = ls_audit-reversal_doc.
        ls_monitor_data-reversal_gjahr = ls_audit-reversal_gjahr.
        MODIFY rt_data FROM ls_monitor_data INDEX sy-tabix.
      ELSE.
        " Create new entry
        APPEND VALUE #(
          guid = ls_audit-guid
          bukrs = ls_audit-bukrs
          belnr_cos = ls_audit-belnr_cos
          gjahr = ls_audit-gjahr
          cos_amount = ls_audit-cos_amount
          status = ls_audit-status
          posted_at = ls_audit-posted_at
          posted_by = ls_audit-posted_by
          reversal_doc = ls_audit-reversal_doc
          reversal_gjahr = ls_audit-reversal_gjahr
        ) TO rt_data.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD filter_errors_only.
    DELETE ct_data WHERE status <> 'E'.
  ENDMETHOD.

  METHOD calculate_summary.
    " Initialize summary
    CLEAR rs_summary.

    " Count records
    DESCRIBE TABLE it_data LINES rs_summary-total_records.

    " Calculate totals
    LOOP AT it_data INTO DATA(ls_data).
      " Add to total amount
      rs_summary-total_amount = rs_summary-total_amount + ls_data-cos_amount.

      " Count by status
      CASE ls_data-status.
        WHEN 'P'.
          rs_summary-pending_count = rs_summary-pending_count + 1.
        WHEN 'E'.
          rs_summary-error_count = rs_summary-error_count + 1.
        WHEN 'C'.
          rs_summary-complete_count = rs_summary-complete_count + 1.
        WHEN 'S'.
          rs_summary-skip_count = rs_summary-skip_count + 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

