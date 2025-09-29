*&---------------------------------------------------------------------*
*& Class: ZCL_COS_DOCUMENT_PROCESSOR
*& Description: Document processor for COS auto posting
*&---------------------------------------------------------------------*
CLASS zcl_cos_document_processor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_cos_mapping,
             bukrs        TYPE bukrs,
             trigger_gl   TYPE saknr,
             product_code TYPE char20,
             sales_gl     TYPE saknr,
             cos_gl       TYPE saknr,
             margin_pct   TYPE dec5_2,
           END OF ty_cos_mapping.

    TYPES: BEGIN OF ty_processing_result,
             success       TYPE abap_bool,
             guid          TYPE sysuuid_x16,
             error_message TYPE string,
           END OF ty_processing_result.

    METHODS:
      constructor
        IMPORTING
          io_logger TYPE REF TO zif_cos_logger OPTIONAL,

      process_document
        IMPORTING
          iv_document TYPE acdoca
        RETURNING
          VALUE(rv_result) TYPE ty_processing_result.

  PRIVATE SECTION.
    DATA:
      mo_logger     TYPE REF TO zif_cos_logger,
      mo_validator  TYPE REF TO zif_cos_validator.

    METHODS:
      check_feature_active
        RETURNING
          VALUE(rv_active) TYPE abap_bool,

      check_e008_validation
        IMPORTING
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_belnr TYPE belnr_d
        RETURNING
          VALUE(rv_passed) TYPE abap_bool,

      find_trigger_gl
        IMPORTING
          it_accit TYPE acdoca_t
        RETURNING
          VALUE(rv_trigger_gl) TYPE saknr,

      extract_product_code
        IMPORTING
          is_accit TYPE acdoca
        RETURNING
          VALUE(rv_product_code) TYPE char20,

      calculate_total_charge
        IMPORTING
          it_accit TYPE acdoca_t
          iv_trigger_gl TYPE saknr
        RETURNING
          VALUE(rv_total_charge) TYPE dmbtr,

      get_cos_mapping
        IMPORTING
          iv_bukrs        TYPE bukrs
          iv_trigger_gl   TYPE saknr
          iv_product_code TYPE char20
        RETURNING
          VALUE(rs_mapping) TYPE ty_cos_mapping,

      create_outbox_entry
        IMPORTING
          iv_bukrs        TYPE bukrs
          iv_gjahr        TYPE gjahr
          iv_belnr        TYPE belnr_d
          iv_trigger_gl   TYPE saknr
          iv_product_code TYPE char20
          iv_total_charge TYPE dmbtr
        RETURNING
          VALUE(rv_guid) TYPE sysuuid_x16,

      enqueue_qrfc_unit
        IMPORTING
          iv_guid  TYPE sysuuid_x16
          iv_bukrs TYPE bukrs
          iv_gjahr TYPE gjahr
          iv_belnr TYPE belnr_d.

ENDCLASS.

CLASS zcl_cos_document_processor IMPLEMENTATION.

  METHOD constructor.
    " Initialize logger
    IF io_logger IS BOUND.
      mo_logger = io_logger.
    ELSE.
      mo_logger = NEW zcl_cos_logger( ).
    ENDIF.

    " Initialize validator
    mo_validator = NEW zcl_cos_validator( ).
  ENDMETHOD.

  METHOD process_document.
    DATA: lv_active        TYPE abap_bool,
          lv_e008_passed   TYPE abap_bool,
          lv_trigger_gl    TYPE saknr,
          lv_product_code  TYPE char20,
          lv_total_charge  TYPE dmbtr,
          lv_guid          TYPE sysuuid_x16,
          ls_mapping       TYPE ty_cos_mapping.

    " Initialize result
    CLEAR rv_result.

    " Check if feature is active
    lv_active = check_feature_active( ).
    IF lv_active = abap_false.
      DATA(ls_feature_msg) = zcl_cos_message_utility=>get_feature_not_active_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_feature_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Find trigger G/L account
    lv_trigger_gl = find_trigger_gl( it_accit = iv_document-accit ).
    IF lv_trigger_gl IS INITIAL.
      DATA(ls_trigger_msg) = zcl_cos_message_utility=>get_no_trigger_gl_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_trigger_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Extract product code from first matching line
    LOOP AT iv_document-accit INTO DATA(ls_accit) WHERE hkont = lv_trigger_gl.
      lv_product_code = extract_product_code( ls_accit ).
      EXIT.
    ENDLOOP.

    IF lv_product_code IS INITIAL.
      DATA(ls_product_msg) = zcl_cos_message_utility=>get_product_code_not_found_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_product_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Calculate total charge
    lv_total_charge = calculate_total_charge(
      it_accit = iv_document-accit
      iv_trigger_gl = lv_trigger_gl
    ).

    " Check E008 validation passed
    lv_e008_passed = check_e008_validation(
      iv_bukrs = iv_document-header-bukrs
      iv_gjahr = iv_document-header-gjahr
      iv_belnr = iv_document-header-belnr
    ).
    IF lv_e008_passed = abap_false.
      DATA(ls_e008_msg) = zcl_cos_message_utility=>get_e008_validation_failed_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_e008_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Get COS mapping
    ls_mapping = get_cos_mapping(
      iv_bukrs = iv_document-header-bukrs
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
    ).
    IF ls_mapping IS INITIAL.
      DATA(ls_mapping_msg) = zcl_cos_message_utility=>get_cos_mapping_not_found_error(
        iv_trigger_gl = lv_trigger_gl
        iv_product_code = lv_product_code
      ).
      rv_result = VALUE #( success = abap_false error_message = ls_mapping_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Create outbox entry
    lv_guid = create_outbox_entry(
      iv_bukrs = iv_document-header-bukrs
      iv_gjahr = iv_document-header-gjahr
      iv_belnr = iv_document-header-belnr
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
      iv_total_charge = lv_total_charge
    ).

    IF lv_guid IS INITIAL.
      DATA(ls_outbox_msg) = zcl_cos_message_utility=>get_outbox_creation_failed_error( ).
      rv_result = VALUE #( success = abap_false error_message = ls_outbox_msg-msgv1 ).
      RETURN.
    ENDIF.

    " Enqueue qRFC unit
    enqueue_qrfc_unit(
      iv_guid = lv_guid
      iv_bukrs = iv_document-header-bukrs
      iv_gjahr = iv_document-header-gjahr
      iv_belnr = iv_document-header-belnr
    ).

    rv_result = VALUE #( success = abap_true guid = lv_guid ).

    " Log success
    mo_logger->log_success(
      iv_message_id = 'ZCOS'
      iv_message_no = '020'
      iv_message_v1 = lv_guid
    ).
    mo_logger->save_log( ).

  ENDMETHOD.

  METHOD check_feature_active.
    DATA: lo_feature_toggle TYPE REF TO zcl_cos_feature_toggle.

    lo_feature_toggle = NEW zcl_cos_feature_toggle( ).
    rv_active = lo_feature_toggle->is_feature_active( 'ZCOS_E003_ACTIVE' ).
  ENDMETHOD.

  METHOD check_e008_validation.
    " Check if E008 validation passed
    " This would typically check a custom table or field that E008 sets
    DATA: lv_e008_status TYPE char1.
    
    " Placeholder - implement based on E008's validation result storage
    SELECT SINGLE e008_status FROM zcos_e008_status INTO @lv_e008_status
      WHERE bukrs = @iv_bukrs
        AND gjahr = @iv_gjahr
        AND belnr = @iv_belnr.
    
    rv_passed = COND #( WHEN lv_e008_status = 'P' THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD find_trigger_gl.
    " Find trigger G/L account by checking mapping table
    LOOP AT it_accit INTO DATA(ls_accit).
      IF ls_accit-hkont IS NOT INITIAL.
        " Check if this is a trigger G/L
        SELECT SINGLE trigger_gl FROM zcos_map
          INTO @rv_trigger_gl
          WHERE trigger_gl = @ls_accit-hkont
            AND valid_from <= @sy-datum
            AND valid_to >= @sy-datum
            AND deleted = @abap_false.
        
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_product_code.
    " Extract product code from line item (assuming it's in field assignment)
    rv_product_code = is_accit-zuonr(20).
  ENDMETHOD.

  METHOD calculate_total_charge.
    " Calculate total charge for trigger G/L lines
    LOOP AT it_accit INTO DATA(ls_accit) WHERE hkont = iv_trigger_gl.
      rv_total_charge = rv_total_charge + ls_accit-dmbtr.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_cos_mapping.
    SELECT SINGLE bukrs, trigger_gl, product_code, sales_gl, cos_gl, margin_pct
      FROM zcos_map
      INTO CORRESPONDING FIELDS OF @rs_mapping
      WHERE bukrs = @iv_bukrs
        AND trigger_gl = @iv_trigger_gl
        AND product_code = @iv_product_code
        AND valid_from <= @sy-datum
        AND valid_to >= @sy-datum
        AND deleted = @abap_false.
  ENDMETHOD.

  METHOD create_outbox_entry.
    DATA: ls_outbox TYPE zcos_outbox.
    
    " Generate GUID
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = rv_guid.
    
    " Fill outbox entry
    ls_outbox-client = sy-mandt.
    ls_outbox-guid = rv_guid.
    ls_outbox-bukrs = iv_bukrs.
    ls_outbox-gjahr = iv_gjahr.
    ls_outbox-belnr_src = iv_belnr.
    ls_outbox-trigger_gl = iv_trigger_gl.
    ls_outbox-product_code = iv_product_code.
    ls_outbox-total_charge = iv_total_charge.
    ls_outbox-created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ).
    ls_outbox-status = 'P'. " Pending
    
    INSERT zcos_outbox FROM ls_outbox.
    COMMIT WORK.
  ENDMETHOD.

  METHOD enqueue_qrfc_unit.
    DATA: lv_queue_name TYPE trfcqnam,
          lv_unit_name  TYPE trfcqout-name.
    
    " Build queue name: COS_<BUKRS>_<YYYYMMDD>
    lv_queue_name = |COS_{ iv_bukrs }_{ sy-datum }|.
    
    " Build unit name
    lv_unit_name = |COS_{ iv_guid }|.
    
    " Enqueue qRFC unit
    CALL FUNCTION 'TRFC_SET_QUEUE_NAME'
      EXPORTING
        qname = lv_queue_name.
    
    CALL FUNCTION 'Z_COS_QRFC_WORKER'
      IN BACKGROUND TASK
      EXPORTING
        iv_guid  = iv_guid
        iv_bukrs = iv_bukrs
        iv_gjahr = iv_gjahr
        iv_belnr = iv_belnr.
  ENDMETHOD.

ENDCLASS.

