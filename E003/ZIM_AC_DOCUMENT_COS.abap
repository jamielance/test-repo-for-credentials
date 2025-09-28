*&---------------------------------------------------------------------*
*& BAdI Implementation: ZIM_AC_DOCUMENT_COS
*& Description: Cost of Sales Auto Posting - Validation and Outbox Creation
*&---------------------------------------------------------------------*
CLASS zcl_im_ac_document_cos DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_ex_ac_document.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_cos_mapping,
             bukrs        TYPE bukrs,
             trigger_gl   TYPE saknr,
             product_code TYPE char20,
             sales_gl     TYPE saknr,
             cos_gl       TYPE saknr,
             margin_pct   TYPE dec5_2,
           END OF ty_cos_mapping.

    METHODS: check_feature_active
               RETURNING VALUE(rv_active) TYPE abap_bool,
             check_e008_validation
               IMPORTING iv_bukrs TYPE bukrs
                        iv_gjahr TYPE gjahr
                        iv_belnr TYPE belnr_d
               RETURNING VALUE(rv_passed) TYPE abap_bool,
             get_cos_mapping
               IMPORTING iv_bukrs        TYPE bukrs
                        iv_trigger_gl    TYPE saknr
                        iv_product_code  TYPE char20
               RETURNING VALUE(rs_mapping) TYPE ty_cos_mapping,
             create_outbox_entry
               IMPORTING iv_bukrs        TYPE bukrs
                        iv_gjahr         TYPE gjahr
                        iv_belnr         TYPE belnr_d
                        iv_trigger_gl    TYPE saknr
                        iv_product_code  TYPE char20
                        iv_total_charge  TYPE dmbtr
               RETURNING VALUE(rv_guid)  TYPE sysuuid_x16,
             enqueue_qrfc_unit
               IMPORTING iv_guid  TYPE sysuuid_x16
                        iv_bukrs TYPE bukrs
                        iv_gjahr TYPE gjahr
                        iv_belnr TYPE belnr_d.

ENDCLASS.

CLASS zcl_im_ac_document_cos IMPLEMENTATION.

  METHOD if_ex_ac_document~change_document.
    " No changes to document structure
  ENDMETHOD.

  METHOD if_ex_ac_document~check_document.
    " No additional validations required
  ENDMETHOD.

  METHOD if_ex_ac_document~prepare_document.
    " No preparation needed
  ENDMETHOD.

  METHOD if_ex_ac_document~post_document.
    DATA: lv_active        TYPE abap_bool,
          lv_e008_passed   TYPE abap_bool,
          lv_trigger_gl    TYPE saknr,
          lv_product_code  TYPE char20,
          lv_total_charge  TYPE dmbtr,
          lv_guid          TYPE sysuuid_x16,
          ls_mapping       TYPE ty_cos_mapping.

    " Check if feature is active
    lv_active = check_feature_active( ).
    IF lv_active = abap_false.
      RETURN.
    ENDIF.

    " Check if this is a supplier invoice from external system
    " Look for specific characteristics in the document
    LOOP AT c_document-accit INTO DATA(ls_accit).
      " Check if this is a trigger G/L account
      IF ls_accit-hkont IS NOT INITIAL.
        " Get mapping to check if this is a trigger G/L
        SELECT SINGLE * FROM zcos_map INTO @DATA(ls_map)
          WHERE bukrs = @c_document-header-bukrs
            AND trigger_gl = @ls_accit-hkont
            AND valid_from <= @sy-datum
            AND valid_to >= @sy-datum
            AND deleted = @abap_false.
        
        IF sy-subrc = 0.
          lv_trigger_gl = ls_accit-hkont.
          " Extract product code from line item (assuming it's in field assignment)
          lv_product_code = ls_accit-zuonr(20).
          lv_total_charge = lv_total_charge + ls_accit-dmbtr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " If no trigger G/L found, exit
    IF lv_trigger_gl IS INITIAL OR lv_product_code IS INITIAL.
      RETURN.
    ENDIF.

    " Check E008 validation passed
    lv_e008_passed = check_e008_validation(
      iv_bukrs = c_document-header-bukrs
      iv_gjahr = c_document-header-gjahr
      iv_belnr = c_document-header-belnr
    ).
    IF lv_e008_passed = abap_false.
      RETURN.
    ENDIF.

    " Get COS mapping
    ls_mapping = get_cos_mapping(
      iv_bukrs = c_document-header-bukrs
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
    ).
    IF ls_mapping IS INITIAL.
      RETURN.
    ENDIF.

    " Create outbox entry
    lv_guid = create_outbox_entry(
      iv_bukrs = c_document-header-bukrs
      iv_gjahr = c_document-header-gjahr
      iv_belnr = c_document-header-belnr
      iv_trigger_gl = lv_trigger_gl
      iv_product_code = lv_product_code
      iv_total_charge = lv_total_charge
    ).

    " Enqueue qRFC unit
    enqueue_qrfc_unit(
      iv_guid = lv_guid
      iv_bukrs = c_document-header-bukrs
      iv_gjahr = c_document-header-gjahr
      iv_belnr = c_document-header-belnr
    ).

  ENDMETHOD.

  METHOD check_feature_active.
    DATA: lv_value TYPE char1.
    
    SELECT SINGLE low FROM tvarvc INTO lv_value
      WHERE name = 'ZCOS_E003_ACTIVE'.
    
    rv_active = COND #( WHEN lv_value = 'X' THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD check_e008_validation.
    " Check if E008 validation passed
    " This would typically check a custom table or field that E008 sets
    " For now, we'll assume it's stored in a custom field or table
    DATA: lv_e008_status TYPE char1.
    
    " Placeholder - implement based on E008's validation result storage
    SELECT SINGLE e008_status FROM zcos_e008_status INTO lv_e008_status
      WHERE bukrs = @iv_bukrs
        AND gjahr = @iv_gjahr
        AND belnr = @iv_belnr.
    
    rv_passed = COND #( WHEN lv_e008_status = 'P' THEN abap_true ELSE abap_false ).
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
