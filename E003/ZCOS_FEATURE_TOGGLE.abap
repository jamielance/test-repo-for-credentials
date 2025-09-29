*&---------------------------------------------------------------------*
*& Function Module: ZCOS_SETUP_FEATURE_TOGGLE
*& Description: Setup feature toggle for E003 Cost of Sales Auto Posting
*&---------------------------------------------------------------------*
FUNCTION zcos_setup_feature_toggle.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(iv_feature_name) TYPE string DEFAULT 'ZCOS_E003_ACTIVE'
*"     VALUE(iv_is_active) TYPE abap_bool DEFAULT abap_true
*"     VALUE(iv_description) TYPE string DEFAULT 'E003 Cost of Sales Auto Posting - Active Flag'
*"  EXCEPTIONS
*"     INVALID_INPUT
*"     DATABASE_ERROR
*"----------------------------------------------------------------------

  DATA: lo_feature_toggle TYPE REF TO zcl_cos_feature_toggle,
        lv_success        TYPE abap_bool.

  " Create feature toggle instance
  lo_feature_toggle = NEW zcl_cos_feature_toggle( ).

  " Setup feature toggle
  lv_success = lo_feature_toggle->setup_feature(
    iv_feature_name = iv_feature_name
    iv_is_active = iv_is_active
    iv_description = iv_description
  ).

  IF lv_success = abap_false.
    DATA(ls_error_msg) = zcl_cos_message_utility=>get_feature_setup_error( ).
    MESSAGE ls_error_msg-msgty(zcos) WITH ls_error_msg-msgv1.
    RAISE database_error.
  ENDIF.

  DATA(ls_success_msg) = zcl_cos_message_utility=>get_feature_setup_success( ).
  MESSAGE ls_success_msg-msgty(zcos) WITH ls_success_msg-msgv1.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*& Function Module: ZCOS_DEACTIVATE_FEATURE
*& Description: Deactivate E003 Cost of Sales Auto Posting
*&---------------------------------------------------------------------*
FUNCTION zcos_deactivate_feature.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(iv_feature_name) TYPE string DEFAULT 'ZCOS_E003_ACTIVE'
*"  EXCEPTIONS
*"     INVALID_INPUT
*"     DATABASE_ERROR
*"----------------------------------------------------------------------

  DATA: lo_feature_toggle TYPE REF TO zcl_cos_feature_toggle,
        lv_success        TYPE abap_bool.

  " Create feature toggle instance
  lo_feature_toggle = NEW zcl_cos_feature_toggle( ).

  " Deactivate feature
  lv_success = lo_feature_toggle->deactivate_feature(
    iv_feature_name = iv_feature_name
  ).

  IF lv_success = abap_false.
    DATA(ls_error_msg) = zcl_cos_message_utility=>get_feature_deactivate_error( ).
    MESSAGE ls_error_msg-msgty(zcos) WITH ls_error_msg-msgv1.
    RAISE database_error.
  ENDIF.

  DATA(ls_success_msg) = zcl_cos_message_utility=>get_feature_deactivated_success( ).
  MESSAGE ls_success_msg-msgty(zcos) WITH ls_success_msg-msgv1.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*& Function Module: ZCOS_IS_FEATURE_ACTIVE
*& Description: Check if feature is active
*&---------------------------------------------------------------------*
FUNCTION zcos_is_feature_active.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(iv_feature_name) TYPE string
*"  EXPORTING
*"     VALUE(ev_is_active) TYPE abap_bool
*"  EXCEPTIONS
*"     INVALID_INPUT
*"     DATABASE_ERROR
*"----------------------------------------------------------------------

  DATA: lo_feature_toggle TYPE REF TO zcl_cos_feature_toggle.

  " Create feature toggle instance
  lo_feature_toggle = NEW zcl_cos_feature_toggle( ).

  " Check feature status
  ev_is_active = lo_feature_toggle->is_feature_active(
    iv_feature_name = iv_feature_name
  ).

ENDFUNCTION.

*&---------------------------------------------------------------------*
*& Class: ZCL_COS_FEATURE_TOGGLE
*& Description: Feature toggle management class
*&---------------------------------------------------------------------*
CLASS zcl_cos_feature_toggle DEFINITION.
  PUBLIC SECTION.
    METHODS:
      setup_feature
        IMPORTING
          iv_feature_name TYPE string
          iv_is_active TYPE abap_bool
          iv_description TYPE string
        RETURNING
          VALUE(rv_success) TYPE abap_bool,
      
      deactivate_feature
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rv_success) TYPE abap_bool,
      
      is_feature_active
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rv_is_active) TYPE abap_bool.

  PRIVATE SECTION.
    METHODS:
      validate_feature_name
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rv_valid) TYPE abap_bool,
      
      create_tvarvc_entry
        IMPORTING
          iv_feature_name TYPE string
          iv_is_active TYPE abap_bool
          iv_description TYPE string
        RETURNING
          VALUE(rv_success) TYPE abap_bool,
      
      update_tvarvc_entry
        IMPORTING
          iv_feature_name TYPE string
          iv_is_active TYPE abap_bool
        RETURNING
          VALUE(rv_success) TYPE abap_bool,
      
      read_tvarvc_entry
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rs_tvarvc) TYPE tvarvc.

    CONSTANTS:
      gc_feature_type TYPE tvarvc-type VALUE 'P',
      gc_feature_numb TYPE tvarvc-numb VALUE '000000'.

ENDCLASS.

CLASS zcl_cos_feature_toggle IMPLEMENTATION.

  METHOD setup_feature.
    DATA: lv_exists TYPE abap_bool.

    " Validate input
    IF validate_feature_name( iv_feature_name ) = abap_false.
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " Check if entry already exists
    SELECT SINGLE name FROM tvarvc
      INTO @DATA(lv_name)
      WHERE name = @iv_feature_name.
    
    lv_exists = sy-subrc = 0.

    " Create or update entry
    IF lv_exists = abap_true.
      rv_success = update_tvarvc_entry(
        iv_feature_name = iv_feature_name
        iv_is_active = iv_is_active
      ).
    ELSE.
      rv_success = create_tvarvc_entry(
        iv_feature_name = iv_feature_name
        iv_is_active = iv_is_active
        iv_description = iv_description
      ).
    ENDIF.

    " Commit if successful
    IF rv_success = abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

  METHOD deactivate_feature.
    " Validate input
    IF validate_feature_name( iv_feature_name ) = abap_false.
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " Update entry to inactive
    rv_success = update_tvarvc_entry(
      iv_feature_name = iv_feature_name
      iv_is_active = abap_false
    ).

    " Commit if successful
    IF rv_success = abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.

  METHOD is_feature_active.
    DATA: ls_tvarvc TYPE tvarvc.

    " Validate input
    IF validate_feature_name( iv_feature_name ) = abap_false.
      rv_is_active = abap_false.
      RETURN.
    ENDIF.

    " Read feature status
    ls_tvarvc = read_tvarvc_entry( iv_feature_name ).
    
    " Check if feature is active
    rv_is_active = COND #( WHEN ls_tvarvc-low = 'X' THEN abap_true
                          ELSE abap_false ).

  ENDMETHOD.

  METHOD validate_feature_name.
    " Check if feature name is not empty and follows naming convention
    rv_valid = COND #( WHEN iv_feature_name IS NOT INITIAL 
                      AND strlen( iv_feature_name ) <= 30
                      THEN abap_true
                      ELSE abap_false ).
  ENDMETHOD.

  METHOD create_tvarvc_entry.
    DATA: ls_tvarvc TYPE tvarvc.

    " Prepare TVARVC entry
    ls_tvarvc-name = iv_feature_name.
    ls_tvarvc-type = gc_feature_type.
    ls_tvarvc-numb = gc_feature_numb.
    ls_tvarvc-low = COND #( WHEN iv_is_active = abap_true THEN 'X' ELSE '' ).
    ls_tvarvc-text = iv_description.

    " Insert entry
    MODIFY tvarvc FROM ls_tvarvc.
    
    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true
                        ELSE abap_false ).

  ENDMETHOD.

  METHOD update_tvarvc_entry.
    " Update TVARVC entry
    UPDATE tvarvc SET 
      low = @( COND #( WHEN iv_is_active = abap_true THEN 'X' ELSE '' ) )
    WHERE name = @iv_feature_name.
    
    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true
                        ELSE abap_false ).

  ENDMETHOD.

  METHOD read_tvarvc_entry.
    " Read TVARVC entry
    SELECT SINGLE * FROM tvarvc
      INTO @rs_tvarvc
      WHERE name = @iv_feature_name.
  ENDMETHOD.

ENDCLASS.
