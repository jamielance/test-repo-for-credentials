*&---------------------------------------------------------------------*
*& Class: ZCL_COS_FEATURE_TOGGLE
*& Description: Feature Toggle Management for COS Auto Posting
*&---------------------------------------------------------------------*
CLASS zcl_cos_feature_toggle DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Feature toggle result structure</p>
    "! <p>Contains the result of feature toggle operations including
    "! success status, error information, and feature details.</p>
    TYPES: BEGIN OF ty_feature_toggle_result,
             "! <p class="shorttext synchronized">Success flag</p>
             "! <p>True if operation was successful</p>
             success        TYPE abap_bool,
             "! <p class="shorttext synchronized">Error code</p>
             "! <p>Error code if operation failed</p>
             error_code     TYPE char4,
             "! <p class="shorttext synchronized">Error message</p>
             "! <p>Error message if operation failed</p>
             error_message  TYPE string,
             "! <p class="shorttext synchronized">Feature name</p>
             "! <p>Name of the feature toggle</p>
             feature_name   TYPE string,
             "! <p class="shorttext synchronized">Is active</p>
             "! <p>Current active status of the feature</p>
             is_active      TYPE abap_bool,
           END OF ty_feature_toggle_result.


    "! <p class="shorttext synchronized">Check if feature is active</p>
    "! <p>Checks if the specified feature toggle is currently active.
    "! Returns true if the feature is enabled, false otherwise.</p>
    "! @parameter iv_feature_name | <p class="shorttext synchronized">Feature name to check</p>
    "! @parameter rv_is_active | <p class="shorttext synchronized">True if feature is active</p>
    "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If check fails</p>
    METHODS:
      is_feature_active
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rv_is_active) TYPE abap_bool
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Setup feature toggle</p>
      "! <p>Creates or updates a feature toggle entry in TVARVC.
      "! This method handles the complete setup process including
      "! validation, creation, and error handling.</p>
      "! @parameter iv_feature_name | <p class="shorttext synchronized">Feature name to setup</p>
      "! @parameter iv_is_active | <p class="shorttext synchronized">Initial active status</p>
      "! @parameter iv_description | <p class="shorttext synchronized">Feature description</p>
      "! @parameter rv_result | <p class="shorttext synchronized">Setup result</p>
      "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If setup fails</p>
      setup_feature_toggle
        IMPORTING
          iv_feature_name TYPE string DEFAULT 'ZCOS_E003_ACTIVE'
          iv_is_active    TYPE abap_bool DEFAULT abap_true
          iv_description  TYPE string DEFAULT 'E003 Cost of Sales Auto Posting - Active Flag'
        RETURNING
          VALUE(rv_result) TYPE ty_feature_toggle_result
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Activate feature</p>
      "! <p>Activates the specified feature toggle by setting its value to 'X'.
      "! If the feature doesn't exist, it will be created.</p>
      "! @parameter iv_feature_name | <p class="shorttext synchronized">Feature name to activate</p>
      "! @parameter rv_success | <p class="shorttext synchronized">True if activation successful</p>
      "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If activation fails</p>
      activate_feature
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rv_success) TYPE abap_bool
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Deactivate feature</p>
      "! <p>Deactivates the specified feature toggle by setting its value to ''.
      "! If the feature doesn't exist, it will be created.</p>
      "! @parameter iv_feature_name | <p class="shorttext synchronized">Feature name to deactivate</p>
      "! @parameter rv_success | <p class="shorttext synchronized">True if deactivation successful</p>
      "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If deactivation fails</p>
      deactivate_feature
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rv_success) TYPE abap_bool
        RAISING
          zcx_cos_processing_error.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Validate feature name</p>
    "! <p>Validates that the feature name is not empty and follows
    "! the required naming conventions for TVARVC entries.</p>
    "! @parameter iv_feature_name | <p class="shorttext synchronized">Feature name to validate</p>
    "! @parameter rv_valid | <p class="shorttext synchronized">True if valid</p>
    METHODS:
      validate_feature_name
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rv_valid) TYPE abap_bool,

      "! <p class="shorttext synchronized">Create TVARVC entry</p>
      "! <p>Creates a new TVARVC entry for the feature toggle.
      "! This method handles the database insert operation.</p>
      "! @parameter iv_feature_name | <p class="shorttext synchronized">Feature name</p>
      "! @parameter iv_is_active | <p class="shorttext synchronized">Active status</p>
      "! @parameter iv_description | <p class="shorttext synchronized">Description</p>
      "! @parameter rv_success | <p class="shorttext synchronized">True if creation successful</p>
      "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If creation fails</p>
      create_tvarvc_entry
        IMPORTING
          iv_feature_name TYPE string
          iv_is_active    TYPE abap_bool
          iv_description  TYPE string
        RETURNING
          VALUE(rv_success) TYPE abap_bool
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Update TVARVC entry</p>
      "! <p>Updates an existing TVARVC entry for the feature toggle.
      "! This method handles the database update operation.</p>
      "! @parameter iv_feature_name | <p class="shorttext synchronized">Feature name</p>
      "! @parameter iv_is_active | <p class="shorttext synchronized">Active status</p>
      "! @parameter rv_success | <p class="shorttext synchronized">True if update successful</p>
      "! @raising zcx_cos_processing_error | <p class="shorttext synchronized">If update fails</p>
      update_tvarvc_entry
        IMPORTING
          iv_feature_name TYPE string
          iv_is_active    TYPE abap_bool
        RETURNING
          VALUE(rv_success) TYPE abap_bool
        RAISING
          zcx_cos_processing_error,

      "! <p class="shorttext synchronized">Read TVARVC entry</p>
      "! <p>Reads an existing TVARVC entry for the feature toggle.
      "! This method handles the database select operation.</p>
      "! @parameter iv_feature_name | <p class="shorttext synchronized">Feature name</p>
      "! @parameter rs_tvarvc | <p class="shorttext synchronized">TVARVC entry data</p>
      read_tvarvc_entry
        IMPORTING
          iv_feature_name TYPE string
        RETURNING
          VALUE(rs_tvarvc) TYPE tvarvc.

ENDCLASS.

CLASS zcl_cos_feature_toggle IMPLEMENTATION.

  METHOD is_feature_active.
    " Check if feature toggle is active
    DATA: ls_tvarvc TYPE tvarvc.

    " Read TVARVC entry
    ls_tvarvc = read_tvarvc_entry( iv_feature_name ).

    " Check if entry exists and is active
    rv_is_active = COND #( 
      WHEN ls_tvarvc-name IS NOT INITIAL AND ls_tvarvc-low = 'X' 
      THEN abap_true 
      ELSE abap_false 
    ).
  ENDMETHOD.

  METHOD setup_feature_toggle.
    " Setup feature toggle entry
    DATA: lv_exists TYPE abap_bool,
          lv_success TYPE abap_bool.

    " Initialize result
    rv_result = VALUE #(
      feature_name = iv_feature_name
      is_active = iv_is_active
    ).

    " Validate input
    IF validate_feature_name( iv_feature_name ) = abap_false.
      rv_result = VALUE #(
        success = abap_false
        error_code = 'INV1'
        error_message = 'Invalid feature name'
        feature_name = iv_feature_name
        is_active = iv_is_active
      ).
      RETURN.
    ENDIF.

    " Check if entry already exists
    " Note: TVARVC has no standard VDM view, using direct table access
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

    " Set result
    rv_result = VALUE #(
      success = rv_success
      feature_name = iv_feature_name
      is_active = iv_is_active
      error_code = COND #( WHEN rv_success = abap_false THEN 'DB01' ELSE '' )
      error_message = COND #( WHEN rv_success = abap_false THEN 'Database operation failed' ELSE '' )
    ).

    " Raise exception if failed
    IF rv_success = abap_false.
      RAISE EXCEPTION TYPE zcx_cos_processing_error
        EXPORTING
          textid = zcx_cos_processing_error=>feature_toggle_setup_failed
          feature_name = iv_feature_name.
    ENDIF.
  ENDMETHOD.

  METHOD activate_feature.
    " Activate feature toggle
    rv_success = setup_feature_toggle(
      iv_feature_name = iv_feature_name
      iv_is_active = abap_true
      iv_description = |Feature toggle for { iv_feature_name }|
    )-success.
  ENDMETHOD.

  METHOD deactivate_feature.
    " Deactivate feature toggle
    rv_success = setup_feature_toggle(
      iv_feature_name = iv_feature_name
      iv_is_active = abap_false
      iv_description = |Feature toggle for { iv_feature_name }|
    )-success.
  ENDMETHOD.

  METHOD validate_feature_name.
    " Validate feature name
    rv_valid = COND #( 
      WHEN iv_feature_name IS INITIAL 
      THEN abap_false
      WHEN strlen( iv_feature_name ) > 30
      THEN abap_false
      ELSE abap_true
    ).
  ENDMETHOD.

  METHOD create_tvarvc_entry.
    " Create TVARVC entry
    DATA: ls_tvarvc TYPE tvarvc.

    " Prepare TVARVC entry
    ls_tvarvc = VALUE #(
      name = iv_feature_name
      type = 'P'
      low = COND #( WHEN iv_is_active = abap_true THEN 'X' ELSE '' )
      high = ''
      text = iv_description
    ).

    " Insert entry
    INSERT tvarvc FROM ls_tvarvc.
    
    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true
                        ELSE abap_false ).

  ENDMETHOD.

  METHOD update_tvarvc_entry.
    " Update TVARVC entry
    DATA: lv_value TYPE tvarvc-low.

    " Set value based on active status
    lv_value = COND #( WHEN iv_is_active = abap_true THEN 'X' ELSE '' ).

    " Update entry
    UPDATE tvarvc 
      SET low = @lv_value
      WHERE name = @iv_feature_name.
    
    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true
                        ELSE abap_false ).

  ENDMETHOD.

  METHOD read_tvarvc_entry.
    " Read TVARVC entry
    " Note: TVARVC has no standard VDM view, using direct table access
    SELECT SINGLE * FROM tvarvc
      INTO @rs_tvarvc
      WHERE name = @iv_feature_name.
  ENDMETHOD.

ENDCLASS.
