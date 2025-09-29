*&---------------------------------------------------------------------*
*& Class: ZCL_COS_MESSAGE_UTILITY
*& Description: Utility class for COS message handling
*&---------------------------------------------------------------------*
CLASS zcl_cos_message_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_message_info,
             msgty TYPE char1,
             msgid TYPE msgid,
             msgno TYPE msgno,
             msgv1 TYPE msgv1,
             msgv2 TYPE msgv2,
             msgv3 TYPE msgv3,
             msgv4 TYPE msgv4,
           END OF ty_message_info.

    CLASS-METHODS:
      " Success messages
      get_feature_setup_success
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_feature_deactivated_success
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_cos_document_created_success
        IMPORTING
          iv_document TYPE belnr_d
          iv_year    TYPE gjahr
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_validation_success
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_processing_success
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_monitor_data_success
        IMPORTING
          iv_count TYPE i
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_test_data_setup_success
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_test_data_cleanup_success
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_document_creation_success
        IMPORTING
          iv_document TYPE belnr_d
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_integration_test_success
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      " Error messages
      get_feature_setup_error
        IMPORTING
          iv_error_text TYPE string OPTIONAL
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_feature_deactivate_error
        IMPORTING
          iv_error_text TYPE string OPTIONAL
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_authorization_error
        IMPORTING
          iv_error_text TYPE string OPTIONAL
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_invalid_input_error
        IMPORTING
          iv_error_text TYPE string OPTIONAL
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_processing_error
        IMPORTING
          iv_error_text TYPE string OPTIONAL
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_company_code_required_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_company_code_not_exists_error
        IMPORTING
          iv_bukrs TYPE bukrs
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_fiscal_year_required_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_fiscal_year_invalid_error
        IMPORTING
          iv_gjahr TYPE gjahr
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_document_number_required_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_guid_required_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_guid_invalid_format_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_gl_account_required_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_gl_account_not_exists_error
        IMPORTING
          iv_saknr TYPE saknr
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_amount_below_minimum_error
        IMPORTING
          iv_amount TYPE dmbtr
          iv_minimum TYPE dmbtr
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_amount_exceeds_maximum_error
        IMPORTING
          iv_amount TYPE dmbtr
          iv_maximum TYPE dmbtr
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_feature_not_active_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_no_trigger_gl_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_product_code_not_found_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_e008_validation_failed_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_cos_mapping_not_found_error
        IMPORTING
          iv_trigger_gl TYPE saknr
          iv_product_code TYPE char20
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_outbox_creation_failed_error
        IMPORTING
          iv_error_text TYPE string OPTIONAL
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_already_processed_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_amount_below_tolerance_warning
        IMPORTING
          iv_amount TYPE dmbtr
          iv_tolerance TYPE dmbtr
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_outbox_not_found_error
        IMPORTING
          iv_guid TYPE sysuuid_x16
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_mapping_not_found_error
        IMPORTING
          iv_trigger_gl TYPE saknr
          iv_product_code TYPE char20
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_document_creation_failed_error
        IMPORTING
          iv_error_text TYPE string OPTIONAL
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_database_operation_failed_error
        IMPORTING
          iv_error_text TYPE string OPTIONAL
        RETURNING
          VALUE(rs_message) TYPE ty_message_info,

      get_date_validation_error
        RETURNING
          VALUE(rs_message) TYPE ty_message_info.

  PRIVATE SECTION.
    CONSTANTS:
      c_msgid TYPE msgid VALUE 'ZCOS'.

ENDCLASS.

CLASS zcl_cos_message_utility IMPLEMENTATION.

  " Success Messages
  METHOD get_feature_setup_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '001'
    ).
  ENDMETHOD.

  METHOD get_feature_deactivated_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '002'
    ).
  ENDMETHOD.

  METHOD get_cos_document_created_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '003'
      msgv1 = iv_document
      msgv2 = |{ iv_year }|
    ).
  ENDMETHOD.

  METHOD get_validation_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '038'
    ).
  ENDMETHOD.

  METHOD get_processing_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '039'
    ).
  ENDMETHOD.

  METHOD get_monitor_data_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '036'
      msgv1 = |{ iv_count }|
    ).
  ENDMETHOD.

  METHOD get_test_data_setup_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '034'
    ).
  ENDMETHOD.

  METHOD get_test_data_cleanup_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '035'
    ).
  ENDMETHOD.

  METHOD get_document_creation_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '033'
      msgv1 = iv_document
    ).
  ENDMETHOD.

  METHOD get_integration_test_success.
    rs_message = VALUE #(
      msgty = 'S'
      msgid = c_msgid
      msgno = '040'
    ).
  ENDMETHOD.

  " Error Messages
  METHOD get_feature_setup_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '004'
      msgv1 = COND #( WHEN iv_error_text IS NOT INITIAL THEN iv_error_text ELSE 'Unknown error' )
    ).
  ENDMETHOD.

  METHOD get_feature_deactivate_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '005'
      msgv1 = COND #( WHEN iv_error_text IS NOT INITIAL THEN iv_error_text ELSE 'Unknown error' )
    ).
  ENDMETHOD.

  METHOD get_authorization_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '006'
      msgv1 = COND #( WHEN iv_error_text IS NOT INITIAL THEN iv_error_text ELSE 'Authorization check failed' )
    ).
  ENDMETHOD.

  METHOD get_invalid_input_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '007'
      msgv1 = COND #( WHEN iv_error_text IS NOT INITIAL THEN iv_error_text ELSE 'Invalid input parameters' )
    ).
  ENDMETHOD.

  METHOD get_processing_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '008'
      msgv1 = COND #( WHEN iv_error_text IS NOT INITIAL THEN iv_error_text ELSE 'Processing error' )
    ).
  ENDMETHOD.

  METHOD get_company_code_required_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '009'
    ).
  ENDMETHOD.

  METHOD get_company_code_not_exists_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '010'
      msgv1 = iv_bukrs
    ).
  ENDMETHOD.

  METHOD get_fiscal_year_required_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '011'
    ).
  ENDMETHOD.

  METHOD get_fiscal_year_invalid_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '012'
      msgv1 = |{ iv_gjahr }|
    ).
  ENDMETHOD.

  METHOD get_document_number_required_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '013'
    ).
  ENDMETHOD.

  METHOD get_guid_required_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '014'
    ).
  ENDMETHOD.

  METHOD get_guid_invalid_format_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '015'
    ).
  ENDMETHOD.

  METHOD get_gl_account_required_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '016'
    ).
  ENDMETHOD.

  METHOD get_gl_account_not_exists_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '017'
      msgv1 = iv_saknr
    ).
  ENDMETHOD.

  METHOD get_amount_below_minimum_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '018'
      msgv1 = |{ iv_amount }|
      msgv2 = |{ iv_minimum }|
    ).
  ENDMETHOD.

  METHOD get_amount_exceeds_maximum_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '019'
      msgv1 = |{ iv_amount }|
      msgv2 = |{ iv_maximum }|
    ).
  ENDMETHOD.

  METHOD get_feature_not_active_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '020'
    ).
  ENDMETHOD.

  METHOD get_no_trigger_gl_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '021'
    ).
  ENDMETHOD.

  METHOD get_product_code_not_found_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '022'
    ).
  ENDMETHOD.

  METHOD get_e008_validation_failed_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '023'
    ).
  ENDMETHOD.

  METHOD get_cos_mapping_not_found_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '024'
      msgv1 = iv_trigger_gl
      msgv2 = iv_product_code
    ).
  ENDMETHOD.

  METHOD get_outbox_creation_failed_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '025'
      msgv1 = COND #( WHEN iv_error_text IS NOT INITIAL THEN iv_error_text ELSE 'Unknown error' )
    ).
  ENDMETHOD.

  METHOD get_already_processed_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '026'
    ).
  ENDMETHOD.

  METHOD get_amount_below_tolerance_warning.
    rs_message = VALUE #(
      msgty = 'W'
      msgid = c_msgid
      msgno = '027'
      msgv1 = |{ iv_amount }|
      msgv2 = |{ iv_tolerance }|
    ).
  ENDMETHOD.

  METHOD get_outbox_not_found_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '028'
      msgv1 = iv_guid
    ).
  ENDMETHOD.

  METHOD get_mapping_not_found_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '029'
      msgv1 = iv_trigger_gl
      msgv2 = iv_product_code
    ).
  ENDMETHOD.

  METHOD get_document_creation_failed_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '030'
      msgv1 = COND #( WHEN iv_error_text IS NOT INITIAL THEN iv_error_text ELSE 'Unknown error' )
    ).
  ENDMETHOD.

  METHOD get_database_operation_failed_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '031'
      msgv1 = COND #( WHEN iv_error_text IS NOT INITIAL THEN iv_error_text ELSE 'Unknown error' )
    ).
  ENDMETHOD.

  METHOD get_date_validation_error.
    rs_message = VALUE #(
      msgty = 'E'
      msgid = c_msgid
      msgno = '032'
    ).
  ENDMETHOD.

ENDCLASS.
