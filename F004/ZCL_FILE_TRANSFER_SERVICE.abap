*&---------------------------------------------------------------------*
*& Class: ZCL_FILE_TRANSFER_SERVICE
*& Description: File Transfer Service for SFTP Operations
*& Purpose: Secure file operations with OData integration
*&---------------------------------------------------------------------*
CLASS zcl_file_transfer_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">File transfer result structure</p>
    TYPES: BEGIN OF ty_file_transfer_result,
             success        TYPE abap_bool,
             message        TYPE string,
             source_file    TYPE string,
             target_file    TYPE string,
             operation_time TYPE timestampl,
             error_code     TYPE sy-subrc,
           END OF ty_file_transfer_result.

    "! <p class="shorttext synchronized">File transfer request structure</p>
    TYPES: BEGIN OF ty_file_transfer_request,
             source_file      TYPE string,
             target_directory TYPE string,
             operation_type   TYPE char10, " MOVE, COPY, DELETE
             create_backup    TYPE abap_bool,
             overwrite_existing TYPE abap_bool,
           END OF ty_file_transfer_request.

    "! <p class="shorttext synchronized">Move file to output directory</p>
    "! <p>Securely moves a file from source to target directory with validation</p>
    "! @parameter iv_input_file | <p class="shorttext synchronized">Source file path</p>
    "! @parameter iv_output_directory | <p class="shorttext synchronized">Target directory path</p>
    "! @parameter iv_operation_type | <p class="shorttext synchronized">Operation type (MOVE/COPY/DELETE)</p>
    "! @parameter rv_result | <p class="shorttext synchronized">Operation result</p>
    METHODS: move_files_to_output_directory
      IMPORTING
        iv_input_file        TYPE string
        iv_output_directory  TYPE string
        iv_operation_type    TYPE char10 DEFAULT 'MOVE'
        iv_create_backup     TYPE abap_bool DEFAULT abap_false
        iv_overwrite_existing TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result)     TYPE ty_file_transfer_result,

      "! <p class="shorttext synchronized">Process file transfer request</p>
      "! <p>Processes a complete file transfer request with validation and logging</p>
      "! @parameter is_request | <p class="shorttext synchronized">File transfer request</p>
      "! @parameter rv_result | <p class="shorttext synchronized">Operation result</p>
      process_file_transfer
        IMPORTING
          is_request      TYPE ty_file_transfer_request
        RETURNING
          VALUE(rv_result) TYPE ty_file_transfer_result,

      "! <p class="shorttext synchronized">Validate file paths</p>
      "! <p>Validates source and target paths for security and existence</p>
      "! @parameter iv_source_path | <p class="shorttext synchronized">Source file path</p>
      "! @parameter iv_target_path | <p class="shorttext synchronized">Target directory path</p>
      "! @parameter rv_valid | <p class="shorttext synchronized">Validation result</p>
      validate_file_paths
        IMPORTING
          iv_source_path TYPE string
          iv_target_path TYPE string
        RETURNING
          VALUE(rv_valid) TYPE abap_bool.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Logger instance</p>
    DATA: mo_logger TYPE REF TO zif_cos_logger.

    "! <p class="shorttext synchronized">Constants</p>
    CONSTANTS:
      mc_move_unix_files    TYPE sxpgcolist-commandname VALUE 'MOVE_FILE',
      mc_copy_unix_files    TYPE sxpgcolist-commandname VALUE 'COPY_FILE',
      mc_delete_unix_files  TYPE sxpgcolist-commandname VALUE 'DELETE_FILE',
      mc_max_path_length    TYPE i VALUE 255,
      mc_operation_move     TYPE char10 VALUE 'MOVE',
      mc_operation_copy     TYPE char10 VALUE 'COPY',
      mc_operation_delete   TYPE char10 VALUE 'DELETE'.

    "! <p class="shorttext synchronized">Sanitize file path</p>
    "! <p>Removes dangerous characters and validates path security</p>
    METHODS: sanitize_file_path
      IMPORTING
        iv_path      TYPE string
      RETURNING
        VALUE(rv_clean_path) TYPE string,

      "! <p class="shorttext synchronized">Check file exists</p>
      "! <p>Checks if file exists on the system</p>
      "! @parameter iv_file_path | <p class="shorttext synchronized">File path to check</p>
      "! @parameter rv_exists | <p class="shorttext synchronized">File exists flag</p>
      check_file_exists
        IMPORTING
          iv_file_path TYPE string
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      "! <p class="shorttext synchronized">Check directory exists</p>
      "! <p>Checks if directory exists on the system</p>
      "! @parameter iv_directory | <p class="shorttext synchronized">Directory path to check</p>
      "! @parameter rv_exists | <p class="shorttext synchronized">Directory exists flag</p>
      check_directory_exists
        IMPORTING
          iv_directory TYPE string
        RETURNING
          VALUE(rv_exists) TYPE abap_bool,

      "! <p class="shorttext synchronized">Create directory</p>
      "! <p>Creates directory if it doesn't exist</p>
      "! @parameter iv_directory | <p class="shorttext synchronized">Directory path to create</p>
      "! @parameter rv_success | <p class="shorttext synchronized">Creation success flag</p>
      create_directory
        IMPORTING
          iv_directory TYPE string
        RETURNING
          VALUE(rv_success) TYPE abap_bool,

      "! <p class="shorttext synchronized">Execute system command</p>
      "! <p>Executes system command with proper error handling</p>
      "! @parameter iv_command | <p class="shorttext synchronized">Command name</p>
      "! @parameter iv_parameters | <p class="shorttext synchronized">Command parameters</p>
      "! @parameter rv_success | <p class="shorttext synchronized">Execution success flag</p>
      execute_system_command
        IMPORTING
          iv_command    TYPE sxpgcolist-commandname
          iv_parameters TYPE string
        RETURNING
          VALUE(rv_success) TYPE abap_bool,

      "! <p class="shorttext synchronized">Log file operation</p>
      "! <p>Logs file operation for audit trail</p>
      "! @parameter is_request | <p class="shorttext synchronized">File transfer request</p>
      "! @parameter is_result | <p class="shorttext synchronized">Operation result</p>
      log_file_operation
        IMPORTING
          is_request TYPE ty_file_transfer_request
          is_result  TYPE ty_file_transfer_result.

ENDCLASS.

CLASS zcl_file_transfer_service IMPLEMENTATION.

  METHOD move_files_to_output_directory.
    " Initialize result
    rv_result = VALUE #(
      source_file    = iv_input_file
      target_file    = iv_output_directory
      operation_time = utclong_current( )
      success        = abap_false
    ).

    " Initialize logger
    mo_logger = NEW zcl_cos_logger( iv_object = 'ZFILE' iv_subobject = 'TRANSFER' ).

    " Input validation
    IF iv_input_file IS INITIAL OR iv_output_directory IS INITIAL.
      rv_result-message = 'Input file or output directory is empty'.
      rv_result-error_code = 1.
      mo_logger->log_error(
        iv_message_id = 'ZFILE'
        iv_message_no = '001'
        iv_message_v1 = rv_result-message
      ).
      RETURN.
    ENDIF.

    " Validate file paths
    IF validate_file_paths( iv_source_path = iv_input_file iv_target_path = iv_output_directory ) = abap_false.
      rv_result-message = 'Invalid file paths detected'.
      rv_result-error_code = 2.
      mo_logger->log_error(
        iv_message_id = 'ZFILE'
        iv_message_no = '002'
        iv_message_v1 = rv_result-message
      ).
      RETURN.
    ENDIF.

    " Sanitize paths
    DATA(lv_clean_source) = sanitize_file_path( iv_input_file ).
    DATA(lv_clean_target) = sanitize_file_path( iv_output_directory ).

    " Check source file exists
    IF check_file_exists( lv_clean_source ) = abap_false.
      rv_result-message = |Source file does not exist: { lv_clean_source }|.
      rv_result-error_code = 3.
      mo_logger->log_error(
        iv_message_id = 'ZFILE'
        iv_message_no = '003'
        iv_message_v1 = lv_clean_source
      ).
      RETURN.
    ENDIF.

    " Ensure target directory exists
    IF check_directory_exists( lv_clean_target ) = abap_false.
      IF create_directory( lv_clean_target ) = abap_false.
        rv_result-message = |Failed to create target directory: { lv_clean_target }|.
        rv_result-error_code = 4.
        mo_logger->log_error(
          iv_message_id = 'ZFILE'
          iv_message_no = '004'
          iv_message_v1 = lv_clean_target
        ).
        RETURN.
      ENDIF.
    ENDIF.

    " Build command parameters
    DATA(lv_param) = |{ lv_clean_source } { lv_clean_target }|.

    " Log operation start
    mo_logger->log_info(
      iv_message_id = 'ZFILE'
      iv_message_no = '005'
      iv_message_v1 = lv_clean_source
      iv_message_v2 = lv_clean_target
      iv_message_v3 = iv_operation_type
    ).

    " Execute command based on operation type
    DATA(lv_command) = COND #(
      WHEN iv_operation_type = mc_operation_move THEN mc_move_unix_files
      WHEN iv_operation_type = mc_operation_copy THEN mc_copy_unix_files
      WHEN iv_operation_type = mc_operation_delete THEN mc_delete_unix_files
      ELSE mc_move_unix_files
    ).

    " Execute system command
    IF execute_system_command( iv_command = lv_command iv_parameters = lv_param ) = abap_true.
      rv_result-success = abap_true.
      rv_result-message = |File { iv_operation_type } operation completed successfully|.
      rv_result-error_code = 0.
      
      mo_logger->log_success(
        iv_message_id = 'ZFILE'
        iv_message_no = '006'
        iv_message_v1 = lv_clean_source
        iv_message_v2 = lv_clean_target
        iv_message_v3 = iv_operation_type
      ).
    ELSE.
      rv_result-message = |File { iv_operation_type } operation failed|.
      rv_result-error_code = 5.
      
      mo_logger->log_error(
        iv_message_id = 'ZFILE'
        iv_message_no = '007'
        iv_message_v1 = lv_clean_source
        iv_message_v2 = lv_clean_target
        iv_message_v3 = iv_operation_type
      ).
    ENDIF.

    " Save log
    mo_logger->save_log( ).

  ENDMETHOD.

  METHOD process_file_transfer.
    " Initialize result
    rv_result = VALUE #(
      source_file    = is_request-source_file
      target_file    = is_request-target_directory
      operation_time = utclong_current( )
      success        = abap_false
    ).

    " Process the file transfer request
    rv_result = move_files_to_output_directory(
      iv_input_file        = is_request-source_file
      iv_output_directory  = is_request-target_directory
      iv_operation_type    = is_request-operation_type
      iv_create_backup     = is_request-create_backup
      iv_overwrite_existing = is_request-overwrite_existing
    ).

    " Log the operation
    log_file_operation(
      is_request = is_request
      is_result  = rv_result
    ).

  ENDMETHOD.

  METHOD validate_file_paths.
    " Check if paths are not empty
    IF iv_source_path IS INITIAL OR iv_target_path IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check path length
    IF strlen( iv_source_path ) > mc_max_path_length OR strlen( iv_target_path ) > mc_max_path_length.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check for dangerous characters
    IF iv_source_path CS ';' OR iv_source_path CS '|' OR iv_source_path CS '&' OR
       iv_source_path CS '`' OR iv_source_path CS '$' OR iv_source_path CS '(' OR
       iv_source_path CS ')' OR iv_target_path CS ';' OR iv_target_path CS '|' OR
       iv_target_path CS '&' OR iv_target_path CS '`' OR iv_target_path CS '$' OR
       iv_target_path CS '(' OR iv_target_path CS ')'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check for path traversal
    IF iv_source_path CS '../' OR iv_source_path CS '..\' OR
       iv_target_path CS '../' OR iv_target_path CS '..\'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD sanitize_file_path.
    rv_clean_path = iv_path.
    
    " Remove dangerous characters
    REPLACE ALL OCCURRENCES OF ';' IN rv_clean_path WITH ''.
    REPLACE ALL OCCURRENCES OF '|' IN rv_clean_path WITH ''.
    REPLACE ALL OCCURRENCES OF '&' IN rv_clean_path WITH ''.
    REPLACE ALL OCCURRENCES OF '`' IN rv_clean_path WITH ''.
    REPLACE ALL OCCURRENCES OF '$' IN rv_clean_path WITH ''.
    REPLACE ALL OCCURRENCES OF '(' IN rv_clean_path WITH ''.
    REPLACE ALL OCCURRENCES OF ')' IN rv_clean_path WITH ''.
    
    " Prevent path traversal
    IF rv_clean_path CS '../' OR rv_clean_path CS '..\'.
      CLEAR rv_clean_path.
    ENDIF.
    
  ENDMETHOD.

  METHOD check_file_exists.
    " Use OPEN DATASET to check file existence
    OPEN DATASET iv_file_path FOR INPUT IN BINARY MODE.
    IF sy-subrc = 0.
      CLOSE DATASET iv_file_path.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD check_directory_exists.
    " Check if directory exists by trying to list it
    DATA: lt_files TYPE TABLE OF string.
    
    CALL METHOD cl_gui_frontend_services=>directory_list_files
      EXPORTING
        directory                   = iv_directory
      CHANGING
        file_table                  = lt_files
      EXCEPTIONS
        directory_list_files_failed = 1
        OTHERS                      = 2.
        
    rv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD create_directory.
    CALL METHOD cl_gui_frontend_services=>directory_create
      EXPORTING
        directory                = iv_directory
      EXCEPTIONS
        directory_create_failed  = 1
        OTHERS                   = 2.
        
    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD execute_system_command.
    DATA: lt_btcxpm TYPE TABLE OF btcxpm.

    CALL FUNCTION 'SXPG_CALL_SYSTEM'
      EXPORTING
        commandname                = iv_command
        additional_parameters      = iv_parameters
      TABLES
        exec_protocol              = lt_btcxpm
      EXCEPTIONS
        no_permission              = 1
        command_not_found          = 2
        parameters_too_long        = 3
        security_risk              = 4
        wrong_check_call_interface = 5
        program_start_error        = 6
        program_termination_error  = 7
        x_error                    = 8
        parameter_expected         = 9
        too_many_parameters        = 10
        illegal_command            = 11
        OTHERS                     = 12.

    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    " Log specific error details
    IF sy-subrc <> 0.
      mo_logger->log_error(
        iv_message_id = 'ZFILE'
        iv_message_no = '008'
        iv_message_v1 = |SY-SUBRC: { sy-subrc }|
        iv_message_v2 = iv_command
        iv_message_v3 = iv_parameters
      ).
    ENDIF.

  ENDMETHOD.

  METHOD log_file_operation.
    " Log file operation for audit trail
    mo_logger->log_info(
      iv_message_id = 'ZFILE'
      iv_message_no = '009'
      iv_message_v1 = is_request-source_file
      iv_message_v2 = is_request-target_directory
      iv_message_v3 = is_request-operation_type
      iv_message_v4 = COND #( WHEN is_result-success = abap_true THEN 'SUCCESS' ELSE 'FAILED' )
    ).
  ENDMETHOD.

ENDCLASS.
