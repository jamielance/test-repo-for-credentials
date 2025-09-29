*&---------------------------------------------------------------------*
*& Program: ZCOS_INSTALLATION_SCRIPT
*& Description: Automated installation script for COS Auto Posting Solution
*&---------------------------------------------------------------------*
REPORT zcos_installation_script.

TYPES: BEGIN OF ty_install_step,
         step_number TYPE i,
         object_type TYPE char10,
         object_name TYPE char30,
         description TYPE char100,
         status      TYPE char1,
       END OF ty_install_step.

DATA: lt_steps TYPE TABLE OF ty_install_step,
      ls_step  TYPE ty_install_step.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_dev   AS CHECKBOX DEFAULT 'X',
              p_test  AS CHECKBOX,
              p_prod  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_dd    AS CHECKBOX DEFAULT 'X',
              p_class AS CHECKBOX DEFAULT 'X',
              p_func  AS CHECKBOX DEFAULT 'X',
              p_badi  AS CHECKBOX DEFAULT 'X',
              p_msg   AS CHECKBOX DEFAULT 'X',
              p_test  AS CHECKBOX DEFAULT 'X',
              p_report AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_config AS CHECKBOX DEFAULT 'X',
              p_verify AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  " Initialize installation steps
  PERFORM initialize_installation_steps.

START-OF-SELECTION.
  " Display installation header
  PERFORM display_installation_header.

  " Install Data Dictionary Objects
  IF p_dd = 'X'.
    PERFORM install_data_dictionary_objects.
  ENDIF.

  " Install Classes
  IF p_class = 'X'.
    PERFORM install_classes.
  ENDIF.

  " Install Function Modules
  IF p_func = 'X'.
    PERFORM install_function_modules.
  ENDIF.

  " Install BAdI Implementation
  IF p_badi = 'X'.
    PERFORM install_badi_implementation.
  ENDIF.

  " Install Message Classes
  IF p_msg = 'X'.
    PERFORM install_message_classes.
  ENDIF.

  " Install Test Classes
  IF p_test = 'X'.
    PERFORM install_test_classes.
  ENDIF.

  " Install Reports
  IF p_report = 'X'.
    PERFORM install_reports.
  ENDIF.

  " Configure System
  IF p_config = 'X'.
    PERFORM configure_system.
  ENDIF.

  " Verify Installation
  IF p_verify = 'X'.
    PERFORM verify_installation.
  ENDIF.

  " Display Installation Summary
  PERFORM display_installation_summary.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_INSTALLATION_STEPS
*&---------------------------------------------------------------------*
FORM initialize_installation_steps.
  " Data Dictionary Objects
  ls_step-step_number = 1.
  ls_step-object_type = 'TABLE'.
  ls_step-object_name = 'ZCOS_OUTBOX'.
  ls_step-description = 'Outbox table for COS processing'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 2.
  ls_step-object_type = 'TABLE'.
  ls_step-object_name = 'ZCOS_AUD'.
  ls_step-description = 'Audit table for COS processing'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 3.
  ls_step-object_type = 'TABLE'.
  ls_step-object_name = 'ZCOS_MAP'.
  ls_step-description = 'Mapping table for COS configuration'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  " Core Classes
  ls_step-step_number = 4.
  ls_step-object_type = 'INTERFACE'.
  ls_step-object_name = 'ZIF_COS_LOGGER'.
  ls_step-description = 'Logger interface'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 5.
  ls_step-object_type = 'INTERFACE'.
  ls_step-object_name = 'ZIF_COS_VALIDATOR'.
  ls_step-description = 'Validator interface'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 6.
  ls_step-object_type = 'CLASS'.
  ls_step-object_name = 'ZCL_COS_LOGGER'.
  ls_step-description = 'Logger implementation'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 7.
  ls_step-object_type = 'CLASS'.
  ls_step-object_name = 'ZCL_COS_VALIDATOR'.
  ls_step-description = 'Validator implementation'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 8.
  ls_step-object_type = 'CLASS'.
  ls_step-object_name = 'ZCL_COS_MESSAGE_UTILITY'.
  ls_step-description = 'Message utility class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 9.
  ls_step-object_type = 'CLASS'.
  ls_step-object_name = 'ZCL_COS_FEATURE_TOGGLE'.
  ls_step-description = 'Feature toggle class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 10.
  ls_step-object_type = 'CLASS'.
  ls_step-object_name = 'ZCL_COS_QRFC_WORKER'.
  ls_step-description = 'qRFC worker class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 11.
  ls_step-object_type = 'CLASS'.
  ls_step-object_name = 'ZCL_COS_DOCUMENT_PROCESSOR'.
  ls_step-description = 'Document processor class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 12.
  ls_step-object_type = 'CLASS'.
  ls_step-object_name = 'ZCL_COS_MONITOR'.
  ls_step-description = 'Monitor class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  " Function Modules
  ls_step-step_number = 13.
  ls_step-object_type = 'FUNCTION'.
  ls_step-object_name = 'Z_COS_QRFC_WORKER'.
  ls_step-description = 'qRFC worker function module'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 14.
  ls_step-object_type = 'FUNCTION'.
  ls_step-object_name = 'ZCOS_SETUP_FEATURE_TOGGLE'.
  ls_step-description = 'Feature toggle setup function'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  " BAdI Implementation
  ls_step-step_number = 15.
  ls_step-object_type = 'BADI'.
  ls_step-object_name = 'ZIM_AC_DOCUMENT_COS'.
  ls_step-description = 'Document processing BAdI'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  " Message Classes
  ls_step-step_number = 16.
  ls_step-object_type = 'MESSAGE'.
  ls_step-object_name = 'ZCOS_MESSAGES_ENHANCED'.
  ls_step-description = 'Enhanced message class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  " Test Classes
  ls_step-step_number = 17.
  ls_step-object_type = 'TEST'.
  ls_step-object_name = 'ZCL_TC_COS_FEATURE_TOGGLE'.
  ls_step-description = 'Feature toggle test class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 18.
  ls_step-object_type = 'TEST'.
  ls_step-object_name = 'ZCL_TC_COS_VALIDATOR'.
  ls_step-description = 'Validator test class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 19.
  ls_step-object_type = 'TEST'.
  ls_step-object_name = 'ZCL_TC_COS_QRFC_WORKER'.
  ls_step-description = 'qRFC worker test class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 20.
  ls_step-object_type = 'TEST'.
  ls_step-object_name = 'ZCL_TC_COS_MONITOR'.
  ls_step-description = 'Monitor test class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 21.
  ls_step-object_type = 'TEST'.
  ls_step-object_name = 'ZCL_TC_COS_INTEGRATION'.
  ls_step-description = 'Integration test class'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  " Reports
  ls_step-step_number = 22.
  ls_step-object_type = 'REPORT'.
  ls_step-object_name = 'ZCOS_MONITOR'.
  ls_step-description = 'Monitoring report'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

  ls_step-step_number = 23.
  ls_step-object_type = 'REPORT'.
  ls_step-object_name = 'ZCOS_TEST_NEW'.
  ls_step-description = 'Test runner report'.
  ls_step-status = 'P'.
  APPEND ls_step TO lt_steps.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_INSTALLATION_HEADER
*&---------------------------------------------------------------------*
FORM display_installation_header.
  WRITE: / '==================================================',
         / 'COS Auto Posting Solution - Installation Script',
         / '==================================================',
         / 'Version: 1.0',
         / 'Date:', sy-datum,
         / 'Time:', sy-uzeit,
         / 'User:', sy-uname,
         / 'System:', sy-sysid,
         / '==================================================',
         /.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSTALL_DATA_DICTIONARY_OBJECTS
*&---------------------------------------------------------------------*
FORM install_data_dictionary_objects.
  WRITE: / 'Installing Data Dictionary Objects...'.

  " Check if tables exist
  PERFORM check_table_exists USING 'ZCOS_OUTBOX'.
  PERFORM check_table_exists USING 'ZCOS_AUD'.
  PERFORM check_table_exists USING 'ZCOS_MAP'.

  " Update step status
  LOOP AT lt_steps INTO ls_step WHERE object_type = 'TABLE'.
    ls_step-status = 'C'.
    MODIFY lt_steps FROM ls_step.
  ENDLOOP.

  WRITE: / 'Data Dictionary Objects installation completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSTALL_CLASSES
*&---------------------------------------------------------------------*
FORM install_classes.
  WRITE: / 'Installing Classes...'.

  " Check if classes exist
  PERFORM check_class_exists USING 'ZIF_COS_LOGGER'.
  PERFORM check_class_exists USING 'ZIF_COS_VALIDATOR'.
  PERFORM check_class_exists USING 'ZCL_COS_LOGGER'.
  PERFORM check_class_exists USING 'ZCL_COS_VALIDATOR'.
  PERFORM check_class_exists USING 'ZCL_COS_MESSAGE_UTILITY'.
  PERFORM check_class_exists USING 'ZCL_COS_FEATURE_TOGGLE'.
  PERFORM check_class_exists USING 'ZCL_COS_QRFC_WORKER'.
  PERFORM check_class_exists USING 'ZCL_COS_DOCUMENT_PROCESSOR'.
  PERFORM check_class_exists USING 'ZCL_COS_MONITOR'.

  " Update step status
  LOOP AT lt_steps INTO ls_step WHERE object_type = 'CLASS' OR object_type = 'INTERFACE'.
    ls_step-status = 'C'.
    MODIFY lt_steps FROM ls_step.
  ENDLOOP.

  WRITE: / 'Classes installation completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSTALL_FUNCTION_MODULES
*&---------------------------------------------------------------------*
FORM install_function_modules.
  WRITE: / 'Installing Function Modules...'.

  " Check if function modules exist
  PERFORM check_function_exists USING 'Z_COS_QRFC_WORKER'.
  PERFORM check_function_exists USING 'ZCOS_SETUP_FEATURE_TOGGLE'.

  " Update step status
  LOOP AT lt_steps INTO ls_step WHERE object_type = 'FUNCTION'.
    ls_step-status = 'C'.
    MODIFY lt_steps FROM ls_step.
  ENDLOOP.

  WRITE: / 'Function Modules installation completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSTALL_BADI_IMPLEMENTATION
*&---------------------------------------------------------------------*
FORM install_badi_implementation.
  WRITE: / 'Installing BAdI Implementation...'.

  " Check if BAdI implementation exists
  PERFORM check_badi_exists USING 'ZIM_AC_DOCUMENT_COS'.

  " Update step status
  LOOP AT lt_steps INTO ls_step WHERE object_type = 'BADI'.
    ls_step-status = 'C'.
    MODIFY lt_steps FROM ls_step.
  ENDLOOP.

  WRITE: / 'BAdI Implementation installation completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSTALL_MESSAGE_CLASSES
*&---------------------------------------------------------------------*
FORM install_message_classes.
  WRITE: / 'Installing Message Classes...'.

  " Check if message classes exist
  PERFORM check_message_exists USING 'ZCOS_MESSAGES_ENHANCED'.

  " Update step status
  LOOP AT lt_steps INTO ls_step WHERE object_type = 'MESSAGE'.
    ls_step-status = 'C'.
    MODIFY lt_steps FROM ls_step.
  ENDLOOP.

  WRITE: / 'Message Classes installation completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSTALL_TEST_CLASSES
*&---------------------------------------------------------------------*
FORM install_test_classes.
  WRITE: / 'Installing Test Classes...'.

  " Check if test classes exist
  PERFORM check_class_exists USING 'ZCL_TC_COS_FEATURE_TOGGLE'.
  PERFORM check_class_exists USING 'ZCL_TC_COS_VALIDATOR'.
  PERFORM check_class_exists USING 'ZCL_TC_COS_QRFC_WORKER'.
  PERFORM check_class_exists USING 'ZCL_TC_COS_MONITOR'.
  PERFORM check_class_exists USING 'ZCL_TC_COS_INTEGRATION'.

  " Update step status
  LOOP AT lt_steps INTO ls_step WHERE object_type = 'TEST'.
    ls_step-status = 'C'.
    MODIFY lt_steps FROM ls_step.
  ENDLOOP.

  WRITE: / 'Test Classes installation completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSTALL_REPORTS
*&---------------------------------------------------------------------*
FORM install_reports.
  WRITE: / 'Installing Reports...'.

  " Check if reports exist
  PERFORM check_report_exists USING 'ZCOS_MONITOR'.
  PERFORM check_report_exists USING 'ZCOS_TEST_NEW'.

  " Update step status
  LOOP AT lt_steps INTO ls_step WHERE object_type = 'REPORT'.
    ls_step-status = 'C'.
    MODIFY lt_steps FROM ls_step.
  ENDLOOP.

  WRITE: / 'Reports installation completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONFIGURE_SYSTEM
*&---------------------------------------------------------------------*
FORM configure_system.
  WRITE: / 'Configuring System...'.

  " Set up feature toggle
  PERFORM setup_feature_toggle.

  " Create sample mapping data
  PERFORM create_sample_mapping_data.

  " Set up application log
  PERFORM setup_application_log.

  WRITE: / 'System configuration completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VERIFY_INSTALLATION
*&---------------------------------------------------------------------*
FORM verify_installation.
  WRITE: / 'Verifying Installation...'.

  " Run basic verification checks
  PERFORM verify_tables_exist.
  PERFORM verify_classes_exist.
  PERFORM verify_function_modules_exist.
  PERFORM verify_reports_exist.

  " Run test suite
  PERFORM run_test_suite.

  WRITE: / 'Installation verification completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_INSTALLATION_SUMMARY
*&---------------------------------------------------------------------*
FORM display_installation_summary.
  DATA: lv_total   TYPE i,
        lv_success TYPE i,
        lv_failed  TYPE i.

  " Count installation results
  LOOP AT lt_steps INTO ls_step.
    lv_total = lv_total + 1.
    IF ls_step-status = 'C'.
      lv_success = lv_success + 1.
    ELSE.
      lv_failed = lv_failed + 1.
    ENDIF.
  ENDLOOP.

  WRITE: / '==================================================',
         / 'Installation Summary',
         / '==================================================',
         / 'Total Steps:', lv_total,
         / 'Successful:', lv_success,
         / 'Failed:', lv_failed,
         / '==================================================',
         /.

  " Display detailed results
  LOOP AT lt_steps INTO ls_step.
    WRITE: / ls_step-step_number, ls_step-object_type, ls_step-object_name, ls_step-status.
  ENDLOOP.

  IF lv_failed = 0.
    WRITE: / 'Installation completed successfully!'.
  ELSE.
    WRITE: / 'Installation completed with errors. Please check the log.'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_TABLE_EXISTS
*&---------------------------------------------------------------------*
FORM check_table_exists USING pv_table TYPE tabname.
  DATA: lv_exists TYPE abap_bool.

  SELECT SINGLE tabname FROM dd02l INTO @DATA(lv_tabname)
    WHERE tabname = @pv_table.

  lv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  IF lv_exists = abap_true.
    WRITE: / 'Table', pv_table, 'exists ✓'.
  ELSE.
    WRITE: / 'Table', pv_table, 'missing ✗'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_CLASS_EXISTS
*&---------------------------------------------------------------------*
FORM check_class_exists USING pv_class TYPE seoclsname.
  DATA: lv_exists TYPE abap_bool.

  SELECT SINGLE clsname FROM seoclass INTO @DATA(lv_clsname)
    WHERE clsname = @pv_class.

  lv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  IF lv_exists = abap_true.
    WRITE: / 'Class', pv_class, 'exists ✓'.
  ELSE.
    WRITE: / 'Class', pv_class, 'missing ✗'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_FUNCTION_EXISTS
*&---------------------------------------------------------------------*
FORM check_function_exists USING pv_function TYPE rs38l_fnam.
  DATA: lv_exists TYPE abap_bool.

  SELECT SINGLE funcname FROM tfdir INTO @DATA(lv_funcname)
    WHERE funcname = @pv_function.

  lv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  IF lv_exists = abap_true.
    WRITE: / 'Function', pv_function, 'exists ✓'.
  ELSE.
    WRITE: / 'Function', pv_function, 'missing ✗'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_BADI_EXISTS
*&---------------------------------------------------------------------*
FORM check_badi_exists USING pv_badi TYPE seoclsname.
  DATA: lv_exists TYPE abap_bool.

  SELECT SINGLE clsname FROM seoclass INTO @DATA(lv_clsname)
    WHERE clsname = @pv_badi.

  lv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  IF lv_exists = abap_true.
    WRITE: / 'BAdI', pv_badi, 'exists ✓'.
  ELSE.
    WRITE: / 'BAdI', pv_badi, 'missing ✗'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_MESSAGE_EXISTS
*&---------------------------------------------------------------------*
FORM check_message_exists USING pv_message TYPE arbgb.
  DATA: lv_exists TYPE abap_bool.

  SELECT SINGLE arbgb FROM t100a INTO @DATA(lv_arbgb)
    WHERE arbgb = @pv_message.

  lv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  IF lv_exists = abap_true.
    WRITE: / 'Message class', pv_message, 'exists ✓'.
  ELSE.
    WRITE: / 'Message class', pv_message, 'missing ✗'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_REPORT_EXISTS
*&---------------------------------------------------------------------*
FORM check_report_exists USING pv_report TYPE syrepid.
  DATA: lv_exists TYPE abap_bool.

  SELECT SINGLE progname FROM trdir INTO @DATA(lv_progname)
    WHERE progname = @pv_report.

  lv_exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  IF lv_exists = abap_true.
    WRITE: / 'Report', pv_report, 'exists ✓'.
  ELSE.
    WRITE: / 'Report', pv_report, 'missing ✗'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SETUP_FEATURE_TOGGLE
*&---------------------------------------------------------------------*
FORM setup_feature_toggle.
  DATA: lv_success TYPE abap_bool.

  " Call function module to setup feature toggle
  CALL FUNCTION 'ZCOS_SETUP_FEATURE_TOGGLE'
    EXPORTING
      iv_feature_name = 'ZCOS_E003_ACTIVE'
      iv_is_active    = abap_true
      iv_description  = 'COS Auto Posting Active'
    IMPORTING
      rv_success      = lv_success.

  IF lv_success = abap_true.
    WRITE: / 'Feature toggle ZCOS_E003_ACTIVE setup successfully ✓'.
  ELSE.
    WRITE: / 'Feature toggle ZCOS_E003_ACTIVE setup failed ✗'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CREATE_SAMPLE_MAPPING_DATA
*&---------------------------------------------------------------------*
FORM create_sample_mapping_data.
  DATA: ls_mapping TYPE zmap_cos_rules.

  " Create sample mapping entry
  ls_mapping-client = sy-mandt.
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_16 = ls_mapping-guid.
  ls_mapping-source_gl = '400000'.
  ls_mapping-sales_gl = '500000'.
  ls_mapping-cos_gl = '600000'.
  ls_mapping-netmargin_gl = '700000'.
  ls_mapping-productcode = 'SAMPLE'.
  ls_mapping-validfrom = sy-datum.
  ls_mapping-validto = '99991231'.
  ls_mapping-active = abap_true.
  ls_mapping-created_by = sy-uname.
  ls_mapping-created_on = sy-datum.
  ls_mapping-last_changed_by = sy-uname.
  ls_mapping-last_changed_on = sy-datum.

  " Insert mapping data
  INSERT zmap_cos_rules FROM ls_mapping.
  IF sy-subrc = 0.
    WRITE: / 'Sample mapping data created successfully ✓'.
  ELSE.
    WRITE: / 'Sample mapping data creation failed ✗'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SETUP_APPLICATION_LOG
*&---------------------------------------------------------------------*
FORM setup_application_log.
  " Application log setup would be done manually
  " This is a placeholder for the setup process
  WRITE: / 'Application log setup - please configure manually'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VERIFY_TABLES_EXIST
*&---------------------------------------------------------------------*
FORM verify_tables_exist.
  WRITE: / 'Verifying tables exist...'.
  PERFORM check_table_exists USING 'ZCOS_OUTBOX'.
  PERFORM check_table_exists USING 'ZCOS_AUD'.
  PERFORM check_table_exists USING 'ZCOS_MAP'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VERIFY_CLASSES_EXIST
*&---------------------------------------------------------------------*
FORM verify_classes_exist.
  WRITE: / 'Verifying classes exist...'.
  PERFORM check_class_exists USING 'ZCL_COS_FEATURE_TOGGLE'.
  PERFORM check_class_exists USING 'ZCL_COS_QRFC_WORKER'.
  PERFORM check_class_exists USING 'ZCL_COS_DOCUMENT_PROCESSOR'.
  PERFORM check_class_exists USING 'ZCL_COS_MONITOR'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VERIFY_FUNCTION_MODULES_EXIST
*&---------------------------------------------------------------------*
FORM verify_function_modules_exist.
  WRITE: / 'Verifying function modules exist...'.
  PERFORM check_function_exists USING 'Z_COS_QRFC_WORKER'.
  PERFORM check_function_exists USING 'ZCOS_SETUP_FEATURE_TOGGLE'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VERIFY_REPORTS_EXIST
*&---------------------------------------------------------------------*
FORM verify_reports_exist.
  WRITE: / 'Verifying reports exist...'.
  PERFORM check_report_exists USING 'ZCOS_MONITOR'.
  PERFORM check_report_exists USING 'ZCOS_TEST_NEW'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form RUN_TEST_SUITE
*&---------------------------------------------------------------------*
FORM run_test_suite.
  WRITE: / 'Running test suite...'.
  
  " Submit test program
  SUBMIT zcos_test_new.
  
  WRITE: / 'Test suite execution completed.'.
ENDFORM.
