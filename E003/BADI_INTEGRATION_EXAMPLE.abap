*&---------------------------------------------------------------------*
*& Example: How to call ZCL_COS_AC_DOCUMENT_HANDLER from IF_EX_ACC_DOCUMENT
*& Description: Example implementation showing how to integrate COS processing
*&              into the existing IF_EX_ACC_DOCUMENT change method
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& In your existing IF_EX_ACC_DOCUMENT implementation, add this code
*& to the change_document method:
*&---------------------------------------------------------------------*

METHOD if_ex_acc_document~change_document.
  " Your existing change_document logic here...
  
  " Add COS processing call
  TRY.
      DATA(lo_cos_handler) = NEW zcl_cos_ac_document_handler( ).
      
      " Process AC document for COS
      DATA(lv_processed) = lo_cos_handler->process_ac_document( c_accit ).
      
      " Log if any COS entries were created
      IF lv_processed = abap_true.
        " Optional: Add any additional logging or notifications
        MESSAGE i001(zcos) WITH 'COS processing completed successfully'.
      ENDIF.
      
    CATCH zcx_cos_processing_error INTO DATA(lx_cos_error).
      " Handle COS processing errors
      MESSAGE e002(zcos) WITH lx_cos_error->error_message.
    CATCH cx_sy_create_object_error.
      " Handle object creation errors
      MESSAGE e003(zcos) WITH 'Failed to create COS handler'.
  ENDTRY.
  
  " Continue with your existing change_document logic...
ENDMETHOD.

*&---------------------------------------------------------------------*
*& Alternative: More sophisticated integration with error handling
*&---------------------------------------------------------------------*

METHOD if_ex_acc_document~change_document.
  " Your existing change_document logic here...
  
  " Add COS processing call with comprehensive error handling
  TRY.
      " Check if COS processing should be attempted
      DATA(lo_cos_handler) = NEW zcl_cos_ac_document_handler( ).
      
      " First check if document should be processed
      IF lo_cos_handler->should_process_document( c_accit ) = abap_true.
        
        " Process AC document for COS
        DATA(lv_processed) = lo_cos_handler->process_ac_document( c_accit ).
        
        " Log result
        IF lv_processed = abap_true.
          " Success - COS entries created
          MESSAGE i001(zcos) WITH 'COS processing completed successfully'.
        ELSE.
          " No COS entries created (not an error)
          MESSAGE i004(zcos) WITH 'No COS processing required for this document'.
        ENDIF.
        
      ELSE.
        " Document doesn't meet COS processing criteria
        " No action needed - this is normal
      ENDIF.
      
    CATCH zcx_cos_processing_error INTO DATA(lx_cos_error).
      " COS processing failed - log error but don't stop document posting
      " This ensures AC document posting continues even if COS fails
      MESSAGE w005(zcos) WITH lx_cos_error->error_message.
      
      " Optional: Log to application log for monitoring
      DATA(lo_logger) = NEW zcl_cos_logger( ).
      lo_logger->log_error(
        iv_message_id = 'ZCOS'
        iv_message_no = '013'
        iv_message_v1 = lx_cos_error->error_message
        iv_message_v2 = |GUID: { lx_cos_error->guid }|
      ).
      lo_logger->save_log( ).
      
    CATCH cx_sy_create_object_error.
      " Object creation failed - log but don't stop document posting
      MESSAGE w006(zcos) WITH 'Failed to create COS handler - COS processing skipped'.
      
  ENDTRY.
  
  " Continue with your existing change_document logic...
ENDMETHOD.

*&---------------------------------------------------------------------*
*& Integration with existing validation logic
*&---------------------------------------------------------------------*

METHOD if_ex_acc_document~change_document.
  " Your existing change_document logic here...
  
  " Add COS processing after your existing validations
  " but before any document modifications
  
  " Perform COS processing if feature is active
  DATA(lo_feature_toggle) = NEW zcl_cos_feature_toggle( ).
  IF lo_feature_toggle->is_feature_active( 'ZCOS_E003_ACTIVE' ) = abap_true.
    
    TRY.
        DATA(lo_cos_handler) = NEW zcl_cos_ac_document_handler( ).
        DATA(lv_processed) = lo_cos_handler->process_ac_document( c_accit ).
        
        " Optional: Add COS processing result to document context
        " for use in other BAdI methods if needed
        
      CATCH zcx_cos_processing_error INTO DATA(lx_cos_error).
        " Log error but continue with document processing
        MESSAGE w005(zcos) WITH lx_cos_error->error_message.
    ENDTRY.
    
  ENDIF.
  
  " Continue with your existing change_document logic...
ENDMETHOD.

*&---------------------------------------------------------------------*
*& Message class entries needed (add to ZCOS message class):
*&---------------------------------------------------------------------*

*& Message 001: I001 - COS processing completed successfully
*& Message 002: E002 - COS processing error: &1
*& Message 003: E003 - Failed to create COS handler
*& Message 004: I004 - No COS processing required for this document
*& Message 005: W005 - COS processing warning: &1
*& Message 006: W006 - Failed to create COS handler - COS processing skipped

*&---------------------------------------------------------------------*
*& Benefits of this approach:
*&---------------------------------------------------------------------*

*& 1. Clean separation of concerns
*& 2. COS processing doesn't interfere with existing AC document logic
*& 3. Easy to enable/disable via feature toggle
*& 4. Comprehensive error handling
*& 5. Detailed logging for monitoring
*& 6. AC document posting continues even if COS fails
*& 7. Easy to test and maintain
*& 8. Follows clean code principles
