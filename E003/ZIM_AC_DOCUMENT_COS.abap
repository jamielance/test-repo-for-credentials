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

  PRIVATE SECTION.
    DATA:
      mo_document_processor TYPE REF TO zcl_cos_document_processor.

    METHODS:
      get_document_processor
        RETURNING
          VALUE(ro_processor) TYPE REF TO zcl_cos_document_processor.

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
    DATA: lo_processor TYPE REF TO zcl_cos_document_processor,
          ls_result    TYPE zcl_cos_document_processor=>ty_processing_result.

    " Get document processor
    lo_processor = get_document_processor( ).

    " Process document
    ls_result = lo_processor->process_document( c_document ).

    " Log result if processing failed
    IF ls_result-success = abap_false.
      " Error is already logged in the processor
    ENDIF.

  ENDMETHOD.

  METHOD get_document_processor.
    " Create processor instance if not already created
    IF mo_document_processor IS NOT BOUND.
      mo_document_processor = NEW zcl_cos_document_processor( ).
    ENDIF.

    ro_processor = mo_document_processor.
  ENDMETHOD.

ENDCLASS.
