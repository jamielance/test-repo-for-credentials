*&---------------------------------------------------------------------*
*& Function Module: Z_COS_QRFC_WORKER
*& Description: qRFC Worker for Cost of Sales Auto Posting
*&---------------------------------------------------------------------*
*& Purpose: Processes outbox entries for Cost of Sales auto posting
*&          This function module is called asynchronously via qRFC
*&          to create COS documents based on supplier invoices
*&---------------------------------------------------------------------*
*& Parameters:
*&   IV_GUID  - Unique identifier for the outbox entry
*&   IV_BUKRS - Company code
*&   IV_GJAHR - Fiscal year
*&   IV_BELNR - Source document number
*&---------------------------------------------------------------------*
*& Author: System Generated
*& Date:   &SY-DATUM
*&---------------------------------------------------------------------*
FUNCTION z_cos_qrfc_worker.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_GUID) TYPE SYSUUID_X16
*"     VALUE(IV_BUKRS) TYPE BUKRS
*"     VALUE(IV_GJAHR) TYPE GJAHR
*"     VALUE(IV_BELNR) TYPE BELNR_D
*"----------------------------------------------------------------------

  DATA: lo_qrfc_worker TYPE REF TO zcl_cos_qrfc_worker,
        ls_result      TYPE zcl_cos_qrfc_worker=>ty_processing_result.

  " Create qRFC worker instance
  lo_qrfc_worker = NEW zcl_cos_qrfc_worker( ).

  " Process outbox entry
  ls_result = lo_qrfc_worker->process_outbox_entry(
    iv_guid = iv_guid
    iv_bukrs = iv_bukrs
    iv_gjahr = iv_gjahr
    iv_belnr = iv_belnr
  ).

  " Log result
  IF ls_result-success = abap_true.
    DATA(ls_success_msg) = zcl_cos_message_utility=>get_cos_document_created_success(
      iv_document = ls_result-cos_document
      iv_year = ls_result-cos_year
    ).
    MESSAGE ls_success_msg-msgty(zcos) WITH ls_success_msg-msgv1 ls_success_msg-msgv2.
  ELSE.
    DATA(ls_error_msg) = zcl_cos_message_utility=>get_processing_error( ls_result-error_message ).
    MESSAGE ls_error_msg-msgty(zcos) WITH ls_error_msg-msgv1.
  ENDIF.

ENDFUNCTION.
