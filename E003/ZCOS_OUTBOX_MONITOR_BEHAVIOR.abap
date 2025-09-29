*&---------------------------------------------------------------------*
*& Behavior Definition: ZCOS_OUTBOX_MONITOR_BDEF
*& Description: Behavior Definition for COS Outbox Monitor
*&---------------------------------------------------------------------*
managed implementation in class zbp_zcos_outbox_monitor_cds unique;
strict ( 2 );

define behavior for ZCOS_OUTBOX_MONITOR_CDS alias OutboxMonitor
persistent table zcos_outbox
lock master
authorization master ( instance )
etag master created_at
{
  // Administrative fields (read-only)
  field ( readonly ) client, guid, created_at, processed_at;
  
  // Business fields (read-only for monitoring)
  field ( readonly ) bukrs, gjahr, belnr_src, trigger_gl, product_code;
  field ( readonly ) total_charge, total_charge_currency;
  field ( readonly ) cos_amount, cos_amount_currency;
  field ( readonly ) status, error_message;
  
  // Associations (read-only)
  association _company { }
  association _trigger_gl { }
  association _audit { }
  
  // Standard operations
  action ( features : instance ) refresh;
  action ( features : instance ) process_retry;
  action ( features : instance ) view_details;
}
