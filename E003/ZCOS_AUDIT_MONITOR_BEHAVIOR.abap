*&---------------------------------------------------------------------*
*& Behavior Definition: ZCOS_AUDIT_MONITOR_BDEF
*& Description: Behavior Definition for COS Audit Monitor
*&---------------------------------------------------------------------*
managed implementation in class zbp_zcos_audit_monitor_cds unique;
strict ( 2 );

define behavior for ZCOS_AUDIT_MONITOR_CDS alias AuditMonitor
persistent table zcos_audit
lock master
authorization master ( instance )
etag master posted_at
{
  // Administrative fields (read-only)
  field ( readonly ) client, guid, posted_at, posted_by;
  
  // Business fields (read-only for monitoring)
  field ( readonly ) bukrs, gjahr, belnr_cos, belnr_src;
  field ( readonly ) cos_amount, cos_amount_currency;
  field ( readonly ) status, reversal_doc, reversal_gjahr;
  
  // Associations (read-only)
  association _company { }
  association _posted_by { }
  association _cos_doc { }
  association _src_doc { }
  
  // Standard operations
  action ( features : instance ) refresh;
  action ( features : instance ) view_document;
  action ( features : instance ) reverse_document;
}
