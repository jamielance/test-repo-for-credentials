*&---------------------------------------------------------------------*
*& Service Definition: ZCOS_AUDIT_MONITOR_SRV
*& Description: Service Definition for COS Audit Monitor
*&---------------------------------------------------------------------*
@EndUserText.label: 'COS Audit Monitor Service'
define service ZCOS_AUDIT_MONITOR_SRV {
  expose ZCOS_AUDIT_MONITOR_CDS as AuditMonitor;
}
