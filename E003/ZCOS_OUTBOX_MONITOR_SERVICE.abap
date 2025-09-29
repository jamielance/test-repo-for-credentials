*&---------------------------------------------------------------------*
*& Service Definition: ZCOS_OUTBOX_MONITOR_SRV
*& Description: Service Definition for COS Outbox Monitor
*&---------------------------------------------------------------------*
@EndUserText.label: 'COS Outbox Monitor Service'
define service ZCOS_OUTBOX_MONITOR_SRV {
  expose ZCOS_OUTBOX_MONITOR_CDS as OutboxMonitor;
}
