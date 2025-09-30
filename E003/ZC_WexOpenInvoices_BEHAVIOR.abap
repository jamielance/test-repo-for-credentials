*&---------------------------------------------------------------------*
*& RAP Behavior Definition: ZC_WexOpenInvoices
*& Purpose: Behavior definition for WEX reconciliation actions
*& Namespace: ZHLM
*& Package: ZHLM_AP_WEX
*&---------------------------------------------------------------------*

managed implementation in class ZBP_C_WEXOPENINVOICES unique;
strict ( 2 );

define behavior for ZC_WexOpenInvoices
/* 
 * Read-only entity - no standard CRUD operations
 * Only custom actions are allowed
 */
{
  /* Static Actions */
  action ( features : instance ) FindMatch
    result [1] ZC_WexMatchResult;
    
  action ( features : instance ) ReleaseIfMatch
    result [1] ZC_WexReleaseResult;
    
  /* No standard CRUD operations */
  // create;
  // update;
  // delete;
}

/* 
 * Result structures for actions
 */
define behavior for ZC_WexMatchResult
{
  /* Read-only result structure */
}

define behavior for ZC_WexReleaseResult  
{
  /* Read-only result structure */
}
