*&---------------------------------------------------------------------*
*& Authorization Role: Z_API_WEX_RECON
*& Purpose: PFCG role template for WEX reconciliation service
*& Namespace: ZHLM
*& Package: ZHLM_AP_WEX
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Role Template: Z_API_WEX_RECON
*& Description: Authorization role for WEX reconciliation service
*&---------------------------------------------------------------------*

/*
 * PFCG Role Configuration:
 * 
 * 1. OData Service Authorizations:
 *    - Service: ZC_WEXOPENINVOICES_CDS
 *    - Authorization: GET (Read access to blocked WEX invoices)
 * 
 *    - Service: API_WEX_RECON_SRV  
 *    - Authorization: EXECUTE (Execute FindMatch and ReleaseIfMatch actions)
 * 
 * 2. Table Authorizations:
 *    - Table: ZWEX_CFG
 *    - Authorization: Display (SM30 view access)
 *    - Authorization: Change (Configuration maintenance)
 * 
 * 3. Additional Authorizations:
 *    - Transaction: SM30 (Table maintenance)
 *    - Object: S_TABU_DIS (Table display authorization)
 *    - Object: S_TABU_NAM (Table name authorization)
 * 
 * 4. Separate Communication Role (for CPI):
 *    - Role: Z_COMM_SUPPLIER_INVOICE_API
 *    - Service: API_SUPPLIERINVOICE_PROCESS_SRV
 *    - Authorization: POST, PATCH (Create and update supplier invoices)
 * 
 * Implementation Notes:
 * - This role should be assigned to technical users/service accounts
 * - Consider restricting by company code if needed
 * - Monitor usage through SU53 for authorization failures
 * - Regular review of role assignments recommended
 */

*&---------------------------------------------------------------------*
*& Authorization Objects (stubs for implementation)
*&---------------------------------------------------------------------*

* Authorization object for OData service access
* Object: S_ODATA
* Field: ACTVT (Activity: 03 = Display, 16 = Execute)
* Field: SERVICE (Service name: ZC_WEXOPENINVOICES_CDS, API_WEX_RECON_SRV)

* Authorization object for table access  
* Object: S_TABU_DIS
* Field: ACTVT (Activity: 03 = Display)
* Field: DICBERCLS (Table class: TRANSPARENT)
* Field: TABLENAME (Table name: ZWEX_CFG)

* Authorization object for table maintenance
* Object: S_TABU_NAM  
* Field: ACTVT (Activity: 02 = Change)
* Field: TABLENAME (Table name: ZWEX_CFG)
