*&---------------------------------------------------------------------*
*& Authorization Role: Z_API_WEX_QUERY
*& Purpose: PFCG role for WEX document query service
*& Namespace: ZHLM
*& Package: ZHLM_AP_WEX
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Role Template: Z_API_WEX_QUERY
*& Description: Simple authorization role for WEX document query service
*&---------------------------------------------------------------------*

/*
 * PFCG Role Configuration:
 * 
 * 1. OData Service Authorizations:
 *    - Service: ZC_WEX_DOCUMENTS_FOR_RELEASE_CDS
 *    - Authorization: GET (Read access to WEX documents)
 * 
 *    - Service: API_WEX_DOCUMENT_QUERY  
 *    - Authorization: GET (Query WEX documents)
 * 
 * 2. Additional Authorizations:
 *    - Object: S_TABU_DIS (Table display authorization for underlying views)
 *    - Object: S_ODATA (OData service access)
 * 
 * 3. Separate Communication Role (for CPI):
 *    - Role: Z_COMM_SUPPLIER_INVOICE_API
 *    - Service: API_SUPPLIERINVOICE_PROCESS_SRV
 *    - Authorization: PATCH (Update supplier invoices to release holds)
 * 
 * Implementation Notes:
 * - This role only needs READ access - no write operations
 * - Much simpler than RAP-based solution
 * - CPI handles the business logic and hold release
 * - Monitor usage through SU53 for authorization failures
 */

*&---------------------------------------------------------------------*
*& Authorization Objects (stubs for implementation)
*&---------------------------------------------------------------------*

* Authorization object for OData service access
* Object: S_ODATA
* Field: ACTVT (Activity: 03 = Display)
* Field: SERVICE (Service name: ZC_WEX_DOCUMENTS_FOR_RELEASE_CDS, API_WEX_DOCUMENT_QUERY)

* Authorization object for table access  
* Object: S_TABU_DIS
* Field: ACTVT (Activity: 03 = Display)
* Field: DICBERCLS (Table class: TRANSPARENT)
* Field: TABLENAME (Table name: underlying supplier invoice view)
