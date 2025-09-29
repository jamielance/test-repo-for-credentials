*&---------------------------------------------------------------------*
*& PFCG Role Template: Z_API_RULES_READ
*& Description: Role for API Rules Read Operations
*& Package: ZHLM_AP_INTF
*& Transport: Development Class
*&---------------------------------------------------------------------*

* Role: Z_API_RULES_READ
* Description: API Rules Read Access
* 
* Authorization Data:
*   S_TABU_NAM - Table Authorization
*     ZINV_CAT_BANK - Display (03)
*     ZINV_CAT_1099 - Display (03)
*   
*   S_RFC - RFC Authorization
*     Function Group: ZHLM_AP_INTF
*     Function: ZHLM_GET_BANK_RULE
*     Function: ZHLM_GET_WHT_RULE
*   
*   S_SERVICE - Service Authorization
*     Service: ZC_BANKDETERMINATION_CDS
*     Method: GET
*     Service: ZC_WHTDETERMINATION_CDS
*     Method: GET
*   
*   ZHLM_API_READ - Custom Authorization
*     ACTVT: 01 (Display)
*     SERVICE: ZC_BANKDETERMINATION_CDS
*     SERVICE: ZC_WHTDETERMINATION_CDS
*   
*   ZHLM_BANK_DET - Bank Determination Authorization
*     ACTVT: 01 (Display)
*     BUKRS: * (All Company Codes)
*     INV_CAT: * (All Invoice Categories)
*     PAYMETH: * (All Payment Methods)
*   
*   ZHLM_WHT_DET - WHT Determination Authorization
*     ACTVT: 01 (Display)
*     BUKRS: * (All Company Codes)
*     INV_CAT: * (All Invoice Categories)

* Service Registration Names:
*   - ZC_BANKDETERMINATION_CDS
*   - ZC_WHTDETERMINATION_CDS

* SICF Activation Steps:
*   1. Go to transaction SICF
*   2. Navigate to /sap/bc/ui5_ui5/sap/zhlm_ap_intf/
*   3. Right-click on ZC_BANKDETERMINATION_CDS service
*   4. Choose "Activate Service"
*   5. Repeat for ZC_WHTDETERMINATION_CDS service
*   6. Verify services are active (green status)

* Role Assignment:
*   - Assign role Z_API_RULES_READ to CPI service user
*   - Ensure user has S_UHTTP authorization for HTTP calls
*   - Test with /IWFND/MAINT_SERVICE transaction
