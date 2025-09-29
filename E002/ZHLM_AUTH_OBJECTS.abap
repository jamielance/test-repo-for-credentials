*&---------------------------------------------------------------------*
*& Authorization Objects for ZHLM Invoice Category Lookup Services
*& Package: ZHLM_AP_INTF
*& Transport: Development Class
*&---------------------------------------------------------------------*

* Authorization Object: ZHLM_BANK_DET
* Description: Authorization for Bank Determination Lookup
* Fields:
*   ACTVT - Activity (01=Display, 02=Create, 03=Change, 06=Delete)
*   BUKRS - Company Code
*   INV_CAT - Invoice Category
*   PAYMETH - Payment Method
*----------------------------------------------------------------------*

* Authorization Object: ZHLM_WHT_DET
* Description: Authorization for WHT Determination Lookup
* Fields:
*   ACTVT - Activity (01=Display, 02=Create, 03=Change, 06=Delete)
*   BUKRS - Company Code
*   INV_CAT - Invoice Category
*----------------------------------------------------------------------*

* Authorization Object: ZHLM_API_READ
* Description: Authorization for API Read Operations
* Fields:
*   ACTVT - Activity (01=Display)
*   SERVICE - Service Name (ZC_BANKDETERMINATION_CDS, ZC_WHTDETERMINATION_CDS)
*----------------------------------------------------------------------*
