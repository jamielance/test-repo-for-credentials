*&---------------------------------------------------------------------*
*& Table Definition: ZINV_CAT_1099
*& 1099/WHT Determination by Invoice Category
*& Package: ZHLM_AP_INTF
*& Transport: Development Class
*&---------------------------------------------------------------------*

* Table: ZINV_CAT_1099
* Description: 1099/Withholding Tax determination rules by invoice category
* Technical Settings:
*   - Data Class: APPL0 (Application Data)
*   - Size Category: 0 (Small)
*   - Buffering: Single Record Buffering
*   - Log Data Changes: Yes
*   - Change Document: Yes

* Key Fields:
*   BUKRS      - Company Code (BUKRS, 4 CHAR)
*   INV_CAT    - Invoice Category (ZHLM_DE_INV_CAT, 3 CHAR)
*   VALIDFROM  - Valid From Date (DATS, 8 CHAR)

* Data Fields:
*   VALIDTO        - Valid To Date (DATS, 8 CHAR)
*   ISACTIVE       - Active Flag (FLAG, 1 CHAR)
*   WHTTYPE        - Withholding Tax Type (ZHLM_DE_WHTTYPE, 2 CHAR)
*   WHTCODE        - Withholding Tax Code (ZHLM_DE_WHTCODE, 4 CHAR)
*   LAST_CHANGED_AT - Last Changed At (TIMESTAMPL, 15 CHAR)

* Indexes:
*   Primary Key: BUKRS, INV_CAT, VALIDFROM (Unique)
*   Secondary: BUKRS, INV_CAT, ISACTIVE (Non-unique)

* Change Document:
*   Object Class: ZINV_CAT_1099
*   Change Document Table: ZINV_CAT_1099_CD
*   Fields to be logged: All fields except LAST_CHANGED_AT

* Field Texts:
*   BUKRS: Company Code
*   INV_CAT: Invoice Category
*   VALIDFROM: Valid From
*   VALIDTO: Valid To
*   ISACTIVE: Active
*   WHTTYPE: WHT Type
*   WHTCODE: WHT Code
*   LAST_CHANGED_AT: Last Changed At
