*&---------------------------------------------------------------------*
*& Table Definition: ZINV_CAT_BANK
*& Bank Determination by Invoice Category and Payment Method
*& Package: ZHLM_AP_INTF
*& Transport: Development Class
*&---------------------------------------------------------------------*

* Table: ZINV_CAT_BANK
* Description: Bank determination rules by invoice category and payment method
* Technical Settings:
*   - Data Class: APPL0 (Application Data)
*   - Size Category: 0 (Small)
*   - Buffering: Single Record Buffering
*   - Log Data Changes: Yes
*   - Change Document: Yes

* Key Fields:
*   BUKRS      - Company Code (BUKRS, 4 CHAR)
*   INV_CAT    - Invoice Category (ZHLM_DE_INV_CAT, 3 CHAR)
*   PAYMETH    - Payment Method (ZHLM_DE_PAYMETH, 1 CHAR)
*   VALIDFROM  - Valid From Date (DATS, 8 CHAR)

* Data Fields:
*   VALIDTO        - Valid To Date (DATS, 8 CHAR)
*   ISACTIVE       - Active Flag (FLAG, 1 CHAR)
*   HBKID          - House Bank ID (ZHLM_DE_HBKID, 5 CHAR)
*   HKTID          - House Bank Account ID (ZHLM_DE_HKTID, 5 CHAR)
*   BVTYP          - Partner Bank Type (ZHLM_DE_BVTYP, 4 CHAR)
*   PAYMENTID      - Payment ID (CHAR5, 5 CHAR)
*   LAST_CHANGED_AT - Last Changed At (TIMESTAMPL, 15 CHAR)

* Indexes:
*   Primary Key: BUKRS, INV_CAT, PAYMETH, VALIDFROM (Unique)
*   Secondary: BUKRS, INV_CAT, PAYMETH, ISACTIVE (Non-unique)

* Change Document:
*   Object Class: ZINV_CAT_BANK
*   Change Document Table: ZINV_CAT_BANK_CD
*   Fields to be logged: All fields except LAST_CHANGED_AT

* Field Texts:
*   BUKRS: Company Code
*   INV_CAT: Invoice Category
*   PAYMETH: Payment Method
*   VALIDFROM: Valid From
*   VALIDTO: Valid To
*   ISACTIVE: Active
*   HBKID: House Bank
*   HKTID: House Bank Account
*   BVTYP: Partner Bank Type
*   PAYMENTID: Payment ID
*   LAST_CHANGED_AT: Last Changed At
