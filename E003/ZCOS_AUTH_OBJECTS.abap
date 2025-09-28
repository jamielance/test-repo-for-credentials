*&---------------------------------------------------------------------*
*& Authorization Objects for E003 Cost of Sales Auto Posting
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Authorization Object: ZCOS_MAP
*& Description: Authorization for COS Mapping Table Maintenance
*&---------------------------------------------------------------------*
* Authorization Object: ZCOS_MAP
* Activity: 01 (Create), 02 (Change), 03 (Display), 06 (Delete)
* Company Code: BUKRS
* Field Values:
*   BUKRS: Specific company codes or * for all

*&---------------------------------------------------------------------*
*& Authorization Object: ZCOS_POST
*& Description: Authorization for COS Posting
*&---------------------------------------------------------------------*
* Authorization Object: ZCOS_POST
* Activity: 01 (Post)
* Company Code: BUKRS
* G/L Account: SAKNR (COS and Sales G/L accounts)
* Field Values:
*   BUKRS: Specific company codes or * for all
*   SAKNR: Specific G/L accounts or * for all

*&---------------------------------------------------------------------*
*& Authorization Object: ZCOS_MONITOR
*& Description: Authorization for Monitoring and Logs
*&---------------------------------------------------------------------*
* Authorization Object: ZCOS_MONITOR
* Activity: 03 (Display)
* Application: SLG1 (Application Log)
* Field Values:
*   Application: ZCOS or * for all

*&---------------------------------------------------------------------*
*& Role: ZCOS_ADMIN
*& Description: Administrator role for COS Auto Posting
*&---------------------------------------------------------------------*
* Role: ZCOS_ADMIN
* Authorizations:
*   ZCOS_MAP: 01, 02, 03, 06 with BUKRS = *
*   ZCOS_POST: 01 with BUKRS = *, SAKNR = *
*   ZCOS_MONITOR: 03 with Application = ZCOS
*   F_BKPF_BUK: 03 with BUKRS = * (for document display)

*&---------------------------------------------------------------------*
*& Role: ZCOS_USER
*& Description: User role for COS Auto Posting
*&---------------------------------------------------------------------*
* Role: ZCOS_USER
* Authorizations:
*   ZCOS_MAP: 03 with BUKRS = * (read-only)
*   ZCOS_MONITOR: 03 with Application = ZCOS
*   F_BKPF_BUK: 03 with BUKRS = * (for document display)

*&---------------------------------------------------------------------*
*& Role: ZCOS_POSTER
*& Description: Poster role for COS Auto Posting
*&---------------------------------------------------------------------*
* Role: ZCOS_POSTER
* Authorizations:
*   ZCOS_POST: 01 with BUKRS = *, SAKNR = *
*   ZCOS_MONITOR: 03 with Application = ZCOS
*   F_BKPF_BUK: 03 with BUKRS = * (for document display)
