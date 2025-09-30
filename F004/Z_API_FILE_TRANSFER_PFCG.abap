*&---------------------------------------------------------------------*
*& Authorization Objects: ZFILE
*& Description: Authorization objects for File Transfer Service
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Authorization Object: ZFILE_OPER
*& Description: File Transfer Operations
*&---------------------------------------------------------------------*
* Field: ZFILE_OPER
* Values: 
*   MOVE    - Move file operations
*   COPY    - Copy file operations  
*   DELETE  - Delete file operations
*   ALL     - All operations

*&---------------------------------------------------------------------*
*& Authorization Object: ZFILE_DIR
*& Description: Directory Access
*&---------------------------------------------------------------------*
* Field: ZFILE_DIR
* Values:
*   INPUT   - Input directory access
*   OUTPUT  - Output directory access
*   ARCHIVE - Archive directory access
*   ALL     - All directories

*&---------------------------------------------------------------------*
*& Authorization Object: ZFILE_AUDIT
*& Description: Audit Trail Access
*&---------------------------------------------------------------------*
* Field: ZFILE_AUDIT
* Values:
*   READ    - Read audit records
*   WRITE   - Write audit records
*   ALL     - Full audit access

*&---------------------------------------------------------------------*
*& Authorization Object: ZFILE_ADMIN
*& Description: Administrative Functions
*&---------------------------------------------------------------------*
* Field: ZFILE_ADMIN
* Values:
*   CONFIG  - Configuration management
*   MONITOR - System monitoring
*   CLEANUP - Cleanup operations
*   ALL     - All administrative functions

*&---------------------------------------------------------------------*
*& Role: ZFILE_USER
*& Description: Standard file transfer user
*&---------------------------------------------------------------------*
* ZFILE_OPER: MOVE, COPY
* ZFILE_DIR: INPUT, OUTPUT
* ZFILE_AUDIT: READ

*&---------------------------------------------------------------------*
*& Role: ZFILE_ADMIN
*& Description: File transfer administrator
*&---------------------------------------------------------------------*
* ZFILE_OPER: ALL
* ZFILE_DIR: ALL
* ZFILE_AUDIT: ALL
* ZFILE_ADMIN: ALL

*&---------------------------------------------------------------------*
*& Role: ZFILE_MONITOR
*& Description: File transfer monitoring user
*&---------------------------------------------------------------------*
* ZFILE_AUDIT: READ
* ZFILE_ADMIN: MONITOR

*&---------------------------------------------------------------------*
*& Role: ZFILE_ARCHIVE
*& Description: Archive operations user
*&---------------------------------------------------------------------*
* ZFILE_OPER: MOVE, DELETE
* ZFILE_DIR: ARCHIVE
* ZFILE_AUDIT: READ, WRITE
