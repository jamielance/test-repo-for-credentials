# F004 - File Transfer Service

## Overview
Independent OData service for secure SFTP file transfer operations, designed for moving READ and PROCESSED files to Archive/Delete folders.

## Key Features
- ✅ **Secure File Operations** - Input validation, path sanitization, authorization control
- ✅ **OData V4 Integration** - RESTful API for external system integration
- ✅ **Comprehensive Audit Trail** - Complete operation logging and monitoring
- ✅ **Multiple Operation Types** - MOVE, COPY, DELETE file operations
- ✅ **Error Handling** - Structured error management with retry logic
- ✅ **Authorization Control** - Role-based access to operations and directories

## Files in this Folder

### Core Service
- `ZCL_FILE_TRANSFER_SERVICE.abap` - Main service class with secure file operations
- `API_FILE_TRANSFER.srv` - OData service definition

### Data Models
- `ZC_FILE_TRANSFER_AUDIT.ddic` - Audit trail CDS view
- `ZC_FILE_TRANSFER_REQUEST.ddic` - Request management CDS view  
- `ZC_FILE_SYSTEM_INFO.ddic` - System monitoring CDS view

### Security & Configuration
- `Z_API_FILE_TRANSFER_PFCG.abap` - Authorization objects and roles
- `ZFILE_MESSAGES.abap` - Message class for error handling

### Documentation
- `FILE_TRANSFER_SERVICE_GUIDE.md` - Complete implementation guide
- `README.md` - This overview file

## Quick Start

### 1. Basic File Move
```abap
DATA: lo_service TYPE REF TO zcl_file_transfer_service,
      ls_result  TYPE zcl_file_transfer_service=>ty_file_transfer_result.

lo_service = NEW zcl_file_transfer_service( ).

ls_result = lo_service->move_files_to_output_directory(
  iv_input_file       = '/sap/input/processed_file.txt'
  iv_output_directory  = '/sap/archive/'
  iv_operation_type    = 'MOVE'
).
```

### 2. OData Service Call
```http
POST /sap/opu/odata/sap/API_FILE_TRANSFER/FileTransferService
Content-Type: application/json

{
  "source_file": "/sap/input/processed_file.txt",
  "target_directory": "/sap/archive/",
  "operation_type": "MOVE"
}
```

## Security Highlights
- **Input Validation**: Sanitizes file paths and prevents injection attacks
- **Path Traversal Protection**: Blocks `../` and similar attacks
- **Authorization Control**: Role-based access to operations and directories
- **Audit Logging**: Complete operation trail for security monitoring

## Use Cases
- **SFTP File Processing**: Move processed files to archive folders
- **Data Migration**: Secure file operations during system migrations
- **Backup Operations**: Automated file backup and cleanup
- **Integration**: OData service for external system file operations

## Next Steps
1. Review the complete implementation guide: `FILE_TRANSFER_SERVICE_GUIDE.md`
2. Set up authorization objects and roles
3. Configure system commands in SM69
4. Deploy OData service to gateway
5. Test with sample file operations

## Support
- Check audit logs in `ZC_FILE_TRANSFER_AUDIT` for operation history
- Monitor system health via `ZC_FILE_SYSTEM_INFO`
- Review error messages in message class `ZFILE`
