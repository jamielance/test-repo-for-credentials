# File Transfer Service - OData & SFTP Integration

## Overview
This service provides secure file transfer operations for SAP systems with OData integration, designed for SFTP file processing workflows including moving READ and PROCESSED files to Archive/Delete folders.

## Architecture
- **Clean-core extensibility** approach
- **OData V4** service for external integration
- **Secure file operations** with input validation
- **Comprehensive audit trail** and logging
- **Authorization-based access control**

## Components

### 1. Core Service Class
- **ZCL_FILE_TRANSFER_SERVICE**: Main service class with secure file operations
- Supports MOVE, COPY, and DELETE operations
- Input validation and security checks
- Comprehensive error handling and logging

### 2. OData Service
- **API_FILE_TRANSFER**: Service definition for external access
- **ZC_FILE_TRANSFER_AUDIT**: Audit trail view
- **ZC_FILE_TRANSFER_REQUEST**: Request management view
- **ZC_FILE_SYSTEM_INFO**: System monitoring view

### 3. Security & Authorization
- **ZFILE_OPER**: File operation permissions
- **ZFILE_DIR**: Directory access control
- **ZFILE_AUDIT**: Audit trail access
- **ZFILE_ADMIN**: Administrative functions

### 4. Message Management
- **ZFILE**: Message class with 25+ predefined messages
- Structured error handling and user feedback
- Multi-language support ready

## Installation Steps

### 1. Create DDIC Objects
```abap
" Create tables in SE11/SE80
- ZFILE_TRANSFER_AUDIT
- ZFILE_TRANSFER_REQUEST  
- ZFILE_SYSTEM_INFO
```

### 2. Create Authorization Objects
```abap
" In SE11
- ZFILE_OPER (File operations)
- ZFILE_DIR (Directory access)
- ZFILE_AUDIT (Audit access)
- ZFILE_ADMIN (Admin functions)
```

### 3. Create PFCG Roles
```abap
" In PFCG
- ZFILE_USER (Standard user)
- ZFILE_ADMIN (Administrator)
- ZFILE_MONITOR (Monitoring)
- ZFILE_ARCHIVE (Archive operations)
```

### 4. Deploy OData Service
```abap
" In ADT
- Create service definition API_FILE_TRANSFER
- Create CDS views for audit and requests
- Deploy service to gateway
```

## Usage Examples

### 1. Basic File Move Operation
```abap
DATA: lo_service TYPE REF TO zcl_file_transfer_service,
      ls_result  TYPE zcl_file_transfer_service=>ty_file_transfer_result.

lo_service = NEW zcl_file_transfer_service( ).

ls_result = lo_service->move_files_to_output_directory(
  iv_input_file       = '/sap/input/processed_file.txt'
  iv_output_directory  = '/sap/archive/'
  iv_operation_type    = 'MOVE'
  iv_create_backup     = abap_true
).
```

### 2. OData Service Calls
```http
# Move file operation
POST /sap/opu/odata/sap/API_FILE_TRANSFER/FileTransferService
Content-Type: application/json

{
  "source_file": "/sap/input/processed_file.txt",
  "target_directory": "/sap/archive/",
  "operation_type": "MOVE",
  "create_backup": true
}
```

### 3. Query Audit Trail
```http
# Get recent file operations
GET /sap/opu/odata/sap/API_FILE_TRANSFER/FileTransferAudit?$filter=OperationDate ge 2024-01-01&$orderby=operation_timestamp desc

# Get failed operations
GET /sap/opu/odata/sap/API_FILE_TRANSFER/FileTransferAudit?$filter=success_flag eq false

# Get operations by user
GET /sap/opu/odata/sap/API_FILE_TRANSFER/FileTransferAudit?$filter=created_by eq 'USER123'
```

## Security Features

### 1. Input Validation
- Path sanitization (removes dangerous characters)
- Path traversal protection (blocks `../` attacks)
- File size limits and type validation
- Directory existence checks

### 2. Authorization Control
- Operation-based permissions (MOVE, COPY, DELETE)
- Directory-level access control
- Audit trail access restrictions
- Administrative function separation

### 3. Audit Trail
- Complete operation logging
- Performance metrics tracking
- Error detail capture
- User activity monitoring

## Configuration

### 1. System Commands
Configure the following commands in SM69:
- `MOVE_FILE`: Unix move command
- `COPY_FILE`: Unix copy command  
- `DELETE_FILE`: Unix delete command

### 2. Directory Permissions
Ensure proper directory permissions:
```bash
# Input directory (read/write)
chmod 755 /sap/input/

# Archive directory (write)
chmod 755 /sap/archive/

# Log directory (write)
chmod 755 /sap/logs/
```

### 3. Message Configuration
Maintain message class ZFILE in SE91 with all 25 messages for proper error handling.

## Monitoring & Troubleshooting

### 1. Audit Logs
- Check ZFILE_TRANSFER_AUDIT table for operation history
- Monitor error patterns and performance metrics
- Use SLG1 for detailed application logs

### 2. System Monitoring
- Monitor directory space usage via ZC_FILE_SYSTEM_INFO
- Check system command execution in SM37
- Review authorization failures in SU53

### 3. Performance Optimization
- Index audit tables on frequently queried fields
- Monitor processing times and optimize slow operations
- Consider batch processing for large file volumes

## Error Handling

### Common Error Scenarios
1. **Permission Denied**: Check user authorizations and directory permissions
2. **File Not Found**: Verify source file existence and path correctness
3. **Directory Full**: Monitor space usage and cleanup old files
4. **Security Violation**: Review input validation and path sanitization

### Error Codes
- `1`: Input validation error
- `2`: Path validation error  
- `3`: Source file not found
- `4`: Directory creation failed
- `5`: Operation execution failed

## Best Practices

### 1. File Operations
- Always validate file paths before operations
- Use appropriate operation types (MOVE vs COPY)
- Implement proper error handling and retry logic
- Monitor disk space regularly

### 2. Security
- Follow principle of least privilege for authorizations
- Regular audit of file operations and access patterns
- Implement file type restrictions where appropriate
- Monitor for suspicious activity

### 3. Performance
- Use batch processing for large file volumes
- Implement proper indexing on audit tables
- Monitor and optimize slow operations
- Consider asynchronous processing for large files

## Support

### 1. Documentation
- Keep this guide updated with changes
- Document any customizations or extensions
- Maintain troubleshooting procedures

### 2. Monitoring
- Set up alerts for critical errors
- Monitor system performance and space usage
- Regular review of audit logs

### 3. Maintenance
- Regular cleanup of old audit records
- Update authorizations as needed
- Monitor and update system commands
