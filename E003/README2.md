# E003 Cost of Sales Auto Posting Enhancement

## Overview
This enhancement implements automatic Cost of Sales (COS) posting for UK GAAP in SAP S/4HANA 2023 Private Cloud. When an external invoice posts successfully (after E008 validation), it creates a separate follow-on FI document that gross-ups revenue and posts COS.

## Architecture
- **Clean-core extensibility** approach
- **qRFC + BAPI** for asynchronous processing
- **BAdI AC_DOCUMENT** for validation and trigger
- **No BTE** usage as per requirements
- **Idempotency** and **audit trail** built-in

## Components

### 1. DDIC Tables
- **ZCOS_MAP**: Mapping table for trigger G/L → Sales G/L + COS G/L
- **ZCOS_OUTBOX**: Payload for qRFC processing
- **ZCOS_AUD**: Audit table for idempotency and tracking

### 2. BAdI Implementation
- **ZIM_AC_DOCUMENT_COS**: BAdI implementation for AC_DOCUMENT
- Validates trigger G/L and product code presence
- Checks E008 validation status
- Creates outbox entry and enqueues qRFC unit

### 3. qRFC Worker
- **Z_COS_QRFC_WORKER**: Remote-enabled function module
- Processes outbox entries asynchronously
- Calculates COS amounts with proration
- Creates FI documents using BAPI_ACC_DOCUMENT_POST

### 4. Fiori/RAP App
- **ZCOS_MAP_CDS**: CDS view for mapping table
- **ZCOS_MAP_META**: Metadata extension for Fiori UI
- **ZCOS_MAP_BEHAVIOR**: Behavior definition with validations

### 5. Monitoring & Utilities
- **ZCOS_MONITOR**: Monitoring program for SLG1 logs
- **ZCOS_TEST**: Test program for validation
- **ZCOS_FEATURE_TOGGLE**: Feature toggle management

## Installation Steps

### 1. Create DDIC Objects
```abap
" Create tables in SE11/SE80
- ZCOS_MAP
- ZCOS_OUTBOX  
- ZCOS_AUD
```

### 2. Create BAdI Implementation
```abap
" In SE18/SE19
- Create BAdI implementation for AC_DOCUMENT
- Implement class ZCL_IM_AC_DOCUMENT_COS
```

### 3. Create Function Modules
```abap
" In SE37
- Z_COS_QRFC_WORKER (Remote-enabled)
- ZCOS_SETUP_FEATURE_TOGGLE
- ZCOS_DEACTIVATE_FEATURE
```

### 4. Setup Feature Toggle
```abap
" Execute function module
CALL FUNCTION 'ZCOS_SETUP_FEATURE_TOGGLE'
```

### 5. Create Fiori App
```abap
" In ADT
- Create CDS view ZCOS_MAP_CDS
- Create metadata extension ZCOS_MAP_META
- Create behavior definition ZCOS_MAP_BDEF
- Deploy Fiori app
```

### 6. Setup Authorizations
```abap
" Create authorization objects
- ZCOS_MAP (table maintenance)
- ZCOS_POST (posting)
- ZCOS_MONITOR (monitoring)

" Create roles
- ZCOS_ADMIN
- ZCOS_USER  
- ZCOS_POSTER
```

## Configuration

### 1. Mapping Table Setup
Use the Fiori app or SE16 to maintain ZCOS_MAP:
- Company Code
- Trigger G/L Account
- Product Code
- Sales G/L Account
- COS G/L Account
- Validity Period
- Optional margin percentage

### 2. Queue Configuration
- Queue naming: `COS_<BUKRS>_<YYYYMMDD>`
- Monitor via SMQ1/SMQ2/SM58
- Configure parallel processing if needed

### 3. Monitoring Setup
- Use ZCOS_MONITOR program
- Check SLG1 logs for application ZCOS
- Monitor qRFC queues

## Testing

### 1. Unit Testing
```abap
" Execute test program
REPORT zcos_test.
" This will:
" - Create test mapping entries
" - Post test supplier invoice
" - Verify COS document creation
```

### 2. Integration Testing
1. Create mapping entries in ZCOS_MAP
2. Post supplier invoice with trigger G/L
3. Verify outbox entry creation
4. Check qRFC processing
5. Verify COS document creation
6. Test reversal scenarios

## Functional Rules

### Trigger Conditions
- Supplier invoice from external system (IntelliFleet/Leasewave)
- E008 validation must have passed
- Trigger G/L + Product Code combination must exist in mapping

### COS Calculation
- **Simple**: Direct cost from invoice
- **Multi-line**: Prorated by line charge / total charge
- **Rounding**: 2 decimals, adjust final line for debit=credit
- **Tolerance**: Skip if |COS| < 0.01

### Document Creation
- New FI document: DR COS / CR Sales
- No tax postings
- Copy cost objects from source or default via OKB9
- Reference to source document

### Reversals
- Do not enqueue for reversals
- Reverse follow-on COS document using standard reversal
- Maintain symmetry

## Monitoring & Troubleshooting

### 1. SLG1 Logs
- Application: ZCOS
- Check for errors in BAdI and qRFC worker
- Monitor processing times

### 2. qRFC Monitoring
- SMQ1: Queue overview
- SMQ2: Queue details
- SM58: qRFC monitoring

### 3. Data Consistency
- Check ZCOS_OUTBOX for pending entries
- Verify ZCOS_AUD for processed entries
- Ensure no duplicate processing

## Performance Considerations

### 1. Queue Processing
- Configure appropriate number of parallel processes
- Monitor queue depth and processing times
- Consider queue partitioning by company code

### 2. Database Performance
- Index on ZCOS_MAP: BUKRS, TRIGGER_GL, PRODUCT_CODE, VALID_FROM
- Index on ZCOS_OUTBOX: GUID, STATUS, CREATED_AT
- Index on ZCOS_AUD: GUID, BUKRS, BELNR_SRC

### 3. Memory Usage
- qRFC worker processes in background
- Consider memory limits for large documents
- Monitor ABAP memory usage

## Security

### 1. Authorizations
- Restrict COS/Sales G/L posting to authorized users
- Separate roles for maintenance vs. monitoring
- Audit trail for all changes

### 2. Data Protection
- Sensitive financial data in audit tables
- Consider data retention policies
- Secure access to monitoring tools

## Maintenance

### 1. Regular Tasks
- Monitor queue processing
- Check for failed entries
- Review audit logs
- Update mappings as needed

### 2. Updates
- Test in development first
- Use feature toggle for controlled rollout
- Maintain backward compatibility
- Document all changes

## Support

### 1. Documentation
- Keep this README updated
- Document any customizations
- Maintain test scenarios

### 2. Troubleshooting
- Check SLG1 logs first
- Verify feature toggle status
- Check mapping table entries
- Monitor qRFC processing

### 3. Escalation
- Contact SAP support for BAdI issues
- Check SAP Notes for known issues
- Review enhancement documentation
