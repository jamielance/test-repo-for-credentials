# ZHLM CPI/iFlow Lookup Services Documentation

## Project Overview

**Namespace:** ZHLM  
**Package:** ZHLM_AP_INTF  
**Transport:** Development Class  
**Authors:** [Placeholder]  
**Clean Core:** Yes - No table append to SAP standard; all custom in Z*

## Scope

This project provides ABAP infrastructure for CPI/iFlow lookup services that enrich Supplier Invoice payloads with:

1. **REQ2 - Bank Determination:** (BUKRS, InvoiceCategory, PaymentMethod) → HouseBank (HBKID), HouseBankAccount (HKTID), PartnerBankType (BVTYP opt), PaymentID (opt)
2. **REQ3 - 1099/WHT Determination:** (BUKRS, InvoiceCategory) → WithholdingTaxType and Code

## Architecture

### Data Model

```
ZINV_CAT_BANK (Bank Determination Rules)
├── Key: BUKRS, INV_CAT, PAYMETH, VALIDFROM
├── Data: VALIDTO, ISACTIVE, HBKID, HKTID, BVTYP, PAYMENTID, LAST_CHANGED_AT
└── Indexes: Primary (unique), Secondary (BUKRS, INV_CAT, PAYMETH, ISACTIVE)

ZINV_CAT_1099 (WHT Determination Rules)
├── Key: BUKRS, INV_CAT, VALIDFROM
├── Data: VALIDTO, ISACTIVE, WHTTYPE, WHTCODE, LAST_CHANGED_AT
└── Indexes: Primary (unique), Secondary (BUKRS, INV_CAT, ISACTIVE)
```

### CDS View Entities

- **ZC_BankDetermination:** Read-only view with validity and active filters
- **ZC_WhtDetermination:** Read-only view with validity and active filters
- Both views are OData-published for CPI consumption

### BAdI Implementation

- **ZCL_IM_MRM_CHECK_INV_CLOUD:** Validates invoice data against rules
- Re-derives both bank and WHT rules
- Raises clear error messages on mismatch
- No enrichment - validation only

## OData Service URLs

### Bank Determination Service

**Service Name:** ZC_BANKDETERMINATION_CDS  
**Base URL:** `/sap/opu/odata/sap/ZC_BANKDETERMINATION_CDS/`

**Example CPI Calls:**
```
GET /sap/opu/odata/sap/ZC_BANKDETERMINATION_CDS/ZC_BankDetermination?$filter=CompanyCode eq '5100' and InvoiceCategory eq '203' and PaymentMethod eq 'A'&$top=1

GET /sap/opu/odata/sap/ZC_BANKDETERMINATION_CDS/ZC_BankDetermination?$filter=CompanyCode eq '1000' and InvoiceCategory eq '001' and PaymentMethod eq 'B'&$top=1
```

**Response Fields:**
- `CompanyCode` (BUKRS)
- `InvoiceCategory` (INV_CAT)
- `PaymentMethod` (PAYMETH)
- `HouseBank` (HBKID)
- `HouseBankAccount` (HKTID)
- `PartnerBankType` (BVTYP)
- `PaymentID` (PAYMENTID)
- `ValidFrom` (VALIDFROM)
- `ValidTo` (VALIDTO)
- `IsActive` (ISACTIVE)
- `LastChangedAt` (LAST_CHANGED_AT)

### WHT Determination Service

**Service Name:** ZC_WHTDETERMINATION_CDS  
**Base URL:** `/sap/opu/odata/sap/ZC_WHTDETERMINATION_CDS/`

**Example CPI Calls:**
```
GET /sap/opu/odata/sap/ZC_WHTDETERMINATION_CDS/ZC_WhtDetermination?$filter=CompanyCode eq '5100' and InvoiceCategory eq '203'&$top=1

GET /sap/opu/odata/sap/ZC_WHTDETERMINATION_CDS/ZC_WhtDetermination?$filter=CompanyCode eq '1000' and InvoiceCategory eq '001'&$top=1
```

**Response Fields:**
- `CompanyCode` (BUKRS)
- `InvoiceCategory` (INV_CAT)
- `WithholdingTaxType` (WHTTYPE)
- `WithholdingTaxCode` (WHTCODE)
- `ValidFrom` (VALIDFROM)
- `ValidTo` (VALIDTO)
- `IsActive` (ISACTIVE)
- `LastChangedAt` (LAST_CHANGED_AT)

## CPI Integration

### Bank Determination Mapping

CPI should map the response fields to Supplier Invoice payload:

```json
{
  "CompanyCode": "5100",
  "InvoiceCategory": "203",
  "PaymentMethod": "A",
  "HouseBank": "HBK01",
  "HouseBankAccount": "HKT01",
  "PartnerBankType": "BV01",
  "PaymentID": "PAY01"
}
```

### WHT Determination Mapping

CPI should map the response fields to Supplier Invoice payload:

```json
{
  "CompanyCode": "5100",
  "InvoiceCategory": "203",
  "WithholdingTaxType": "WT",
  "WithholdingTaxCode": "WTC1"
}
```

### Error Handling

- Empty result = no rule found (CPI should handle gracefully)
- Multiple results prevented by unique indexes
- BAdI validation provides clear error messages for mismatches

## Service Registration

### Service Names

1. **ZC_BANKDETERMINATION_CDS** - Bank determination service
2. **ZC_WHTDETERMINATION_CDS** - WHT determination service

### SICF Activation Steps

1. Go to transaction SICF
2. Navigate to `/sap/bc/ui5_ui5/sap/zhlm_ap_intf/`
3. Right-click on ZC_BANKDETERMINATION_CDS service
4. Choose "Activate Service"
5. Repeat for ZC_WHTDETERMINATION_CDS service
6. Verify services are active (green status)

### Service Testing

Use transaction `/IWFND/MAINT_SERVICE` to test the services:

1. Search for "ZC_BANKDETERMINATION_CDS"
2. Click "Gateway Client"
3. Test with sample filter parameters
4. Verify response format and data

## Authorization

### PFCG Role: Z_API_RULES_READ

**Purpose:** Display access to lookup tables and execute GET on CDS services

**Authorizations:**
- S_TABU_NAM: Display on ZINV_CAT_BANK, ZINV_CAT_1099
- S_RFC: Execute lookup functions
- S_SERVICE: GET on both CDS services
- ZHLM_API_READ: Custom authorization for API access
- ZHLM_BANK_DET: Bank determination display
- ZHLM_WHT_DET: WHT determination display

### PFCG Role: Z_API_SUPPINV_POST

**Purpose:** Placeholder for Supplier Invoice API communication user

**Note:** This is a placeholder role for the separate Supplier Invoice API user who performs create/post operations.

## BAdI Validation

### Implementation: ZCL_IM_MRM_CHECK_INV_CLOUD

**Behavior:**
1. Reads CompanyCode (BUKRS), PaymentMethod, and custom field ZZ1_INVOICECATEGORY from invoice context
2. Looks up ZINV_CAT_BANK for active record on posting date
3. If not found → raises error: "No bank rule for &1/&2/&3"
4. If found, compares incoming HBKID/HKTID (and BVTYP if populated) with rule
5. On mismatch → error with expected values
6. Looks up ZINV_CAT_1099 and validates WHT data if rule exists
7. No enrichment - only validation

### Error Messages

- **E001:** No bank rule for CompanyCode/InvoiceCategory/PaymentMethod
- **E002:** House Bank mismatch (expected vs actual)
- **E003:** House Bank Account mismatch (expected vs actual)
- **E004:** Partner Bank Type mismatch (expected vs actual)
- **E005:** WHT Type mismatch (expected vs actual)
- **E006:** WHT Code mismatch (expected vs actual)

## Performance Considerations

### Database Design

- **Primary Indexes:** Unique constraints prevent ambiguous configurations
- **Secondary Indexes:** Optimize common query patterns
- **Buffering:** Single record buffering for frequently accessed data
- **Null-safe dates:** COALESCE for validity period handling

### Query Optimization

- Uses SELECT SINGLE with proper WHERE clauses
- Filters on ISACTIVE = 'X' and current date validity
- No SELECT * - only required fields
- Proper index usage for performance

## Testing

### ABAP Unit Tests

**Test Class:** ZCL_TC_BANK_DETERMINATION

**Test Scenarios:**
1. **Happy Path:** Both rules exist and data matches
2. **No Bank Rule:** Expect error when rule not found
3. **Bank Data Mismatch:** Expect error when data doesn't match rule
4. **WHT Data Mismatch:** Expect error when WHT data doesn't match rule
5. **Ambiguous Config Prevention:** Negative test for unique constraint

### Test Data Management

- Test methods include setup/teardown for data isolation
- Helper methods for inserting test rules
- Cleanup after each test to prevent data pollution

## Maintenance

### Change Documentation

- Both tables have change document enabled
- All fields logged except LAST_CHANGED_AT
- Object classes: ZINV_CAT_BANK, ZINV_CAT_1099

### Data Lifecycle

- Rules have validity periods (VALIDFROM/VALIDTO)
- ISACTIVE flag for soft deletion
- LAST_CHANGED_AT timestamp for audit trail

### Monitoring

- Use standard SAP monitoring tools
- Monitor service performance via /IWFND/MAINT_SERVICE
- Check BAdI execution logs for validation issues

## Deployment Checklist

1. **DDIC Objects:** Create domains, data elements, tables
2. **CDS Views:** Create and activate view entities
3. **BAdI Implementation:** Create and activate BAdI class
4. **Service Registration:** Activate OData services in SICF
5. **Authorization:** Create PFCG roles and assign to users
6. **Testing:** Execute ABAP Unit tests
7. **Integration Testing:** Test with CPI/iFlow
8. **Go-Live:** Monitor service performance and error logs

## Support

For issues or questions:
1. Check BAdI execution logs for validation errors
2. Verify service activation status in SICF
3. Review authorization assignments
4. Check table data for rule configuration
5. Monitor performance via standard SAP tools
