# WEX Violations Reconciliation for SAP S/4HANA

## Overview

This solution implements WEX (Wage Exchange) violations reconciliation for SAP S/4HANA Cloud Private Edition, designed to be consumed by SAP Integration Suite iFlows.

## Business Process

1. **WEX Invoice Posting**: Invoices are posted via Supplier Invoice API with:
   - `InvoiceCategory = '027'` (custom field from REQ1)
   - `PaymentBlock = 'A'` (blocked for payment)
   - WEX UUID stored in header Reference and/or custom ref `ZZ1_ThirdPartyRef`

2. **WEX SUCC Record**: Later, a WEX SUCC record arrives with:
   - CompanyCode
   - WexUUID
   - Amount
   - Currency

3. **Reconciliation Service**: Provides two main functions:
   - **FindMatch**: Locate blocked invoice for UUID, confirm amount (± tolerance) and currency
   - **ReleaseIfMatch**: If matched, clear PaymentBlockingReason (unblock) or return keys for CPI PATCH

## Architecture

### Namespace & Package
- **Namespace**: ZHLM
- **Package**: ZHLM_AP_WEX
- **Clean Core**: No modifications to SAP standard; all Z* objects
- **Target**: S/4HANA Cloud Private Edition with OData V2 exposure

### Components

1. **DDIC Objects**
   - Domain: `ZHLM_DOM_WEX_UUID` (CHAR 32)
   - Data Element: `ZHLM_DE_WEX_UUID`
   - Table: `ZWEX_CFG` (configuration table)

2. **CDS Views**
   - `ZC_WexOpenInvoices`: Read model for blocked WEX invoices
   - `ZC_WexMatchResult`: Result structure for FindMatch action
   - `ZC_WexReleaseResult`: Result structure for ReleaseIfMatch action

3. **RAP Business Object**
   - Behavior definition with static actions
   - Implementation classes for action logic

4. **OData Service**
   - Service: `API_WEX_RECON_SRV`
   - Entity sets and actions exposed

## Configuration

### Table ZWEX_CFG

| Field | Type | Description |
|-------|------|-------------|
| BUKRS | CHAR(4) | Company Code (Key) |
| AMT_TOLERANCE | CURR(13,2) | Amount tolerance for matching |
| CURRENCY | CHAR(5) | Optional currency filter |
| ACTIVE | FLAG | Active configuration flag |
| ALLOW_INTERNAL_UNBLOCK | FLAG | Feature flag for internal unblocking |
| UPDATED_AT | TIMESTAMPL | Last update timestamp |

### Setup Steps

1. **Create Configuration Entries**
   ```sql
   INSERT INTO ZWEX_CFG VALUES (
     '100',  -- Client
     '5100', -- Company Code
     '0.00', -- Amount Tolerance
     '',     -- Currency (optional)
     'X',    -- Active
     'X',    -- Allow Internal Unblock
     CURRENT_TIMESTAMP
   );
   ```

2. **Replace Source View**
   - Update `ZC_WexOpenInvoices.ddic`
   - Replace `REPLACE_ME_SOURCE_VIEW` with actual released supplier invoice view
   - Common options: `I_SupplierInvoice`, `I_SupplierInvoiceItem`

3. **Create Secondary Index**
   ```sql
   CREATE INDEX idx_wex_invoices ON <source_view> (
     CompanyCode, Reference, PaymentBlockingReason, ZZ1_InvoiceCategory
   );
   ```

## API Usage

### Service Endpoints

- **Base URL**: `/sap/opu/odata/sap/API_WEX_RECON_SRV/`
- **Entity Set**: `WexOpenInvoices`
- **Actions**: `FindMatch`, `ReleaseIfMatch`

### FindMatch Action

**Endpoint**: `POST .../API_WEX_RECON_SRV/FindMatch`

**Request Body**:
```json
{
  "CompanyCode": "5100",
  "WexUUID": "8PE3911J657XDN28LD",
  "Amount": "46.84",
  "Currency": "USD",
  "Tolerance": "0.00"
}
```

**Response**:
```json
{
  "MatchFound": true,
  "CompanyCode": "5100",
  "SupplierInvoice": "1234567890",
  "FiscalYear": "2024",
  "InvoiceGrossAmount": "46.84",
  "DocumentCurrency": "USD",
  "Message": "Match found successfully"
}
```

**Error Responses**:
- `404`: No candidate found
- `409`: Ambiguous candidates (multiple matches)
- `422`: Currency mismatch or amount out of tolerance

### ReleaseIfMatch Action

**Endpoint**: `POST .../API_WEX_RECON_SRV/ReleaseIfMatch`

**Request Body**: Same as FindMatch

**Response (Internal Unblock Enabled)**:
```json
{
  "Unblocked": true,
  "CompanyCode": "5100",
  "SupplierInvoice": "1234567890",
  "FiscalYear": "2024",
  "Message": "Invoice unblocked successfully"
}
```

**Response (Internal Unblock Disabled)**:
```json
{
  "Unblocked": false,
  "CompanyCode": "5100",
  "SupplierInvoice": "1234567890",
  "FiscalYear": "2024",
  "Message": "Use SupplierInvoice PATCH with PaymentBlockingReason=''"
}
```

### CPI PATCH (when internal unblock disabled)

**Endpoint**: `PATCH /sap/opu/odata/sap/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice(SupplierInvoice='1234567890',FiscalYear='2024')`

**Request Body**:
```json
{
  "PaymentBlockingReason": ""
}
```

## Error Handling

| HTTP Code | Business Code | Description |
|-----------|---------------|-------------|
| 404 | NO_CANDIDATE | No blocked invoice found for UUID |
| 409 | AMBIGUOUS_CANDIDATES | Multiple invoices match UUID |
| 412 | NOT_BLOCKED | Invoice found but not blocked |
| 422 | CURRENCY_MISMATCH | Currency doesn't match |
| 422 | AMOUNT_OUT_OF_TOLERANCE | Amount difference exceeds tolerance |

## Authorization

### Role: Z_API_WEX_RECON

**OData Service Authorizations**:
- `ZC_WEXOPENINVOICES_CDS`: GET (read access)
- `API_WEX_RECON_SRV`: EXECUTE (action execution)

**Table Authorizations**:
- `ZWEX_CFG`: Display, Change (configuration maintenance)

### Communication Role: Z_COMM_SUPPLIER_INVOICE_API

**Supplier Invoice API**:
- `API_SUPPLIERINVOICE_PROCESS_SRV`: POST, PATCH (create/update invoices)

## Testing

### ABAP Unit Tests

Test class: `ZCL_TC_WEX_RECONCILIATION`

**Test Scenarios**:
- `test_findmatch_no_candidate`: 404 error when no match
- `test_findmatch_ambiguous_candidates`: 409 error for multiple matches
- `test_findmatch_currency_mismatch`: 422 error for currency mismatch
- `test_findmatch_amount_out_of_tolerance`: 422 error for amount mismatch
- `test_findmatch_happy_path`: Successful match
- `test_releaseifmatch_guarded_flag_off`: Returns keys for CPI PATCH
- `test_releaseifmatch_enabled_flag_on`: Internal unblock successful

### Manual Testing

1. **Setup Test Data**:
   - Create blocked WEX invoices
   - Configure tolerance settings
   - Set feature flags

2. **Test FindMatch**:
   - Valid match scenario
   - No candidate scenario
   - Ambiguous candidates scenario
   - Currency/amount mismatch scenarios

3. **Test ReleaseIfMatch**:
   - With internal unblock enabled
   - With internal unblock disabled

## Performance Considerations

1. **Database Indexes**:
   - Secondary index on source view: `(CompanyCode, Reference, PaymentBlockingReason)`
   - Consider additional index on `ZZ1_InvoiceCategory`

2. **Caching**:
   - Configuration table `ZWEX_CFG` is buffered (SINGLE_RECORD)
   - Consider application-level caching for frequently accessed configs

3. **Query Optimization**:
   - Use specific field selection (no SELECT *)
   - Leverage CDS view associations
   - Consider read-only scenarios for better performance

## Deployment

### Prerequisites

1. SAP S/4HANA Cloud Private Edition
2. ABAP Development Tools (ADT)
3. Integration Suite (for CPI consumption)

### Deployment Steps

1. **Import DDIC Objects**:
   - Domain and data element
   - Configuration table
   - Create secondary indexes

2. **Activate CDS Views**:
   - Update source view reference
   - Activate all CDS view entities

3. **Deploy RAP Objects**:
   - Behavior definition
   - Implementation classes
   - Service definition

4. **Configure Authorization**:
   - Create PFCG roles
   - Assign to service users
   - Test authorization

5. **Setup Configuration**:
   - Populate `ZWEX_CFG` table
   - Set appropriate tolerances
   - Configure feature flags

6. **Register Services**:
   - Register OData services
   - Test service endpoints
   - Validate with CPI

## Monitoring & Maintenance

### Key Metrics

1. **Reconciliation Success Rate**
2. **Average Processing Time**
3. **Error Frequency by Type**
4. **Configuration Usage Patterns**

### Maintenance Tasks

1. **Regular Review**:
   - Authorization assignments
   - Configuration settings
   - Performance metrics

2. **Error Monitoring**:
   - SU53 for authorization failures
   - Application logs for business errors
   - CPI monitoring for integration issues

3. **Data Cleanup**:
   - Archive processed invoices
   - Clean up test data
   - Maintain configuration history

## Troubleshooting

### Common Issues

1. **404 No Candidate**:
   - Check invoice posting with correct category
   - Verify UUID storage in Reference/ZZ1_ThirdPartyRef
   - Confirm payment blocking status

2. **409 Ambiguous Candidates**:
   - Review UUID uniqueness
   - Check for duplicate postings
   - Implement additional matching criteria

3. **422 Validation Errors**:
   - Verify currency consistency
   - Check amount tolerance settings
   - Review decimal precision

4. **Authorization Failures**:
   - Check PFCG role assignments
   - Verify service authorizations
   - Review user permissions

### Debug Steps

1. **Enable Debugging**:
   - Set breakpoints in action implementations
   - Enable SQL trace for database queries
   - Monitor OData service calls

2. **Check Configuration**:
   - Verify `ZWEX_CFG` entries
   - Test tolerance calculations
   - Validate feature flags

3. **Review Logs**:
   - Application logs for business logic
   - System logs for technical issues
   - CPI logs for integration problems

## Support & Contact

For technical support or questions regarding this WEX reconciliation solution:

- **Namespace**: ZHLM
- **Package**: ZHLM_AP_WEX
- **Documentation**: This guide and inline code comments
- **Testing**: ABAP Unit test classes included

---

*This solution follows SAP Clean Core principles and uses only released APIs and views.*
