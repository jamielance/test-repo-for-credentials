# WEX Document Query Service - OData Only Solution

## Overview

This is a simplified OData-only solution for WEX document identification. No RAP complexity - just a simple read-only OData service that CPI can query to identify documents eligible for hold removal.

## Architecture

### Simple OData Service
- **Service**: `API_WEX_DOCUMENT_QUERY`
- **Entity Set**: `WexDocuments`
- **View**: `ZC_WEX_DOCUMENTS_FOR_RELEASE`
- **Purpose**: Query existing SAP data to identify WEX documents

### No RAP, No Tables, No Complexity
- ✅ Simple CDS view that queries existing supplier invoice data
- ✅ OData service exposes the view
- ✅ CPI handles all business logic
- ✅ CPI uses standard Supplier Invoice API to release holds

## Business Process

1. **WEX Invoice Posting**: Invoices posted with:
   - `InvoiceCategory = '027'` (WEX category)
   - `PaymentBlock = 'A'` (blocked for payment)
   - WEX UUID in `Reference` or `ZZ1_ThirdPartyRef`

2. **CPI Reconciliation**: 
   - CPI queries `API_WEX_DOCUMENT_QUERY` to find matching documents
   - CPI applies business logic (amount tolerance, currency matching)
   - CPI uses standard Supplier Invoice API to release holds

3. **Hold Release**:
   - CPI calls `PATCH /API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice`
   - Sets `PaymentBlockingReason = ""` to release hold

## API Usage

### Service Endpoint
- **Base URL**: `/sap/opu/odata/sap/API_WEX_DOCUMENT_QUERY/`
- **Entity Set**: `WexDocuments`

### Query Examples

#### 1. Get all WEX documents for a company
```
GET .../API_WEX_DOCUMENT_QUERY/WexDocuments?$filter=CompanyCode eq '5100'
```

#### 2. Find document by WEX UUID
```
GET .../API_WEX_DOCUMENT_QUERY/WexDocuments?$filter=WexUUID eq '8PE3911J657XDN28LD'
```

#### 3. Find by amount and currency
```
GET .../API_WEX_DOCUMENT_QUERY/WexDocuments?$filter=CompanyCode eq '5100' and InvoiceGrossAmount eq 46.84 and DocumentCurrency eq 'USD'
```

#### 4. Find within amount range (tolerance)
```
GET .../API_WEX_DOCUMENT_QUERY/WexDocuments?$filter=CompanyCode eq '5100' and InvoiceGrossAmount ge 40 and InvoiceGrossAmount le 50
```

#### 5. Complex reconciliation query
```
GET .../API_WEX_DOCUMENT_QUERY/WexDocuments?$filter=CompanyCode eq '5100' and WexUUID eq '8PE3911J657XDN28LD' and InvoiceGrossAmount eq 46.84 and DocumentCurrency eq 'USD'
```

### Response Format
```json
{
  "d": {
    "results": [
      {
        "CompanyCode": "5100",
        "SupplierInvoice": "1234567890",
        "FiscalYear": "2024",
        "DocumentCurrency": "USD",
        "InvoiceGrossAmount": "46.84",
        "PaymentBlockingReason": "A",
        "Reference": "8PE3911J657XDN28LD",
        "ZZ1_ThirdPartyRef": "",
        "PostingDate": "2024-01-15T00:00:00",
        "Supplier": "1000001",
        "SupplierName": "WEX Supplier",
        "ReleaseStatus": "ELIGIBLE_FOR_RELEASE",
        "WexUUID": "8PE3911J657XDN28LD"
      }
    ]
  }
}
```

## CPI Integration

### 1. Document Identification
CPI queries the OData service to find documents matching WEX SUCC records:

```javascript
// CPI iFlow logic
var wexUUID = "8PE3911J657XDN28LD";
var companyCode = "5100";
var amount = 46.84;
var currency = "USD";
var tolerance = 0.50;

// Query for exact match first
var exactQuery = "/API_WEX_DOCUMENT_QUERY/WexDocuments?$filter=" +
  "CompanyCode eq '" + companyCode + "' and " +
  "WexUUID eq '" + wexUUID + "' and " +
  "InvoiceGrossAmount eq " + amount + " and " +
  "DocumentCurrency eq '" + currency + "'";

// If no exact match, try with tolerance
var toleranceQuery = "/API_WEX_DOCUMENT_QUERY/WexDocuments?$filter=" +
  "CompanyCode eq '" + companyCode + "' and " +
  "WexUUID eq '" + wexUUID + "' and " +
  "InvoiceGrossAmount ge " + (amount - tolerance) + " and " +
  "InvoiceGrossAmount le " + (amount + tolerance) + " and " +
  "DocumentCurrency eq '" + currency + "'";
```

### 2. Hold Release
Once document is identified, CPI releases the hold:

```javascript
// CPI PATCH to release hold
var patchUrl = "/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice(" +
  "SupplierInvoice='" + document.SupplierInvoice + "'," +
  "FiscalYear='" + document.FiscalYear + "')";

var patchBody = {
  "PaymentBlockingReason": ""
};
```

## Setup

### 1. Update Source View
Replace `REPLACE_ME_SOURCE_VIEW` in `ZC_WEX_DOCUMENTS_FOR_RELEASE.ddic` with actual released supplier invoice view.

### 2. Create Secondary Index
```sql
CREATE INDEX idx_wex_documents ON <source_view> (
  CompanyCode, PaymentBlockingReason, ZZ1_InvoiceCategory, Reference
);
```

### 3. Configure Authorization
- Create PFCG role `Z_API_WEX_QUERY`
- Assign to CPI service user
- Grant GET access to both services

### 4. Register Services
- Register `API_WEX_DOCUMENT_QUERY` service
- Test OData queries
- Configure CPI connectivity

## Advantages of OData-Only Approach

### ✅ Simplicity
- No RAP complexity
- No behavior definitions
- No action implementations
- Just a simple CDS view + OData service

### ✅ Flexibility
- CPI handles all business logic
- Easy to modify tolerance rules
- No ABAP development needed for logic changes
- Standard OData filtering and querying

### ✅ Performance
- Direct database queries
- No RAP overhead
- Efficient OData filtering
- CPI can implement caching

### ✅ Maintainability
- Fewer ABAP objects
- Standard OData patterns
- CPI handles error handling
- Easy to debug and monitor

## Error Handling

CPI handles all error scenarios:
- No documents found → Log and continue
- Multiple documents found → Log warning, use first match
- Amount/currency mismatch → Log error, skip document
- API errors → Retry logic, dead letter queue

## Monitoring

### Key Metrics (CPI)
- Query response times
- Document match rates
- Hold release success rates
- Error frequencies by type

### SAP Monitoring
- OData service usage
- Database query performance
- Authorization failures (SU53)

## Troubleshooting

### Common Issues

1. **No Documents Found**
   - Check `ZZ1_InvoiceCategory = '027'`
   - Verify `PaymentBlockingReason = 'A'`
   - Confirm UUID in Reference or ZZ1_ThirdPartyRef

2. **Slow Queries**
   - Check secondary index exists
   - Review OData filter complexity
   - Consider CPI caching

3. **Authorization Errors**
   - Verify PFCG role assignments
   - Check service authorizations
   - Review CPI user permissions

## Files in This Solution

- `ZC_WEX_DOCUMENTS_FOR_RELEASE.ddic` - CDS view for document identification
- `API_WEX_DOCUMENT_QUERY.srv` - OData service definition
- `Z_API_WEX_QUERY_PFCG.abap` - Authorization role template
- `WEX_ODATA_SOLUTION_GUIDE.md` - This documentation

## Next Steps

1. Replace source view reference
2. Create secondary index
3. Configure authorization
4. Register OData service
5. Test with CPI
6. Implement CPI iFlow logic

---

*This solution is much simpler and more appropriate for your use case than the original RAP-based approach.*
