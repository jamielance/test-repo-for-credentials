# CPI Integration Guide - WEX Document Query with Existing Supplier Invoice iFlow

## Overview

This guide shows how to integrate the WEX document query service with your existing Supplier Invoice API iFlow for seamless WEX reconciliation.

## Integration Approaches

### Option 1: Separate WEX iFlows (Recommended)

Create dedicated iFlows for WEX processing with different payload mappings:

```
Invoice Processing iFlow:
[Start] → [Supplier Invoice Processing] → [WEX Check] → [Put on Hold] → [End]

WEX Reconciliation iFlow:
[Start] → [WEX SUCC Record] → [Query WEX Documents] → [Release Hold] → [End]
```

### Option 2: Single iFlow with Different Entry Points

Use one iFlow with different message types and payload mappings:

```
Unified iFlow:
[Start] → [Message Type Check] → [Invoice Processing] OR [WEX Reconciliation] → [End]
```

**Recommendation: Use Option 1** - Separate iFlows provide:
- Clear separation of concerns
- Different payload mappings
- Independent error handling
- Easier maintenance and testing

## Implementation Details

### 1. WEX Hold Process

When a WEX invoice is detected (InvoiceCategory = '027' AND ZZ1_ThirdPartyRef has data), the system automatically puts it on hold:

```groovy
// WEX Hold Process
def holdUrl = "/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice(" +
    "SupplierInvoice='${response.SupplierInvoice}'," +
    "FiscalYear='${response.FiscalYear}')"

def holdBody = [
    "PaymentBlockingReason": "A"  // Put on hold
]

// Call Supplier Invoice API to put on hold
callSupplierInvoiceAPI(holdUrl, 'PATCH', holdBody)
```

**Key Points:**
- **WEX Criteria**: InvoiceCategory = '027' AND ZZ1_ThirdPartyRef has data
- **Automatic Hold**: WEX invoices are immediately put on hold after posting
- **Error Handling**: Hold failures don't break the main invoice flow
- **Status Tracking**: Documents are marked as 'ON_HOLD' for reconciliation
- **Data Storage**: WEX data is stored for later reconciliation

### 2. WEX Check Logic in Existing iFlow

Add a conditional step after invoice processing to check if the invoice is WEX-related and put it on hold:

```groovy
// In your existing iFlow, add after invoice creation/update
def invoiceCategory = body.InvoiceCategory ?: body.ZZ1_InvoiceCategory
def paymentBlockingReason = body.PaymentBlockingReason

// Check if this is a WEX invoice that needs to be put on hold
// WEX criteria: InvoiceCategory = '027' AND ZZ1_ThirdPartyRef has data
def wexThirdPartyRef = body.ZZ1_ThirdPartyRef ?: ''
if (invoiceCategory == '027' && wexThirdPartyRef.trim() != '') {
    // This is a WEX invoice - put it on hold for reconciliation
    def holdUrl = "/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice(" +
        "SupplierInvoice='${response.SupplierInvoice}'," +
        "FiscalYear='${response.FiscalYear}')"
    
    def holdBody = [
        "PaymentBlockingReason": "A"  // Put on hold
    ]
    
    // Call Supplier Invoice API to put on hold
    try {
        callSupplierInvoiceAPI(holdUrl, 'PATCH', holdBody)
        log.info("WEX invoice ${response.SupplierInvoice} put on hold successfully")
        
        // Store for later reconciliation
        def wexData = [
            companyCode: body.CompanyCode,
            supplierInvoice: response.SupplierInvoice,
            fiscalYear: response.FiscalYear,
            wexUUID: wexThirdPartyRef,  // Use ZZ1_ThirdPartyRef as primary WEX UUID
            amount: body.InvoiceGrossAmount,
            currency: body.DocumentCurrency,
            postingDate: new Date().format("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"),
            status: 'ON_HOLD'
        ]
        
        // Store in data store for reconciliation
        store('WEX_PENDING_RECONCILIATION', wexData)
        
    } catch (Exception holdError) {
        log.error("Failed to put WEX invoice on hold: ${holdError.message}")
        // Continue processing - don't fail the main invoice flow
    }
}
```

### 3. WEX Reconciliation iFlow

Create a separate iFlow specifically for WEX reconciliation with SFTP file processing:

#### SFTP Configuration
```xml
<!-- SFTP Polling Configuration -->
<Property name="SFTP_HOST" value="your-sftp-server.com"/>
<Property name="SFTP_PORT" value="22"/>
<Property name="SFTP_USER" value="wex_user"/>
<Property name="SFTP_PASSWORD" value="encrypted_password"/>
<Property name="SFTP_DIRECTORY" value="/incoming/wex_succ"/>
<Property name="FILE_PATTERN" value="WEX_SUCC_*.txt"/>
<Property name="POLL_INTERVAL" value="300000"/> <!-- 5 minutes -->
```

#### WEX SUCC File Format (Expected)
```
CompanyCode|WexUUID|Amount|Currency|ProcessDate
5100|8PE3911J657XDN28LD|46.84|USD|2024-01-15T10:30:00Z
5100|9QF4822K768YEO39ME|125.50|EUR|2024-01-15T10:31:00Z
```

#### Parsed Payload (After File Processing)
```json
{
  "CompanyCode": "5100",
  "WexUUID": "8PE3911J657XDN28LD", 
  "Amount": 46.84,
  "Currency": "USD",
  "Tolerance": 0.50,
  "ProcessDate": "2024-01-15T10:30:00Z"
}
```

#### File Processing Logic
```groovy
// SFTP File Processing
def fileName = headers['CamelFileName']
def fileContent = body

// Parse WEX SUCC file (pipe-delimited format)
def lines = fileContent.split('\n')
def headerLine = lines[0] // Skip header
def dataLines = lines[1..-1] // Process data lines

// Process each WEX SUCC record
dataLines.each { line ->
    if (line.trim()) {
        def fields = line.split('\\|')
        if (fields.length >= 5) {
            def wexRecord = [
                CompanyCode: fields[0],
                WexUUID: fields[1],
                Amount: fields[2] as BigDecimal,
                Currency: fields[3],
                ProcessDate: fields[4]
            ]
            
            // Process this WEX record
            processWexRecord(wexRecord)
        }
    }
}

def processWexRecord(wexRecord) {
    // Extract WEX SUCC data
    def companyCode = wexRecord.CompanyCode
    def wexUUID = wexRecord.WexUUID
    def amount = wexRecord.Amount
    def currency = wexRecord.Currency
    def tolerance = 0.50 // Default tolerance

// Query WEX documents that need reconciliation
def queryUrl = "/API_WEX_DOCUMENT_QUERY/WexDocuments?\$filter=" +
    "CompanyCode eq '${companyCode}' and " +
    "WexUUID eq '${wexUUID}' and " +
    "DocumentAmount eq ${amount} and " +
    "DocumentCurrencyCode eq '${currency}'"

// Try exact match first
def wexResponse = callWexQueryService(queryUrl)

// If no exact match, try with tolerance
if (wexResponse.d.results.size() == 0 && tolerance > 0) {
    def toleranceQueryUrl = "/API_WEX_DOCUMENT_QUERY/WexDocuments?\$filter=" +
        "CompanyCode eq '${companyCode}' and " +
        "WexUUID eq '${wexUUID}' and " +
        "DocumentAmount ge ${amount - tolerance} and " +
        "DocumentAmount le ${amount + tolerance} and " +
        "DocumentCurrencyCode eq '${currency}'"
    
    wexResponse = callWexQueryService(toleranceQueryUrl)
}

if (wexResponse.d.results.size() > 0) {
    def document = wexResponse.d.results[0]
    
    // Release the hold using Supplier Invoice API
    def releaseUrl = "/API_SUPPLIERINVOICE_PROCESS_SRV/A_SupplierInvoice(" +
        "SupplierInvoice='${document.SupplierInvoice}'," +
        "FiscalYear='${document.FiscalYear}')"
    
    def releaseBody = [
        "PaymentBlockingReason": ""
    ]
    
    // Call Supplier Invoice API to release hold
    callSupplierInvoiceAPI(releaseUrl, 'PATCH', releaseBody)
    
    // Log successful reconciliation
    log.info("WEX reconciliation successful for invoice ${document.SupplierInvoice}")
    
} else {
    // No matching document found
    log.warn("No matching WEX document found for UUID ${wexUUID}, Amount ${amount}")
}
```

### 4. Enhanced Error Handling

Add WEX-specific error handling to your existing iFlow:

```groovy
try {
    // Existing invoice processing logic
    def invoiceResponse = processSupplierInvoice(invoiceData)
    
    // WEX reconciliation check
    if (isWexInvoice(invoiceData)) {
        reconcileWexInvoice(invoiceData, invoiceResponse)
    }
    
} catch (Exception error) {
    if (error.message.contains('WEX')) {
        // WEX-specific error handling
        logWexError(error, invoiceData)
        // Don't fail the main invoice processing
    } else {
        // Re-throw non-WEX errors
        throw error
    }
}
```

## Payload Mappings

### Invoice Processing iFlow Payload
```json
{
  "CompanyCode": "5100",
  "SupplierInvoice": "1234567890",
  "FiscalYear": "2024",
  "InvoiceCategory": "027",
  "InvoiceGrossAmount": 46.84,
  "DocumentCurrency": "USD",
  "Reference": "",
  "ZZ1_ThirdPartyRef": "8PE3911J657XDN28LD",
  "PaymentBlockingReason": ""
}
```

### WEX Reconciliation iFlow Payload
```json
{
  "CompanyCode": "5100",
  "WexUUID": "8PE3911J657XDN28LD",
  "Amount": 46.84,
  "Currency": "USD",
  "Tolerance": 0.50,
  "ProcessDate": "2024-01-15T10:30:00Z"
}
```

### Key Differences
- **Invoice iFlow**: Full invoice data with all fields
- **Reconciliation iFlow**: Minimal WEX SUCC record data
- **Different Field Names**: `InvoiceGrossAmount` vs `Amount`
- **Different Structure**: Complex invoice object vs simple WEX record

## Configuration Changes

### 1. Update iFlow Endpoints

Add the WEX document query service to your iFlow:

```xml
<!-- Add to your iFlow configuration -->
<Property name="WEX_QUERY_SERVICE_URL" value="/API_WEX_DOCUMENT_QUERY"/>
<Property name="WEX_QUERY_TIMEOUT" value="30000"/>
```

### 2. Add WEX Data Store

Create a data store for pending WEX reconciliations:

```xml
<DataStore name="WEX_PENDING_RECONCILIATION">
    <Property name="TableName" value="WEX_PENDING_RECONCILIATION"/>
    <Property name="RetentionPeriod" value="30"/>
</DataStore>
```

### 3. SFTP Configuration

Configure SFTP connection for WEX SUCC file processing:

```xml
<!-- SFTP Connection -->
<Property name="SFTP_HOST" value="${wex.sftp.host}"/>
<Property name="SFTP_PORT" value="${wex.sftp.port}"/>
<Property name="SFTP_USER" value="${wex.sftp.user}"/>
<Property name="SFTP_PASSWORD" value="${wex.sftp.password}"/>
<Property name="SFTP_DIRECTORY" value="${wex.sftp.directory}"/>
<Property name="FILE_PATTERN" value="WEX_SUCC_*.txt"/>
<Property name="POLL_INTERVAL" value="300000"/>
<Property name="ARCHIVE_DIRECTORY" value="${wex.sftp.archive}"/>
<Property name="ERROR_DIRECTORY" value="${wex.sftp.error}"/>
```

### 4. File Processing Configuration

```xml
<!-- File Processing Properties -->
<Property name="FILE_ENCODING" value="UTF-8"/>
<Property name="FIELD_DELIMITER" value="|"/>
<Property name="HEADER_SKIP" value="true"/>
<Property name="BATCH_SIZE" value="100"/>
<Property name="DUPLICATE_CHECK" value="true"/>
```

### 3. Update Authorization

Ensure your CPI user has access to both services:

- Existing Supplier Invoice API access
- New WEX Document Query service access

## Sample iFlow Modifications

### Invoice Processing iFlow
```
[Start] → [Process Invoice] → [Update Database] → [WEX Check?] → [Put on Hold] → [End]
                                    ↓ No
                                  [End]
```

### WEX Reconciliation iFlow (Separate)
```
[Start] → [SFTP Poll] → [Parse WEX SUCC File] → [Query WEX Documents] → [Match Found?] → [Release Hold] → [Log Success]
                                    ↓ Parse Error        ↓ No Match                    ↓ Error
                                [Log Parse Error]    [Log No Match]              [Log Error]
```

### Key Differences
- **Different Input Sources**: Invoice API vs SFTP file
- **Different Processing Logic**: Hold vs Release
- **File Processing**: Parse WEX SUCC files from SFTP
- **Independent Error Handling**: Each iFlow handles its own errors
- **Separate Monitoring**: Different metrics and alerts

### SFTP Processing Considerations
- **File Polling**: Regular polling of SFTP directory
- **File Format**: Pipe-delimited WEX SUCC records
- **Error Handling**: Parse errors, file access issues
- **File Management**: Archive processed files, handle duplicates
- **Performance**: Process multiple records per file

### WEX Reconciliation Subprocess
```
[WEX Check] → [Query WEX Documents] → [Match Found?] → [Release Hold] → [Log Success]
                    ↓ No Match                    ↓ Error
                [Log No Match]              [Log Error]
```

## Benefits of Integration

### ✅ Leverages Existing Infrastructure
- Uses your existing Supplier Invoice API iFlow
- Reuses authentication and error handling
- Maintains existing monitoring and logging

### ✅ Minimal Changes Required
- Add WEX check after invoice processing
- Add reconciliation subprocess
- No changes to core invoice logic

### ✅ Consistent Error Handling
- Same error patterns as existing iFlow
- Unified logging and monitoring
- Consistent retry logic

### ✅ Groovy Scripting Benefits
- **Native CPI Language**: Groovy is the preferred scripting language for SAP CPI
- **Better Performance**: Optimized for CPI runtime environment
- **Rich Libraries**: Access to Apache Camel and SAP-specific libraries
- **Simpler Syntax**: More readable than JavaScript for integration scenarios
- **Type Safety**: Better error handling and debugging capabilities

## Testing Strategy

### 1. Unit Testing
- Test WEX document query service independently
- Test invoice processing with WEX data
- Test hold release functionality

### 2. Integration Testing
- End-to-end WEX invoice processing
- Error scenarios (no match, multiple matches)
- Performance testing with large volumes

### 3. Regression Testing
- Ensure existing invoice processing still works
- Verify non-WEX invoices are unaffected
- Test error handling doesn't break existing flows

## Monitoring and Alerting

### Key Metrics to Monitor
- WEX invoice processing rate
- WEX reconciliation success rate
- Query service response times
- Hold release success rate

### Alerts to Configure
- WEX reconciliation failures
- Query service timeouts
- High error rates in WEX processing
- Data store growth (pending reconciliations)

## Rollout Strategy

### Phase 1: Deploy WEX Query Service
1. Deploy WEX document query service
2. Test with existing iFlow (no WEX processing yet)
3. Verify service accessibility and performance

### Phase 2: Add WEX Check
1. Add WEX detection logic to existing iFlow
2. Log WEX invoices but don't process yet
3. Monitor for any issues

### Phase 3: Enable WEX Reconciliation
1. Add WEX reconciliation subprocess
2. Enable hold release functionality
3. Monitor success rates and errors

### Phase 4: Optimization
1. Fine-tune query performance
2. Optimize reconciliation logic
3. Add advanced error handling

## Troubleshooting

### Common Issues

1. **WEX Query Timeouts**
   - Check service availability
   - Verify authorization
   - Review query complexity

2. **Hold Release Failures**
   - Verify Supplier Invoice API access
   - Check document status
   - Review error messages

3. **Performance Issues**
   - Monitor query response times
   - Check database indexes
   - Review iFlow processing times

### Debug Steps

1. Enable detailed logging for WEX processing
2. Test WEX query service independently
3. Verify data store contents
4. Check authorization for both services

## Conclusion

Integrating the WEX document query service with your existing Supplier Invoice iFlow is straightforward and provides a seamless reconciliation solution. The key is to add WEX processing as an enhancement to your existing flow rather than replacing it.

This approach maintains your existing functionality while adding powerful WEX reconciliation capabilities with minimal risk and complexity.
