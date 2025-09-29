# Currency Code Semantic Annotation - Quick Reference

## 🎯 Overview
Added currency code semantic annotations to all amount fields in the COS Auto Posting solution for proper currency handling and display.

## 📊 Database Changes

### ZCOS_OUTBOX Table
```abap
total_charge          : dmbtr;
@Semantics.amount.currencyCode : 'total_charge_currency'
total_charge_currency : waers not null default 'GBP';
cos_amount            : dmbtr;
@Semantics.amount.currencyCode : 'cos_amount_currency'
cos_amount_currency   : waers not null default 'GBP';
```

### ZCOS_AUD Table
```abap
cos_amount            : dmbtr;
@Semantics.amount.currencyCode : 'cos_amount_currency'
cos_amount_currency   : waers not null default 'GBP';
```

## 🔧 Code Changes

### ZCL_COS_QRFC_WORKER
- **Method Signature:** Added `iv_cos_currency` parameter to `create_audit_entry`
- **Currency Usage:** Uses `iv_outbox-total_charge_currency` instead of hardcoded `c_currency`
- **BAPI Calls:** Currency passed from outbox data

### ZCL_COS_DOCUMENT_PROCESSOR
- **Outbox Creation:** Sets currency fields to 'GBP' by default
- **Currency Fields:** `total_charge_currency` and `cos_amount_currency`

### ZCL_COS_MONITOR
- **Data Structure:** Added `cos_amount_currency` to `ty_monitor_data`
- **Data Selection:** Includes currency field in SELECT statements
- **Data Mapping:** Maps currency field in data conversion

### ZCOS_MONITOR Report
- **Display:** Uses `CURRENCY ls_data-cos_amount_currency` for dynamic currency display

## ✅ Benefits

### 1. **Proper Currency Handling**
- Amount fields have associated currency codes
- Prevents currency mismatches
- Enables multi-currency support

### 2. **Enhanced Display**
- Dynamic currency display in reports
- Proper formatting based on actual currency
- Better user experience

### 3. **Data Integrity**
- Currency information preserved throughout processing
- Audit trail includes currency
- Consistent currency handling

## 🚀 Usage Examples

### Creating Outbox Entry
```abap
ls_outbox-total_charge = '1000.00'.
ls_outbox-total_charge_currency = 'GBP'.
ls_outbox-cos_amount_currency = 'GBP'.
```

### Displaying Amounts
```abap
WRITE: ls_data-cos_amount CURRENCY ls_data-cos_amount_currency.
```

### BAPI Currency Usage
```abap
ls_currencyamount-currency = iv_outbox-total_charge_currency.
ls_currencyamount-amt_doccur = iv_cos_amount.
```

## 🔍 Validation

### Check Currency Fields
```abap
" Verify currency fields exist
SELECT SINGLE total_charge_currency, cos_amount_currency 
  FROM zcos_outbox 
  WHERE guid = 'your-guid'.

" Check audit currency
SELECT SINGLE cos_amount_currency 
  FROM zcos_aud 
  WHERE guid = 'your-guid'.
```

### Test Currency Display
```abap
" Test monitor with currency
SUBMIT zcos_monitor.
" Verify amounts display with correct currency
```

## 📋 Migration Checklist

- [ ] **Database Tables:** Currency fields added with semantic annotations
- [ ] **Classes:** Updated to handle currency fields
- [ ] **Reports:** Updated to display currency dynamically
- [ ] **Testing:** Verify currency handling works correctly
- [ ] **Documentation:** Updated to reflect currency changes

## 🎨 Semantic Annotation Pattern

```abap
@Semantics.amount.currencyCode : 'currency_field_name'
amount_field : dmbtr;
currency_field_name : waers not null default 'GBP';
```

## 🔄 Currency Flow

1. **Document Processing** → Sets default currency in outbox
2. **qRFC Processing** → Uses currency from outbox
3. **Audit Creation** → Stores currency in audit table
4. **Monitoring** → Displays currency from stored data

## 📝 Notes

- **Default Currency:** GBP (British Pound)
- **Backward Compatible:** Existing data automatically gets default currency
- **Future Ready:** Foundation for multi-currency support
- **SAP Standard:** Follows SAP semantic annotation best practices

---

**Status:** ✅ Complete - All amount fields now have proper currency code semantic annotations
