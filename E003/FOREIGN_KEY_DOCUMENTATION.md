# Foreign Key Documentation for COS Auto Posting Solution

## Overview
This document provides comprehensive documentation of all foreign key relationships implemented in the COS Auto Posting solution DDIC objects. Foreign keys ensure data integrity, enable proper referential constraints, and improve system performance through optimized joins.

## Foreign Key Implementation Summary

### **ZCOS_OUTBOX Table**

#### **1. Company Code Reference (T001)**
```abap
foreign key [0..1,1..1] t001
  references t001 on zcos_outbox.bukrs = t001.bukrs
  and zcos_outbox.client = t001.mandt;
```
- **Purpose**: Ensures company code exists and is valid
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)
- **Business Rule**: Every outbox entry must reference a valid company code

#### **2. Trigger G/L Account Reference (SKA1)**
```abap
foreign key [0..1,1..1] ska1_trigger
  references ska1 on zcos_outbox.trigger_gl = ska1.saknr
  and zcos_outbox.bukrs = ska1.bukrs
  and zcos_outbox.client = ska1.mandt;
```
- **Purpose**: Ensures trigger G/L account exists and is valid for the company code
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)
- **Business Rule**: Trigger G/L account must exist in the chart of accounts

#### **3. COS Mapping Reference (ZCOS_MAP)**
```abap
foreign key [0..1,1..1] zcos_map
  left join to zcos_map on zcos_outbox.bukrs = zcos_map.bukrs
  and zcos_outbox.trigger_gl = zcos_map.trigger_gl
  and zcos_outbox.product_code = zcos_map.product_code
  and zcos_outbox.client = zcos_map.client
  and zcos_outbox.created_at >= zcos_map.valid_from
  and zcos_outbox.created_at <= zcos_map.valid_to
  and zcos_map.deleted = @false;
```
- **Purpose**: Ensures valid COS mapping exists for the combination
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)
- **Business Rule**: Mapping must be valid for the creation date and not deleted

---

### **ZCOS_AUD Table**

#### **1. Company Code Reference (T001)**
```abap
foreign key [0..1,1..1] t001
  left join to t001 on zcos_aud.bukrs = t001.bukrs
  and zcos_aud.client = t001.mandt;
```
- **Purpose**: Ensures company code exists and is valid
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **2. Outbox Reference (ZCOS_OUTBOX)**
```abap
foreign key [0..1,1..1] zcos_outbox
  left join to zcos_outbox on zcos_aud.guid = zcos_outbox.guid
  and zcos_aud.client = zcos_outbox.client;
```
- **Purpose**: Links audit entry to original outbox entry
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)
- **Business Rule**: Every audit entry must reference a valid outbox entry

#### **3. User Reference - Posted By (USR21)**
```abap
foreign key [0..1,1..1] usr21
  left join to usr21 on zcos_aud.posted_by = usr21.bname;
```
- **Purpose**: Ensures user exists in the system
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **4. COS Document Reference (BKPF)**
```abap
foreign key [0..1,1..1] bkpf_cos
  left join to bkpf on zcos_aud.belnr_cos = bkpf.belnr
  and zcos_aud.gjahr = bkpf.gjahr
  and zcos_aud.bukrs = bkpf.bukrs
  and zcos_aud.client = bkpf.mandt;
```
- **Purpose**: Links to the generated COS document
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **5. Source Document Reference (BKPF)**
```abap
foreign key [0..1,1..1] bkpf_src
  left join to bkpf on zcos_aud.belnr_src = bkpf.belnr
  and zcos_aud.gjahr = bkpf.gjahr
  and zcos_aud.bukrs = bkpf.bukrs
  and zcos_aud.client = bkpf.mandt;
```
- **Purpose**: Links to the original source document
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **6. Reversal Document Reference (BKPF)**
```abap
foreign key [0..1,1..1] bkpf_reversal
  left join to bkpf on zcos_aud.reversal_doc = bkpf.belnr
  and zcos_aud.reversal_gjahr = bkpf.gjahr
  and zcos_aud.bukrs = bkpf.bukrs
  and zcos_aud.client = bkpf.mandt;
```
- **Purpose**: Links to reversal document if applicable
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

---

### **ZCOS_MAP Table**

#### **1. Company Code Reference (T001)**
```abap
foreign key [0..1,1..1] t001
  left join to t001 on zcos_map.bukrs = t001.bukrs
  and zcos_map.client = t001.mandt;
```
- **Purpose**: Ensures company code exists and is valid
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **2. Trigger G/L Account Reference (SKA1)**
```abap
foreign key [0..1,1..1] ska1_trigger
  left join to ska1 on zcos_map.trigger_gl = ska1.saknr
  and zcos_map.bukrs = ska1.bukrs
  and zcos_map.client = ska1.mandt;
```
- **Purpose**: Ensures trigger G/L account exists and is valid
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **3. Sales G/L Account Reference (SKA1)**
```abap
foreign key [0..1,1..1] ska1_sales
  left join to ska1 on zcos_map.sales_gl = ska1.saknr
  and zcos_map.bukrs = ska1.bukrs
  and zcos_map.client = ska1.mandt;
```
- **Purpose**: Ensures sales G/L account exists and is valid
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **4. COS G/L Account Reference (SKA1)**
```abap
foreign key [0..1,1..1] ska1_cos
  left join to ska1 on zcos_map.cos_gl = ska1.saknr
  and zcos_map.bukrs = ska1.bukrs
  and zcos_map.client = ska1.mandt;
```
- **Purpose**: Ensures COS G/L account exists and is valid
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **5. Created By User Reference (USR21)**
```abap
foreign key [0..1,1..1] usr21_created
  left join to usr21 on zcos_map.created_by = usr21.bname;
```
- **Purpose**: Ensures created by user exists in the system
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

#### **6. Changed By User Reference (USR21)**
```abap
foreign key [0..1,1..1] usr21_changed
  left join to usr21 on zcos_map.changed_by = usr21.bname;
```
- **Purpose**: Ensures changed by user exists in the system
- **Cardinality**: 0..1 to 1..1 (optional to mandatory)

---

## Foreign Key Benefits

### **1. Data Integrity**
- **Referential Integrity**: Ensures all referenced records exist
- **Consistency**: Prevents orphaned records
- **Validation**: Automatic validation of foreign key values

### **2. Performance Optimization**
- **Join Optimization**: Database can optimize joins based on foreign key relationships
- **Index Usage**: Foreign keys often have associated indexes
- **Query Planning**: Better query execution plans

### **3. Business Rule Enforcement**
- **Company Code Validation**: Ensures all records reference valid company codes
- **G/L Account Validation**: Ensures all G/L accounts exist and are valid
- **User Validation**: Ensures all user references are valid
- **Document Validation**: Ensures all document references are valid

### **4. System Integration**
- **SAP Standard Tables**: References to standard SAP tables (T001, SKA1, BKPF, USR21)
- **Cross-Table Consistency**: Ensures data consistency across related tables
- **Audit Trail**: Maintains proper audit relationships

## Implementation Notes

### **Cardinality Explanation**
- **`[0..1,1..1]`**: Optional to mandatory relationship
- **Left Join**: Ensures records can exist even if referenced record doesn't exist
- **Multiple Conditions**: Complex join conditions for multi-field foreign keys

### **Naming Conventions**
- **Descriptive Names**: Foreign key names clearly indicate the relationship
- **Suffixes**: Used to distinguish between multiple references to the same table
- **Examples**: `ska1_trigger`, `ska1_sales`, `ska1_cos`

### **Complex Join Conditions**
- **Date Ranges**: ZCOS_MAP uses date range validation
- **Status Conditions**: Includes deleted flag validation
- **Multi-Field Keys**: Combines multiple fields for unique identification

## Maintenance Considerations

### **1. Data Migration**
- Ensure all referenced data exists before creating foreign key constraints
- Consider data cleanup before implementing foreign keys
- Plan for data validation and correction

### **2. Performance Impact**
- Foreign keys may impact insert/update performance
- Consider indexing strategy for foreign key fields
- Monitor query performance after implementation

### **3. Error Handling**
- Implement proper error handling for foreign key violations
- Provide meaningful error messages to users
- Consider soft validation vs. hard constraints

---

**Note**: These foreign key relationships provide a robust foundation for data integrity and system integration in the COS Auto Posting solution. They ensure that all data relationships are properly maintained and validated throughout the system lifecycle.
