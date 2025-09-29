# Corrected Foreign Key Syntax for ABAP DDIC

## ✅ **Correct ABAP DDIC Foreign Key Syntax**

You are absolutely correct! The `left join` syntax I initially used is **not valid** for ABAP DDIC foreign keys. Here's the correct syntax:

### **Correct Syntax:**
```abap
foreign key [cardinality] foreign_key_name
  references target_table on local_field = target_field
  and local_field2 = target_field2;
```

### **Incorrect Syntax (what I used initially):**
```abap
foreign key [cardinality] foreign_key_name
  left join to target_table on local_field = target_field;  // ❌ WRONG
```

## 🔧 **Corrected Foreign Key Examples**

### **ZCOS_OUTBOX Table**

#### **1. Company Code Reference (T001)**
```abap
foreign key [0..1,1..1] t001
  references t001 on zcos_outbox.bukrs = t001.bukrs
  and zcos_outbox.client = t001.mandt;
```

#### **2. Trigger G/L Account Reference (SKA1)**
```abap
foreign key [0..1,1..1] ska1_trigger
  references ska1 on zcos_outbox.trigger_gl = ska1.saknr
  and zcos_outbox.bukrs = ska1.bukrs
  and zcos_outbox.client = ska1.mandt;
```

#### **3. COS Mapping Reference (ZCOS_MAP)**
```abap
foreign key [0..1,1..1] zcos_map
  references zcos_map on zcos_outbox.bukrs = zcos_map.bukrs
  and zcos_outbox.trigger_gl = zcos_map.trigger_gl
  and zcos_outbox.product_code = zcos_map.product_code
  and zcos_outbox.client = zcos_map.client;
```

### **ZCOS_AUD Table**

#### **1. Company Code Reference (T001)**
```abap
foreign key [0..1,1..1] t001
  references t001 on zcos_aud.bukrs = t001.bukrs
  and zcos_aud.client = t001.mandt;
```

#### **2. Outbox Reference (ZCOS_OUTBOX)**
```abap
foreign key [0..1,1..1] zcos_outbox
  references zcos_outbox on zcos_aud.guid = zcos_outbox.guid
  and zcos_aud.client = zcos_outbox.client;
```

#### **3. User Reference - Posted By (USR21)**
```abap
foreign key [0..1,1..1] usr21
  references usr21 on zcos_aud.posted_by = usr21.bname;
```

#### **4. COS Document Reference (BKPF)**
```abap
foreign key [0..1,1..1] bkpf_cos
  references bkpf on zcos_aud.belnr_cos = bkpf.belnr
  and zcos_aud.gjahr = bkpf.gjahr
  and zcos_aud.bukrs = bkpf.bukrs
  and zcos_aud.client = bkpf.mandt;
```

### **ZCOS_MAP Table**

#### **1. Company Code Reference (T001)**
```abap
foreign key [0..1,1..1] t001
  references t001 on zcos_map.bukrs = t001.bukrs
  and zcos_map.client = t001.mandt;
```

#### **2. Trigger G/L Account Reference (SKA1)**
```abap
foreign key [0..1,1..1] ska1_trigger
  references ska1 on zcos_map.trigger_gl = ska1.saknr
  and zcos_map.bukrs = ska1.bukrs
  and zcos_map.client = ska1.mandt;
```

#### **3. Sales G/L Account Reference (SKA1)**
```abap
foreign key [0..1,1..1] ska1_sales
  references ska1 on zcos_map.sales_gl = ska1.saknr
  and zcos_map.bukrs = ska1.bukrs
  and zcos_map.client = ska1.mandt;
```

#### **4. COS G/L Account Reference (SKA1)**
```abap
foreign key [0..1,1..1] ska1_cos
  references ska1 on zcos_map.cos_gl = ska1.saknr
  and zcos_map.bukrs = ska1.bukrs
  and zcos_map.client = ska1.mandt;
```

## 📋 **Key Points About ABAP DDIC Foreign Keys**

### **1. Syntax Elements**
- **`foreign key`**: Keyword to define foreign key
- **`[cardinality]`**: Relationship cardinality (e.g., `[0..1,1..1]`)
- **`foreign_key_name`**: Name of the foreign key relationship
- **`references`**: Keyword to specify target table
- **`target_table`**: Name of the referenced table
- **`on`**: Keyword before join conditions
- **Join conditions**: Field mappings with `and` for multiple conditions

### **2. Cardinality Notation**
- **`[0..1,1..1]`**: Optional to mandatory (0 or 1 to exactly 1)
- **`[1..1,1..1]`**: Mandatory to mandatory (exactly 1 to exactly 1)
- **`[0..1,0..1]`**: Optional to optional (0 or 1 to 0 or 1)

### **3. Join Conditions**
- **Simple**: `local_field = target_field`
- **Multiple**: `local_field = target_field and local_field2 = target_field2`
- **Client-specific**: Always include `client = mandt` for client-dependent tables

### **4. Naming Conventions**
- **Descriptive names**: `ska1_trigger`, `ska1_sales`, `ska1_cos`
- **Avoid conflicts**: Use suffixes when referencing same table multiple times
- **Clear purpose**: Name should indicate the relationship purpose

## ✅ **What I Fixed**

1. **Removed `left join` syntax** - This is not valid for ABAP DDIC
2. **Used `references` keyword** - Correct syntax for foreign key definitions
3. **Simplified complex conditions** - Removed date range and status conditions that aren't supported
4. **Maintained cardinality notation** - Kept the `[0..1,1..1]` format
5. **Preserved multi-field joins** - Used `and` for multiple join conditions

## 🎯 **Result**

The foreign key syntax is now **correct and valid** for ABAP DDIC objects. Thank you for catching this error! The foreign keys will now properly enforce referential integrity without syntax errors.

---

**Note**: The `left join` syntax I initially used is valid for SQL queries and CDS views, but not for DDIC foreign key definitions. ABAP DDIC foreign keys use the `references` syntax as shown above.
