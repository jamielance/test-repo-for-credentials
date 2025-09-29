# COS Auto Posting Solution - Quick Install Guide

## 🚀 Quick Start (5 Minutes)

### 1. Prerequisites Check
```abap
" Check system version
SELECT SINGLE version FROM svers INTO @DATA(lv_version).

" Check authorizations
AUTHORITY-CHECK OBJECT 'S_DEVELOP' ID 'ACTVT' FIELD '01'.
```

### 2. Install Core Components
```abap
" Run installation script
SUBMIT zcos_installation_script.

" Or install manually:
" 1. Tables: ZCOS_OUTBOX, ZCOS_AUD, ZCOS_MAP
" 2. Classes: ZCL_COS_* classes
" 3. Functions: Z_COS_QRFC_WORKER, ZCOS_* functions
" 4. BAdI: ZIM_AC_DOCUMENT_COS
" 5. Reports: ZCOS_MONITOR, ZCOS_TEST_NEW
```

### 3. Configure System
```abap
" Setup feature toggle
CALL FUNCTION 'ZCOS_SETUP_FEATURE_TOGGLE'
  EXPORTING
    iv_feature_name = 'ZCOS_E003_ACTIVE'
    iv_is_active    = abap_true
    iv_description  = 'COS Auto Posting Active'.

" Create sample mapping
INSERT zcos_map FROM VALUE #(
  client = sy-mandt
  bukrs = '1000'
  trigger_gl = '400000'
  product_code = 'SAMPLE'
  sales_gl = '500000'
  cos_gl = '600000'
  margin_pct = '10.00'
  valid_from = sy-datum
  valid_to = '99991231'
  created_by = sy-uname
  created_at = sy-datum
).
```

### 4. Test Installation
```abap
" Run test suite
SUBMIT zcos_test_new.

" Check feature toggle
CALL FUNCTION 'ZCOS_IS_FEATURE_ACTIVE'
  EXPORTING
    iv_feature_name = 'ZCOS_E003_ACTIVE'
  IMPORTING
    rv_is_active = DATA(lv_active).

" Verify tables exist
SELECT COUNT(*) FROM zcos_outbox.
SELECT COUNT(*) FROM zcos_audit.
SELECT COUNT(*) FROM zcos_map.
```

## 📋 Installation Order

### Phase 1: Data Dictionary
1. `ZCOS_OUTBOX` (table)
2. `ZCOS_AUD` (table)  
3. `ZCOS_MAP` (table)
4. `ZCOS_MAP_META` (table)
5. `ZCOS_MAP_CDS` (CDS view)

### Phase 2: Core Classes
1. `ZIF_COS_LOGGER` (interface)
2. `ZIF_COS_VALIDATOR` (interface)
3. `ZCX_COS_PROCESSING_ERROR` (exception)
4. `ZCL_COS_LOGGER` (class)
5. `ZCL_COS_VALIDATOR` (class)
6. `ZCL_COS_MESSAGE_UTILITY` (class)
7. `ZCL_COS_FEATURE_TOGGLE` (class)
8. `ZCL_COS_QRFC_WORKER` (class)
9. `ZCL_COS_DOCUMENT_PROCESSOR` (class)
10. `ZCL_COS_MONITOR` (class)

### Phase 3: Function Modules
1. `Z_COS_QRFC_WORKER`
2. `ZCOS_SETUP_FEATURE_TOGGLE`
3. `ZCOS_DEACTIVATE_FEATURE`
4. `ZCOS_IS_FEATURE_ACTIVE`

### Phase 4: BAdI & Messages
1. `ZIM_AC_DOCUMENT_COS` (BAdI)
2. `ZCOS_MESSAGES_ENHANCED` (messages)

### Phase 5: Test Classes
1. `ZCL_TC_COS_FEATURE_TOGGLE`
2. `ZCL_TC_COS_VALIDATOR`
3. `ZCL_TC_COS_QRFC_WORKER`
4. `ZCL_TC_COS_MONITOR`
5. `ZCL_TC_COS_INTEGRATION`

### Phase 6: Reports
1. `ZCOS_MONITOR`
2. `ZCOS_TEST_NEW`

## ⚡ Quick Commands

### Check Installation Status
```abap
" Check all components
SELECT COUNT(*) FROM zcos_outbox.
SELECT COUNT(*) FROM zcos_audit.
SELECT COUNT(*) FROM zcos_map.

" Check classes exist
SELECT SINGLE clsname FROM seoclass WHERE clsname = 'ZCL_COS_FEATURE_TOGGLE'.
SELECT SINGLE clsname FROM seoclass WHERE clsname = 'ZCL_COS_QRFC_WORKER'.

" Check functions exist
SELECT SINGLE funcname FROM tfdir WHERE funcname = 'Z_COS_QRFC_WORKER'.
SELECT SINGLE funcname FROM tfdir WHERE funcname = 'ZCOS_SETUP_FEATURE_TOGGLE'.

" Check reports exist
SELECT SINGLE progname FROM trdir WHERE progname = 'ZCOS_MONITOR'.
SELECT SINGLE progname FROM trdir WHERE progname = 'ZCOS_TEST_NEW'.
```

### Test Core Functionality
```abap
" Test feature toggle
CALL FUNCTION 'ZCOS_IS_FEATURE_ACTIVE'
  EXPORTING
    iv_feature_name = 'ZCOS_E003_ACTIVE'
  IMPORTING
    rv_is_active = DATA(lv_active).

" Test qRFC worker
CALL FUNCTION 'Z_COS_QRFC_WORKER'
  EXPORTING
    iv_guid = '1234567890123456'
    iv_bukrs = '1000'
    iv_gjahr = '2024'
    iv_belnr = '0000000001'.

" Test monitor
SUBMIT zcos_monitor.
```

### Run All Tests
```abap
" Run comprehensive test suite
SUBMIT zcos_test_new.

" Run individual test classes
" (Use SE80 or ADT to run ABAP Unit tests)
```

## 🔧 Configuration

### Required Authorizations
- `S_DEVELOP` - Object development
- `S_TRANSPORT` - Transport management  
- `S_TABU_DIS` - Table maintenance
- `S_RS_ADMIN` - Report administration
- `S_APPL_LOG` - Application log

### Custom Authorization Objects
- `ZCOS_POST` - COS document posting
- `ZCOS_MONITOR` - COS monitoring
- `ZCOS_CONFIG` - COS configuration

### Feature Toggle Setup
```abap
" Activate feature
CALL FUNCTION 'ZCOS_SETUP_FEATURE_TOGGLE'
  EXPORTING
    iv_feature_name = 'ZCOS_E003_ACTIVE'
    iv_is_active = abap_true
    iv_description = 'COS Auto Posting Active'.

" Check status
CALL FUNCTION 'ZCOS_IS_FEATURE_ACTIVE'
  EXPORTING
    iv_feature_name = 'ZCOS_E003_ACTIVE'
  IMPORTING
    rv_is_active = DATA(lv_active).
```

### qRFC Configuration
```abap
" Create qRFC queue (SMQ1)
" Queue Name: COS_<BUKRS>_<YYYYMMDD>
" Type: Outbound
" Target: Local
```

## 🐛 Troubleshooting

### Common Issues

#### 1. Compilation Errors
- Check all dependencies installed
- Verify syntax in SE80
- Check transport status

#### 2. Authorization Errors
- Assign proper roles
- Check SU53 for details
- Verify custom auth objects

#### 3. Table Access Errors
- Check table installation
- Verify user authorizations
- Check table maintenance settings

#### 4. BAdI Not Triggering
- Check BAdI filter settings
- Verify implementation status
- Check SE18/SE19

#### 5. qRFC Not Processing
- Check queue configuration
- Verify processing status
- Check SMQ1/SMQ2

### Debug Steps
1. Enable debugging
2. Set breakpoints in key methods
3. Execute test scenarios
4. Check application logs
5. Verify data consistency

## 📊 Monitoring

### Key Reports
- `ZCOS_MONITOR` - Main monitoring report
- `ZCOS_TEST_NEW` - Test execution
- `SLG1` - Application logs (Object: ZCOS)

### Key Tables
- `ZCOS_OUTBOX` - Processing queue
- `ZCOS_AUD` - Audit trail
- `ZCOS_MAP` - Configuration

### Key Functions
- `Z_COS_QRFC_WORKER` - Main processing
- `ZCOS_SETUP_FEATURE_TOGGLE` - Configuration
- `ZCOS_IS_FEATURE_ACTIVE` - Status check

## 📞 Support

### Quick Help
- Check application logs: `SLG1`
- Check system logs: `SM21`
- Check qRFC logs: `SMQ2`
- Check authorization: `SU53`

### Documentation
- `INSTALLER_GUIDE.md` - Detailed installation
- `INSTALLATION_CHECKLIST.md` - Step-by-step checklist
- `README_REFACTORED_SOLUTION.md` - Solution overview

---

**Note**: This is a quick reference guide. For detailed installation instructions, see `INSTALLER_GUIDE.md`.
