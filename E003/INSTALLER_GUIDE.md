# COS Auto Posting Solution - Installer Guide

## Table of Contents
1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Installation Steps](#installation-steps)
4. [Configuration](#configuration)
5. [Testing](#testing)
6. [Troubleshooting](#troubleshooting)
7. [Post-Installation](#post-installation)

## Overview

This installer guide provides step-by-step instructions for installing the COS (Cost of Sales) Auto Posting solution in an SAP system. The solution includes:

- **Core Classes**: Feature toggle management, qRFC worker, document processor, monitor, validator, logger
- **Interfaces**: Logger and validator interfaces for dependency injection
- **Exception Classes**: Custom exception handling
- **Data Dictionary Objects**: Database tables and structures
- **Function Modules**: Wrapper functions for backward compatibility
- **BAdI Implementation**: Document processing enhancement
- **Test Classes**: Comprehensive ABAP Unit test suite
- **Message Classes**: Centralized message management
- **Reports**: Monitoring and testing programs

## Prerequisites

### System Requirements
- **SAP System**: SAP ECC 6.0 or higher, or SAP S/4HANA
- **ABAP Version**: 7.40 or higher
- **Authorization**: Development and transport authorization
- **Transport System**: Access to transport management

### Required Authorizations
- **S_DEVELOP**: Object development
- **S_TRANSPORT**: Transport management
- **S_TABU_DIS**: Table maintenance
- **S_RS_ADMIN**: Report administration
- **S_APPL_LOG**: Application log administration

### Required Tables/Structures
- `TVARVC` - Table for feature toggles
- `T001` - Company code master data
- `SKA1` - G/L account master data
- `BAL*` - Business Application Log tables

## Installation Steps

### Step 1: Create Transport Request

1. **Log into SAP system** with development authorization
2. **Open SE09** (Transport Organizer)
3. **Create new transport request**:
   - Type: `Workbench Request`
   - Description: `COS Auto Posting Solution - Initial Installation`
   - Target system: `Production` (or appropriate target)
4. **Note the transport request number** for later use

### Step 2: Install Data Dictionary Objects

#### 2.1 Database Tables

**Install in this order:**

1. **ZCOS_OUTBOX** (`ZCOS_OUTBOX.ddic`)
   - Table for outbox entries
   - Contains: GUID, company code, document data, status, etc.

2. **ZCOS_AUD** (`ZCOS_AUD.ddic`)
   - Audit table for COS processing
   - Contains: Processing history, amounts, status, etc.

3. **ZCOS_MAP** (`ZCOS_MAP.ddic`)
   - Mapping table for COS configuration
   - Contains: Company code, G/L accounts, product codes, margins

4. **ZCOS_MAP_META** (`ZCOS_MAP_META.ddic`)
   - Metadata table for mapping configuration
   - Contains: Field definitions, validation rules

#### 2.2 CDS Views

1. **ZCOS_MAP_CDS** (`ZCOS_MAP_CDS.ddic`)
   - Core Data Services view for mapping data
   - Provides enhanced data access capabilities

2. **ZCOS_MAP_BEHAVIOR** (`ZCOS_MAP_BEHAVIOR.abap`)
   - Behavior definition for CDS view
   - Enables Fiori UI for mapping maintenance

### Step 3: Install Core Classes

**Install in this order:**

#### 3.1 Interfaces
1. **ZIF_COS_LOGGER** (`ZIF_COS_LOGGER.abap`)
2. **ZIF_COS_VALIDATOR** (`ZIF_COS_VALIDATOR.abap`)

#### 3.2 Exception Classes
1. **ZCX_COS_PROCESSING_ERROR** (`ZCX_COS_PROCESSING_ERROR.abap`)

#### 3.3 Core Classes
1. **ZCL_COS_LOGGER** (`ZCL_COS_LOGGER.abap`)
2. **ZCL_COS_VALIDATOR** (`ZCL_COS_VALIDATOR.abap`)
3. **ZCL_COS_MESSAGE_UTILITY** (`ZCL_COS_MESSAGE_UTILITY.abap`)
4. **ZCL_COS_FEATURE_TOGGLE** (from `ZCOS_FEATURE_TOGGLE.abap`)
5. **ZCL_COS_QRFC_WORKER** (`ZCL_COS_QRFC_WORKER.abap`)
6. **ZCL_COS_DOCUMENT_PROCESSOR** (`ZCL_COS_DOCUMENT_PROCESSOR.abap`)
7. **ZCL_COS_MONITOR** (`ZCL_COS_MONITOR.abap`)

### Step 4: Install Function Modules

1. **Z_COS_QRFC_WORKER** (`Z_COS_QRFC_WORKER.abap`)
   - qRFC function module wrapper
   - Maintains backward compatibility

2. **ZCOS_FEATURE_TOGGLE** (from `ZCOS_FEATURE_TOGGLE.abap`)
   - Feature toggle function modules
   - Wrapper functions for class methods

### Step 5: Install BAdI Implementation

1. **ZIM_AC_DOCUMENT_COS** (`ZIM_AC_DOCUMENT_COS.abap`)
   - BAdI implementation for document processing
   - Enhances standard document posting

### Step 6: Install Message Classes

1. **ZCOS_MESSAGES_ENHANCED** (`ZCOS_MESSAGES_ENHANCED.abap`)
   - Enhanced message class with 40+ messages
   - Success, error, and warning messages

2. **ZCOS_MESSAGES** (`ZCOS_MESSAGES.abap`)
   - Original message class (if needed for backward compatibility)

### Step 7: Install Test Classes

**Install in this order:**

1. **ZCL_TC_COS_FEATURE_TOGGLE** (`ZCL_TC_COS_FEATURE_TOGGLE.abap`)
2. **ZCL_TC_COS_VALIDATOR** (`ZCL_TC_COS_VALIDATOR.abap`)
3. **ZCL_TC_COS_QRFC_WORKER** (`ZCL_TC_COS_QRFC_WORKER.abap`)
4. **ZCL_TC_COS_MONITOR** (`ZCL_TC_COS_MONITOR.abap`)
5. **ZCL_TC_COS_INTEGRATION** (`ZCL_TC_COS_INTEGRATION.abap`)

### Step 8: Install Reports

1. **ZCOS_MONITOR** (`ZCOS_MONITOR.abap`)
   - Monitoring report for COS processing
   - Provides status and error monitoring

2. **ZCOS_TEST_NEW** (`ZCOS_TEST_NEW.abap`)
   - New test runner program
   - Executes ABAP Unit tests

3. **ZCOS_TEST** (`ZCOS_TEST.abap`)
   - Legacy test program (if needed)

### Step 9: Install Authorization Objects

1. **ZCOS_AUTH_OBJECTS** (`ZCOS_AUTH_OBJECTS.abap`)
   - Custom authorization objects
   - Defines security for COS operations

## Configuration

### Step 1: Create Authorization Objects

1. **Open PFCG** (Role Maintenance)
2. **Create new role**: `ZCOS_ADMIN`
3. **Add authorization objects**:
   - `ZCOS_POST` - COS document posting
   - `ZCOS_MONITOR` - COS monitoring
   - `ZCOS_CONFIG` - COS configuration
4. **Assign to users** as needed

### Step 2: Configure Feature Toggles

1. **Execute function module**: `ZCOS_SETUP_FEATURE_TOGGLE`
2. **Set up feature toggle**:
   - Feature Name: `ZCOS_E003_ACTIVE`
   - Active: `X`
   - Description: `COS Auto Posting Active`

### Step 3: Configure COS Mapping

1. **Open table maintenance** for `ZCOS_MAP`
2. **Create mapping entries**:
   - Company Code
   - Trigger G/L Account
   - Product Code
   - Sales G/L Account
   - COS G/L Account
   - Margin Percentage

### Step 4: Configure qRFC

1. **Open SMQ1** (qRFC Administration)
2. **Create qRFC queue**: `COS_<BUKRS>_<YYYYMMDD>`
3. **Set up queue parameters**:
   - Queue Type: `Outbound`
   - Target System: `Local`
   - Processing: `Parallel`

### Step 5: Configure Application Log

1. **Open SLG1** (Application Log)
2. **Create log object**: `ZCOS`
3. **Create sub-object**: `GENERAL`
4. **Configure log levels** as needed

## Testing

### Step 1: Run ABAP Unit Tests

1. **Open SE80** (Object Navigator)
2. **Navigate to test classes**
3. **Execute ABAP Unit tests**:
   - Right-click on test class
   - Select "Execute Unit Tests"
   - Verify all tests pass

### Step 2: Run Integration Tests

1. **Execute report**: `ZCOS_TEST_NEW`
2. **Select test options**:
   - Unit Tests: `X`
   - Integration Tests: `X`
   - Feature Tests: `X`
   - qRFC Tests: `X`
   - Monitor Tests: `X`
3. **Execute and verify results**

### Step 3: Test Feature Toggle

1. **Execute function module**: `ZCOS_IS_FEATURE_ACTIVE`
2. **Input**: `ZCOS_E003_ACTIVE`
3. **Verify**: Returns `X` (active)

### Step 4: Test Document Processing

1. **Create test document** with trigger G/L account
2. **Post document** through standard process
3. **Verify**: BAdI processes document and creates outbox entry
4. **Check outbox table**: `ZCOS_OUTBOX`

### Step 5: Test qRFC Processing

1. **Execute function module**: `Z_COS_QRFC_WORKER`
2. **Input**: GUID from outbox entry
3. **Verify**: COS document created and audit entry logged

### Step 6: Test Monitoring

1. **Execute report**: `ZCOS_MONITOR`
2. **Set selection criteria**:
   - Company Code range
   - Date range
   - Status range
3. **Execute and verify data display**

## Troubleshooting

### Common Issues

#### 1. Compilation Errors
- **Issue**: Syntax errors in classes
- **Solution**: Check ABAP syntax, verify all dependencies installed
- **Check**: SE80 syntax check

#### 2. Authorization Errors
- **Issue**: User lacks required authorizations
- **Solution**: Assign proper roles and authorizations
- **Check**: SU53 (Authorization trace)

#### 3. Table Access Errors
- **Issue**: Cannot access custom tables
- **Solution**: Verify table installation and user authorizations
- **Check**: SE11 table display

#### 4. BAdI Not Triggering
- **Issue**: BAdI implementation not called
- **Solution**: Verify BAdI filter settings and implementation
- **Check**: SE18/SE19 BAdI maintenance

#### 5. qRFC Not Processing
- **Issue**: qRFC units not processed
- **Solution**: Check qRFC queue configuration and processing
- **Check**: SMQ1/SMQ2 qRFC administration

### Debugging Steps

1. **Enable debugging** in development system
2. **Set breakpoints** in key methods
3. **Execute test scenarios** step by step
4. **Check application logs** for errors
5. **Verify data consistency** in tables

### Log Analysis

1. **Application Log**: SLG1 (Object: ZCOS)
2. **System Log**: SM21
3. **qRFC Log**: SMQ2
4. **Authorization Log**: SU53

## Post-Installation

### Step 1: Transport to Production

1. **Release transport request** in development
2. **Import to production** system
3. **Verify installation** in production
4. **Run post-installation tests**

### Step 2: User Training

1. **Train administrators** on:
   - Feature toggle management
   - COS mapping configuration
   - Monitoring and troubleshooting

2. **Train end users** on:
   - Document posting process
   - Error handling and resolution

### Step 3: Documentation

1. **Update system documentation**
2. **Create user manuals**
3. **Document configuration procedures**
4. **Maintain change log**

### Step 4: Monitoring Setup

1. **Set up regular monitoring** of:
   - qRFC queue processing
   - Error rates and patterns
   - Performance metrics

2. **Configure alerts** for:
   - Processing failures
   - Queue backlogs
   - System errors

### Step 5: Maintenance

1. **Schedule regular maintenance**:
   - Clean up old audit data
   - Archive processed outbox entries
   - Review and update mappings

2. **Plan for updates**:
   - Monitor SAP releases
   - Plan for solution updates
   - Maintain backward compatibility

## Support and Maintenance

### Contact Information
- **Development Team**: [Contact Information]
- **System Administrator**: [Contact Information]
- **SAP Basis Team**: [Contact Information]

### Maintenance Schedule
- **Daily**: Monitor qRFC processing
- **Weekly**: Review error logs and performance
- **Monthly**: Clean up old data and review mappings
- **Quarterly**: Review and update authorizations

### Version Control
- **Current Version**: 1.0
- **Last Updated**: [Date]
- **Next Review**: [Date]

## Appendix

### A. File List
- [Complete list of all files in the solution]

### B. Dependencies
- [List of SAP standard objects required]

### C. Customizing Tables
- [List of customizing tables and their purposes]

### D. Authorization Matrix
- [Detailed authorization requirements by role]

### E. Performance Considerations
- [Performance tuning recommendations]

---

**Note**: This installer guide should be reviewed and updated as the solution evolves. Always test in a development environment before applying to production systems.
