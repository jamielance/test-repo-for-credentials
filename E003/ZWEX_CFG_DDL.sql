--&---------------------------------------------------------------------*
--& DDL SQL for ZWEX_CFG Table
--& Purpose: Configuration table for WEX reconciliation
--& Namespace: ZHLM
--& Package: ZHLM_AP_WEX
--&---------------------------------------------------------------------*

CREATE TABLE ZWEX_CFG (
    MANDT           CHAR(3) NOT NULL,
    BUKRS           CHAR(4) NOT NULL,
    AMT_TOLERANCE   CURR(13,2),
    CURRENCY        CHAR(5),
    ACTIVE          CHAR(1),
    ALLOW_INTERNAL_UNBLOCK CHAR(1),
    UPDATED_AT      TIMESTAMPL,
    PRIMARY KEY (MANDT, BUKRS)
) 
WITH PARAMETERS TABLE_CATEGORY = 'TRANSPARENT',
     DATA_CLASS = 'APPL0',
     BUFFERING = 'SINGLE_RECORD',
     CHANGE_DOCUMENT = 'X';

-- Performance note: Consider secondary index on (MANDT, BUKRS, ACTIVE)
-- for efficient lookups during reconciliation process
