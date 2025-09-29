*&---------------------------------------------------------------------*
*& Message Class: ZHLM_INV_VALIDATION
*& Description: Messages for Invoice Validation BAdI
*& Package: ZHLM_AP_INTF
*& Transport: Development Class
*&---------------------------------------------------------------------*

* Message Class: ZHLM_INV_VALIDATION
* Description: Messages for Invoice Category Validation

* Message 001 (E): No bank rule found
* Text: No bank rule for &1/&2/&3
* Parameters: &1 = Company Code, &2 = Invoice Category, &3 = Payment Method
*----------------------------------------------------------------------*

* Message 002 (E): House Bank mismatch
* Text: House Bank &1 does not match expected &2
* Parameters: &1 = Actual House Bank, &2 = Expected House Bank
*----------------------------------------------------------------------*

* Message 003 (E): House Bank Account mismatch
* Text: House Bank Account &1 does not match expected &2
* Parameters: &1 = Actual House Bank Account, &2 = Expected House Bank Account
*----------------------------------------------------------------------*

* Message 004 (E): Partner Bank Type mismatch
* Text: Partner Bank Type &1 does not match expected &2
* Parameters: &1 = Actual Partner Bank Type, &2 = Expected Partner Bank Type
*----------------------------------------------------------------------*

* Message 005 (E): WHT Type mismatch
* Text: WHT Type &1 does not match expected &2
* Parameters: &1 = Actual WHT Type, &2 = Expected WHT Type
*----------------------------------------------------------------------*

* Message 006 (E): WHT Code mismatch
* Text: WHT Code &1 does not match expected &2
* Parameters: &1 = Actual WHT Code, &2 = Expected WHT Code
*----------------------------------------------------------------------*
