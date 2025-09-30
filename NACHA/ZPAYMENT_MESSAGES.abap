*&---------------------------------------------------------------------*
*& Message Class: ZPAYMENT
*& Description: Messages for BTE 1030 Payment Processing
*&---------------------------------------------------------------------*

* Message 001: Payment run processing started
* Type: I
* Text: Payment run processing started for Laufi &1, Company &2

* Message 002: Payment run validation failed
* Type: E
* Text: Payment run validation failed: &1 for Laufi &2

* Message 003: Invoice categories found
* Type: I
* Text: Found &1 invoice categories for processing

* Message 004: Document records retrieved
* Type: I
* Text: Retrieved &1 document records

* Message 005: Invoice records retrieved
* Type: I
* Text: Retrieved &1 invoice records

* Message 006: Invoice records after filtering
* Type: I
* Text: After filtering: &1 invoice records remain

* Message 007: Processing completed successfully
* Type: S
* Text: Processing completed successfully: &1 for Laufi &2

* Message 008: Processing error
* Type: E
* Text: Processing error: &1 for Laufi &2

* Message 009: No invoice categories found
* Type: E
* Text: No invoice categories found for payment run &1

* Message 010: No document data found
* Type: E
* Text: No document data found for processing

* Message 011: No invoice data found
* Type: E
* Text: No invoice data found for processing

* Message 012: Invalid payment method
* Type: E
* Text: Invalid payment method &1 for Laufi &2
