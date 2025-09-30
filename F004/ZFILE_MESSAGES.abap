*&---------------------------------------------------------------------*
*& Message Class: ZFILE
*& Description: Messages for File Transfer Service
*&---------------------------------------------------------------------*

* Message 001: Input validation error
* Type: E
* Text: Input file or output directory is empty

* Message 002: Path validation error
* Type: E
* Text: Invalid file paths detected

* Message 003: Source file not found
* Type: E
* Text: Source file does not exist: &1

* Message 004: Directory creation failed
* Type: E
* Text: Failed to create target directory: &1

* Message 005: Operation started
* Type: I
* Text: File operation started: &1 -> &2 (Type: &3)

* Message 006: Operation successful
* Type: S
* Text: File operation completed successfully: &1 -> &2 (Type: &3)

* Message 007: Operation failed
* Type: E
* Text: File operation failed: &1 -> &2 (Type: &3)

* Message 008: System command error
* Type: E
* Text: System command error: &1 (Command: &2, Parameters: &3)

* Message 009: Audit log entry
* Type: I
* Text: File operation logged: &1 -> &2 (Type: &3, Status: &4)

* Message 010: Security violation
* Type: E
* Text: Security violation detected in file path: &1

* Message 011: Path traversal attempt
* Type: E
* Text: Path traversal attempt blocked: &1

* Message 012: File size exceeded
* Type: E
* Text: File size exceeds maximum allowed: &1 MB

* Message 013: Permission denied
* Type: E
* Text: Permission denied for file operation: &1

* Message 014: Directory not accessible
* Type: E
* Text: Directory not accessible: &1

* Message 015: File already exists
* Type: W
* Text: File already exists, overwrite not allowed: &1

* Message 016: Backup created
* Type: S
* Text: Backup file created: &1

* Message 017: Retry operation
* Type: I
* Text: Retrying file operation (Attempt &1 of &2): &3

* Message 018: Max retries reached
* Type: E
* Text: Maximum retry attempts reached for file operation: &1

* Message 019: Request created
* Type: S
* Text: File transfer request created: &1

* Message 020: Request processed
* Type: S
* Text: File transfer request processed: &1

* Message 021: Space warning
* Type: W
* Text: Directory space usage warning: &1% used in &2

* Message 022: Space critical
* Type: E
* Text: Directory space critical: &1% used in &2

* Message 023: System health check
* Type: I
* Text: File system health check completed: &1

* Message 024: Configuration error
* Type: E
* Text: File transfer configuration error: &1

* Message 025: Service unavailable
* Type: E
* Text: File transfer service temporarily unavailable
