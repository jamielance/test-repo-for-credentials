*&---------------------------------------------------------------------*
*& Interface: ZIF_COS_LOGGER
*& Description: Interface for COS logging functionality
*&---------------------------------------------------------------------*
INTERFACE zif_cos_logger.
  "! <p class="shorttext synchronized">Log entry structure</p>
  "! <p>Contains all information needed for a log entry including
  "! severity, message details, and variable parameters.</p>
  TYPES: BEGIN OF ty_log_entry,
           "! <p class="shorttext synchronized">Severity level</p>
           "! <p>Message severity (I/W/E/S)</p>
           severity    TYPE char1,
           "! <p class="shorttext synchronized">Message class ID</p>
           "! <p>Message class identifier</p>
           message_id  TYPE msgid,
           "! <p class="shorttext synchronized">Message number</p>
           "! <p>Message number within the class</p>
           message_no  TYPE msgno,
           "! <p class="shorttext synchronized">Message variable 1</p>
           "! <p>First message variable</p>
           message_v1  TYPE msgv1,
           "! <p class="shorttext synchronized">Message variable 2</p>
           "! <p>Second message variable</p>
           message_v2  TYPE msgv2,
           "! <p class="shorttext synchronized">Message variable 3</p>
           "! <p>Third message variable</p>
           message_v3  TYPE msgv3,
           "! <p class="shorttext synchronized">Message variable 4</p>
           "! <p>Fourth message variable</p>
           message_v4  TYPE msgv4,
         END OF ty_log_entry.

  "! <p class="shorttext synchronized">Log information message</p>
  "! <p>Logs an informational message to the application log.
  "! Used for general information and process flow tracking.</p>
  "! @parameter iv_message_id | <p class="shorttext synchronized">Message class ID</p>
  "! @parameter iv_message_no | <p class="shorttext synchronized">Message number</p>
  "! @parameter iv_message_v1 | <p class="shorttext synchronized">Message variable 1 (optional)</p>
  "! @parameter iv_message_v2 | <p class="shorttext synchronized">Message variable 2 (optional)</p>
  "! @parameter iv_message_v3 | <p class="shorttext synchronized">Message variable 3 (optional)</p>
  "! @parameter iv_message_v4 | <p class="shorttext synchronized">Message variable 4 (optional)</p>
  METHODS:
    log_info
      IMPORTING
        iv_message_id TYPE msgid
        iv_message_no TYPE msgno
        iv_message_v1 TYPE msgv1 OPTIONAL
        iv_message_v2 TYPE msgv2 OPTIONAL
        iv_message_v3 TYPE msgv3 OPTIONAL
        iv_message_v4 TYPE msgv4 OPTIONAL,

    "! <p class="shorttext synchronized">Log warning message</p>
    "! <p>Logs a warning message to the application log.
    "! Used for non-critical issues that should be noted.</p>
    "! @parameter iv_message_id | <p class="shorttext synchronized">Message class ID</p>
    "! @parameter iv_message_no | <p class="shorttext synchronized">Message number</p>
    "! @parameter iv_message_v1 | <p class="shorttext synchronized">Message variable 1 (optional)</p>
    "! @parameter iv_message_v2 | <p class="shorttext synchronized">Message variable 2 (optional)</p>
    "! @parameter iv_message_v3 | <p class="shorttext synchronized">Message variable 3 (optional)</p>
    "! @parameter iv_message_v4 | <p class="shorttext synchronized">Message variable 4 (optional)</p>
    log_warning
      IMPORTING
        iv_message_id TYPE msgid
        iv_message_no TYPE msgno
        iv_message_v1 TYPE msgv1 OPTIONAL
        iv_message_v2 TYPE msgv2 OPTIONAL
        iv_message_v3 TYPE msgv3 OPTIONAL
        iv_message_v4 TYPE msgv4 OPTIONAL,

    "! <p class="shorttext synchronized">Log error message</p>
    "! <p>Logs an error message to the application log.
    "! Used for critical errors that prevent processing.</p>
    "! @parameter iv_message_id | <p class="shorttext synchronized">Message class ID</p>
    "! @parameter iv_message_no | <p class="shorttext synchronized">Message number</p>
    "! @parameter iv_message_v1 | <p class="shorttext synchronized">Message variable 1 (optional)</p>
    "! @parameter iv_message_v2 | <p class="shorttext synchronized">Message variable 2 (optional)</p>
    "! @parameter iv_message_v3 | <p class="shorttext synchronized">Message variable 3 (optional)</p>
    "! @parameter iv_message_v4 | <p class="shorttext synchronized">Message variable 4 (optional)</p>
    log_error
      IMPORTING
        iv_message_id TYPE msgid
        iv_message_no TYPE msgno
        iv_message_v1 TYPE msgv1 OPTIONAL
        iv_message_v2 TYPE msgv2 OPTIONAL
        iv_message_v3 TYPE msgv3 OPTIONAL
        iv_message_v4 TYPE msgv4 OPTIONAL,

    "! <p class="shorttext synchronized">Log success message</p>
    "! <p>Logs a success message to the application log.
    "! Used for successful operations and completions.</p>
    "! @parameter iv_message_id | <p class="shorttext synchronized">Message class ID</p>
    "! @parameter iv_message_no | <p class="shorttext synchronized">Message number</p>
    "! @parameter iv_message_v1 | <p class="shorttext synchronized">Message variable 1 (optional)</p>
    "! @parameter iv_message_v2 | <p class="shorttext synchronized">Message variable 2 (optional)</p>
    "! @parameter iv_message_v3 | <p class="shorttext synchronized">Message variable 3 (optional)</p>
    "! @parameter iv_message_v4 | <p class="shorttext synchronized">Message variable 4 (optional)</p>
    log_success
      IMPORTING
        iv_message_id TYPE msgid
        iv_message_no TYPE msgno
        iv_message_v1 TYPE msgv1 OPTIONAL
        iv_message_v2 TYPE msgv2 OPTIONAL
        iv_message_v3 TYPE msgv3 OPTIONAL
        iv_message_v4 TYPE msgv4 OPTIONAL,

    "! <p class="shorttext synchronized">Save log</p>
    "! <p>Saves the current log to the database and returns success status.
    "! This method persists all logged messages.</p>
    "! @parameter rv_success | <p class="shorttext synchronized">True if log saved successfully</p>
    save_log
      RETURNING
        VALUE(rv_success) TYPE abap_bool.

ENDINTERFACE.

