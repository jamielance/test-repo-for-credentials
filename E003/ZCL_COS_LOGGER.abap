*&---------------------------------------------------------------------*
*& Class: ZCL_COS_LOGGER
*& Description: Implementation of COS logging functionality
*&---------------------------------------------------------------------*
CLASS zcl_cos_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Logger interface</p>
    "! <p>Implements the ZIF_COS_LOGGER interface for standardized logging</p>
    INTERFACES zif_cos_logger.

    "! <p class="shorttext synchronized">Constructor</p>
    "! <p>Creates a new logger instance with specified object and subobject.
    "! Initializes the Business Application Log (BAL) for COS processing.</p>
    "! @parameter iv_object | <p class="shorttext synchronized">Log object (default: ZCOS)</p>
    "! @parameter iv_subobject | <p class="shorttext synchronized">Log subobject (default: QRFC)</p>
    METHODS:
      constructor
        IMPORTING
          iv_object    TYPE balobj_d DEFAULT 'ZCOS'
          iv_subobject TYPE balsubobj DEFAULT 'QRFC',

      "! <p class="shorttext synchronized">Get log handle</p>
      "! <p>Returns the current log handle for external log operations
      "! and log management.</p>
      "! @parameter rv_handle | <p class="shorttext synchronized">BAL log handle</p>
      get_log_handle
        RETURNING
          VALUE(rv_handle) TYPE balloghndl.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized">Log handle</p>
    "! <p>BAL log handle for the current logging session</p>
    DATA:
      mv_log_handle TYPE balloghndl,
      "! <p class="shorttext synchronized">Log object</p>
      "! <p>BAL log object identifier</p>
      mv_object     TYPE balobj_d,
      "! <p class="shorttext synchronized">Log subobject</p>
      "! <p>BAL log subobject identifier</p>
      mv_subobject  TYPE balsubobj.

    "! <p class="shorttext synchronized">Add log message</p>
    "! <p>Adds a message to the current log with specified severity and parameters.
    "! This is the core logging method used by all interface methods.</p>
    "! @parameter iv_severity | <p class="shorttext synchronized">Message severity (I/W/E/S)</p>
    "! @parameter iv_message_id | <p class="shorttext synchronized">Message class ID</p>
    "! @parameter iv_message_no | <p class="shorttext synchronized">Message number</p>
    "! @parameter iv_message_v1 | <p class="shorttext synchronized">Message variable 1 (optional)</p>
    "! @parameter iv_message_v2 | <p class="shorttext synchronized">Message variable 2 (optional)</p>
    "! @parameter iv_message_v3 | <p class="shorttext synchronized">Message variable 3 (optional)</p>
    "! @parameter iv_message_v4 | <p class="shorttext synchronized">Message variable 4 (optional)</p>
    METHODS:
      add_log_message
        IMPORTING
          iv_severity   TYPE char1
          iv_message_id TYPE msgid
          iv_message_no TYPE msgno
          iv_message_v1 TYPE msgv1 OPTIONAL
          iv_message_v2 TYPE msgv2 OPTIONAL
          iv_message_v3 TYPE msgv3 OPTIONAL
          iv_message_v4 TYPE msgv4 OPTIONAL.

ENDCLASS.

CLASS zcl_cos_logger IMPLEMENTATION.

  METHOD constructor.
    mv_object = iv_object.
    mv_subobject = iv_subobject.

    " Create log handle
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log = VALUE bal_s_log( object = mv_object subobject = mv_subobject )
      IMPORTING
        e_log_handle = mv_log_handle.
  ENDMETHOD.

  METHOD get_log_handle.
    rv_handle = mv_log_handle.
  ENDMETHOD.

  METHOD zif_cos_logger~log_info.
    add_log_message(
      iv_severity = 'I'
      iv_message_id = iv_message_id
      iv_message_no = iv_message_no
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
    ).
  ENDMETHOD.

  METHOD zif_cos_logger~log_warning.
    add_log_message(
      iv_severity = 'W'
      iv_message_id = iv_message_id
      iv_message_no = iv_message_no
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
    ).
  ENDMETHOD.

  METHOD zif_cos_logger~log_error.
    add_log_message(
      iv_severity = 'E'
      iv_message_id = iv_message_id
      iv_message_no = iv_message_no
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
    ).
  ENDMETHOD.

  METHOD zif_cos_logger~log_success.
    add_log_message(
      iv_severity = 'S'
      iv_message_id = iv_message_id
      iv_message_no = iv_message_no
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
    ).
  ENDMETHOD.

  METHOD zif_cos_logger~save_log.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_log_handle = mv_log_handle.
    
    rv_success = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD add_log_message.
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg = VALUE bal_s_msg(
          msgty = iv_severity
          msgid = iv_message_id
          msgno = iv_message_no
          msgv1 = iv_message_v1
          msgv2 = iv_message_v2
          msgv3 = iv_message_v3
          msgv4 = iv_message_v4
        ).
  ENDMETHOD.

ENDCLASS.

