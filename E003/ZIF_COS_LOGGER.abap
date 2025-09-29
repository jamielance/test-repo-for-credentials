*&---------------------------------------------------------------------*
*& Interface: ZIF_COS_LOGGER
*& Description: Interface for COS logging functionality
*&---------------------------------------------------------------------*
INTERFACE zif_cos_logger.
  TYPES: BEGIN OF ty_log_entry,
           severity    TYPE char1,
           message_id  TYPE msgid,
           message_no  TYPE msgno,
           message_v1  TYPE msgv1,
           message_v2  TYPE msgv2,
           message_v3  TYPE msgv3,
           message_v4  TYPE msgv4,
         END OF ty_log_entry.

  METHODS:
    log_info
      IMPORTING
        iv_message_id TYPE msgid
        iv_message_no TYPE msgno
        iv_message_v1 TYPE msgv1 OPTIONAL
        iv_message_v2 TYPE msgv2 OPTIONAL
        iv_message_v3 TYPE msgv3 OPTIONAL
        iv_message_v4 TYPE msgv4 OPTIONAL,

    log_warning
      IMPORTING
        iv_message_id TYPE msgid
        iv_message_no TYPE msgno
        iv_message_v1 TYPE msgv1 OPTIONAL
        iv_message_v2 TYPE msgv2 OPTIONAL
        iv_message_v3 TYPE msgv3 OPTIONAL
        iv_message_v4 TYPE msgv4 OPTIONAL,

    log_error
      IMPORTING
        iv_message_id TYPE msgid
        iv_message_no TYPE msgno
        iv_message_v1 TYPE msgv1 OPTIONAL
        iv_message_v2 TYPE msgv2 OPTIONAL
        iv_message_v3 TYPE msgv3 OPTIONAL
        iv_message_v4 TYPE msgv4 OPTIONAL,

    log_success
      IMPORTING
        iv_message_id TYPE msgid
        iv_message_no TYPE msgno
        iv_message_v1 TYPE msgv1 OPTIONAL
        iv_message_v2 TYPE msgv2 OPTIONAL
        iv_message_v3 TYPE msgv3 OPTIONAL
        iv_message_v4 TYPE msgv4 OPTIONAL,

    save_log
      RETURNING
        VALUE(rv_success) TYPE abap_bool.

ENDINTERFACE.

