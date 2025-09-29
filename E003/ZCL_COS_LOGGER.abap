*&---------------------------------------------------------------------*
*& Class: ZCL_COS_LOGGER
*& Description: Implementation of COS logging functionality
*&---------------------------------------------------------------------*
CLASS zcl_cos_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_cos_logger.

    METHODS:
      constructor
        IMPORTING
          iv_object    TYPE balobj_d DEFAULT 'ZCOS'
          iv_subobject TYPE balsubobj DEFAULT 'QRFC',

      get_log_handle
        RETURNING
          VALUE(rv_handle) TYPE balloghndl.

  PRIVATE SECTION.
    DATA:
      mv_log_handle TYPE balloghndl,
      mv_object     TYPE balobj_d,
      mv_subobject  TYPE balsubobj.

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

