CLASS zcl_logger DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      IMPORTING
        !iv_log_object            TYPE balobj_d
        !iv_log_subobject         TYPE balsubobj
        !iv_extnumber             TYPE balnrext
        !iv_use_multiton_pattern  TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_logger_instance) TYPE REF TO zcl_logger
      EXCEPTIONS
        log_header_inconsistent
        logging_error .
    METHODS add_error
      IMPORTING
        !iv_message TYPE char200
      EXCEPTIONS
        logging_error .
    METHODS add_info
      IMPORTING
        !iv_message TYPE char200
      EXCEPTIONS
        logging_error .
    METHODS add_msg
      IMPORTING
        !iv_msgty     TYPE sy-msgty DEFAULT 'I'
        !iv_msgid     TYPE sy-msgid DEFAULT '00'
        !iv_msgno     TYPE sy-msgno DEFAULT '398'
        !iv_msgv1     TYPE sy-msgv1 OPTIONAL
        !iv_msgv2     TYPE sy-msgv2 OPTIONAL
        !iv_msgv3     TYPE sy-msgv3 OPTIONAL
        !iv_msgv4     TYPE sy-msgv4 OPTIONAL
        !iv_probclass TYPE balprobcl OPTIONAL
      EXCEPTIONS
        logging_error .
    METHODS add_msg_long
      IMPORTING
        !iv_msgty   TYPE sy-msgty DEFAULT 'I'
        !iv_message TYPE char200
      EXCEPTIONS
        logging_error .
    METHODS add_success
      IMPORTING
        !iv_message TYPE char200
      EXCEPTIONS
        logging_error .
    METHODS add_warning
      IMPORTING
        !iv_message TYPE char200
      EXCEPTIONS
        logging_error .
    METHODS constructor
      IMPORTING
        !iv_log_object    TYPE balobj_d
        !iv_log_subobject TYPE balsubobj
        !iv_extnumber     TYPE balnrext
      EXCEPTIONS
        log_header_inconsistent
        logging_error .
    METHODS has_messages
      RETURNING
        VALUE(rv_exist) TYPE abap_bool .
    METHODS post
      IMPORTING
        !iv_in_update_task         TYPE abap_bool DEFAULT abap_false
        !iv_2nd_connection_commmit TYPE abap_bool DEFAULT abap_false
        !iv_2nd_connection         TYPE abap_bool DEFAULT abap_false
          PREFERRED PARAMETER iv_in_update_task
      EXCEPTIONS
        logging_error
        save_not_allowed
        no_messages_exist .
    METHODS show
      EXCEPTIONS
        display_error
        no_messages_exist
        logging_error .
  PROTECTED SECTION.
    METHODS get_log_messages
      RETURNING
        VALUE(rt_log_messages) TYPE bal_t_msg .
    METHODS set_log_messages
      IMPORTING
        it_log_messages TYPE bal_t_msg .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_s_logger_instance,
        log_object    TYPE        balobj_d,
        log_subobject TYPE        balsubobj,
        extnumber     TYPE        balnrext,
        logger        TYPE REF TO zcl_logger,
      END OF t_s_logger_instance .
    TYPES:
      t_t_logger_instances TYPE STANDARD TABLE OF t_s_logger_instance WITH KEY log_object log_subobject extnumber .

    DATA mv_log_handle TYPE balloghndl .
    DATA mv_log_object TYPE balobj_d .
    DATA mv_log_subobject TYPE balsubobj .
    DATA mv_extnumber TYPE balnrext .
    DATA mt_log_messages TYPE bal_t_msg .
    CLASS-DATA mt_logger_instances TYPE t_t_logger_instances .
ENDCLASS.



CLASS ZCL_LOGGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_ERROR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MESSAGE                     TYPE        CHAR200
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_error.
    add_msg_long(
       EXPORTING
         iv_msgty      = 'E'
         iv_message    = iv_message
       EXCEPTIONS
         logging_error = 1
    ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_INFO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MESSAGE                     TYPE        CHAR200
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_info.
    add_msg_long(
       EXPORTING
         iv_msgty      = 'I'
         iv_message    = iv_message
       EXCEPTIONS
         logging_error = 1
    ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_MSG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MSGTY                       TYPE        SY-MSGTY (default ='I')
* | [--->] IV_MSGID                       TYPE        SY-MSGID (default ='00')
* | [--->] IV_MSGNO                       TYPE        SY-MSGNO (default ='398')
* | [--->] IV_MSGV1                       TYPE        SY-MSGV1(optional)
* | [--->] IV_MSGV2                       TYPE        SY-MSGV2(optional)
* | [--->] IV_MSGV3                       TYPE        SY-MSGV3(optional)
* | [--->] IV_MSGV4                       TYPE        SY-MSGV4(optional)
* | [--->] IV_PROBCLASS                   TYPE        BALPROBCL(optional)
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_msg.
    DATA: ls_msg  TYPE bal_s_msg,
          lv_time TYPE timestamp.

    ls_msg-msgty = iv_msgty.
    ls_msg-msgid = iv_msgid.
    ls_msg-msgno = iv_msgno.
    ls_msg-msgv1 = iv_msgv1.
    ls_msg-msgv2 = iv_msgv2.
    ls_msg-msgv3 = iv_msgv3.
    ls_msg-msgv4 = iv_msgv4.
    ls_msg-probclass = iv_probclass.

*** JK 29.03.2016: Changed behaviour of class to bulk adding all messages
***                right before posting to solve inheritance and method
***                chained call issues.
*    CALL FUNCTION 'BAL_LOG_MSG_ADD'
*      EXPORTING
*        i_log_handle = mv_log_handle
*        i_s_msg      = ls_msg
*      EXCEPTIONS
*        OTHERS       = 9.
*    IF sy-subrc <> 0.
*      RAISE logging_error.
*    ENDIF.

    TRY.
        CALL METHOD cl_abap_tstmp=>systemtstmp_syst2utc
          EXPORTING
            syst_date = sy-datum
            syst_time = sy-uzeit
          IMPORTING
            utc_tstmp = lv_time.
      CATCH cx_parameter_invalid_range .
        RAISE logging_error.
    ENDTRY.

    ls_msg-time_stmp = lv_time.
    APPEND ls_msg TO mt_log_messages.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_MSG_LONG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MSGTY                       TYPE        SY-MSGTY (default ='I')
* | [--->] IV_MESSAGE                     TYPE        CHAR200
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_msg_long.
    DATA: lv_msgv1 TYPE sy-msgv1 VALUE space,
          lv_msgv2 TYPE sy-msgv2 VALUE space,
          lv_msgv3 TYPE sy-msgv3 VALUE space,
          lv_msgv4 TYPE sy-msgv4 VALUE space.

    lv_msgv1 = iv_message(50).
    lv_msgv2 = iv_message+50(50).
    lv_msgv3 = iv_message+100(50).
    lv_msgv4 = iv_message+150(50).

    add_msg(
      EXPORTING
        iv_msgty      = iv_msgty
        iv_msgid      = 'D3'
        iv_msgno      = '232'
        iv_msgv1      = lv_msgv1
        iv_msgv2      = lv_msgv2
        iv_msgv3      = lv_msgv3
        iv_msgv4      = lv_msgv4
      EXCEPTIONS
        logging_error = 1
   ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_SUCCESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MESSAGE                     TYPE        CHAR200
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_success.
    add_msg_long(
       EXPORTING
         iv_msgty      = 'S'
         iv_message    = iv_message
       EXCEPTIONS
         logging_error = 1
    ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->ADD_WARNING
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MESSAGE                     TYPE        CHAR200
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_warning.
    add_msg_long(
       EXPORTING
         iv_msgty      = 'W'
         iv_message    = iv_message
       EXCEPTIONS
         logging_error = 1
    ).
    IF sy-subrc EQ 1.
      RAISE logging_error.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LOG_OBJECT                  TYPE        BALOBJ_D
* | [--->] IV_LOG_SUBOBJECT               TYPE        BALSUBOBJ
* | [--->] IV_EXTNUMBER                   TYPE        BALNREXT
* | [EXC!] LOG_HEADER_INCONSISTENT
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    DATA:
      ls_log  TYPE bal_s_log,
      ls_mdef TYPE bal_s_mdef.

    ls_log-object = iv_log_object.
    ls_log-subobject = iv_log_subobject.
    ls_log-extnumber = iv_extnumber.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          RAISE log_header_inconsistent.
        WHEN OTHERS.
          RAISE logging_error.
      ENDCASE.
    ENDIF.

    ls_mdef-log_handle = mv_log_handle.

    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
      EXPORTING
        i_s_msg_defaults = ls_mdef
      EXCEPTIONS
        OTHERS           = 0.

    IF sy-subrc <> 0.
      RAISE logging_error.
    ENDIF.

    mv_log_object    = iv_log_object.
    mv_log_subobject = iv_log_subobject.
    mv_extnumber     = iv_extnumber.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_LOGGER=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LOG_OBJECT                  TYPE        BALOBJ_D
* | [--->] IV_LOG_SUBOBJECT               TYPE        BALSUBOBJ
* | [--->] IV_EXTNUMBER                   TYPE        BALNREXT
* | [--->] IV_USE_MULTITON_PATTERN        TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] RO_LOGGER_INSTANCE             TYPE REF TO ZCL_LOGGER
* | [EXC!] LOG_HEADER_INCONSISTENT
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.
    DATA: ls_instance TYPE t_s_logger_instance.
    READ TABLE mt_logger_instances WITH KEY log_object = iv_log_object log_subobject = iv_log_subobject extnumber = iv_extnumber INTO ls_instance.
    IF sy-subrc NE 0.
      ls_instance-log_object = iv_log_object.
      ls_instance-log_subobject = iv_log_subobject.
      ls_instance-extnumber = iv_extnumber.

      CREATE OBJECT ls_instance-logger
        EXPORTING
          iv_log_object           = iv_log_object
          iv_log_subobject        = iv_log_subobject
          iv_extnumber            = iv_extnumber
        EXCEPTIONS
          log_header_inconsistent = 1
          logging_error           = 2
          OTHERS                  = 3.
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            RAISE log_header_inconsistent.
          WHEN OTHERS.
            RAISE logging_error.
        ENDCASE.
      ENDIF.
      ro_logger_instance = ls_instance-logger.
      INSERT ls_instance INTO TABLE mt_logger_instances.
    ELSE.
      ro_logger_instance = ls_instance-logger.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_LOGGER->GET_LOG_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_LOG_MESSAGES                TYPE        BAL_T_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_log_messages.
    rt_log_messages = mt_log_messages.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->HAS_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_EXIST                       TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD has_messages.
    IF mt_log_messages IS NOT INITIAL.
      rv_exist = abap_true.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->POST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_IN_UPDATE_TASK              TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IV_2ND_CONNECTION_COMMMIT      TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [--->] IV_2ND_CONNECTION              TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [EXC!] LOGGING_ERROR
* | [EXC!] SAVE_NOT_ALLOWED
* | [EXC!] NO_MESSAGES_EXIST
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD post.
    IF iv_in_update_task EQ abap_true AND iv_2nd_connection EQ abap_true.
      RAISE save_not_allowed.
    ENDIF.

    IF has_messages( ) EQ abap_false.
      RAISE no_messages_exist.
    ENDIF.

    LOOP AT mt_log_messages ASSIGNING FIELD-SYMBOL(<log_message>).

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = mv_log_handle
          i_s_msg      = <log_message>
        EXCEPTIONS
          OTHERS       = 9.
      IF sy-subrc <> 0.
        RAISE logging_error.
      ENDIF.
    ENDLOOP.

    DATA lt_log_handle TYPE bal_t_logh.
    APPEND mv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task     = iv_in_update_task
        i_t_log_handle       = lt_log_handle
        i_2th_connect_commit = iv_2nd_connection_commmit
        i_2th_connection     = iv_2nd_connection
      EXCEPTIONS
        log_not_found        = 1
        save_not_allowed     = 2
        numbering_error      = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      RAISE logging_error.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_LOGGER->SET_LOG_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_LOG_MESSAGES                TYPE        BAL_T_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_log_messages.
    mt_log_messages = it_log_messages.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_LOGGER->SHOW
* +-------------------------------------------------------------------------------------------------+
* | [EXC!] DISPLAY_ERROR
* | [EXC!] NO_MESSAGES_EXIST
* | [EXC!] LOGGING_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show.
    IF has_messages( ) EQ abap_false.
      RAISE no_messages_exist.
    ENDIF.

    FIELD-SYMBOLS <log_message> LIKE LINE OF mt_log_messages.

    LOOP AT mt_log_messages ASSIGNING <log_message>.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = mv_log_handle
          i_s_msg      = <log_message>
        EXCEPTIONS
          OTHERS       = 9.
      IF sy-subrc <> 0.
        RAISE logging_error.
      ENDIF.
    ENDLOOP.

    DATA lt_log_handle TYPE bal_t_logh.

    APPEND mv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle       = lt_log_handle
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc <> 0.
      RAISE display_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.