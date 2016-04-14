* The following example is for normal usage of the logger class...
* First instantiate the logger with your log (sub)objects from SLG0...
  DATA(lo_logger) = zcl_logger=>get_instance(
                      iv_log_object           = 'ZTEST'
                      iv_log_subobject        = 'EMAILS'
                      iv_extnumber            = |{ lv_vbeln }|
                  ).

* This is how you add messages (taken from a report of mine)
*....Data Definition, Instantiation etc...

    WHILE lo_repl_iterator->has_more_records( ) EQ abap_true.
    DATA(lr_repl) = lo_repl_iterator->get_next_record( ).
    lo_logger->add_info( |Starting to process IDOC { lr_repl->docnum ALPHA = OUT WIDTH = 1 } for CR { lr_repl->crequest ALPHA = OUT WIDTH = 1 }.| ).
    TRY.
        DATA(lr_edidc) = lo_edidc->get_single_record( iv_docnum = lr_repl->docnum ).
      CATCH lcx_dao_record_read_error.
        lo_logger->add_error( |No entry found in EDIDC for this IDOC.| ).
        CONTINUE.
    ENDTRY.
    lo_logger->add_info( |Partner System is { lr_edidc->rcvprn }. Current IDOC Status is { lr_edidc->status ALPHA = OUT WIDTH = 1 }.| ).
* ...
	ENDWHILE.
* And so on...
* ...once you are done either show the log to the user or post it to the database for later display in SLG1
IF sy-batch EQ abap_false AND lo_logger->has_messages( ) EQ abap_true.
	lo_logger->show( ).
ELSEIF sy-batch EQ abap_true AND lo_logger->has_messages( ) EQ abap_true.
	lo_logger->post( ).
ENDIF

* Here is an example why the instantiation of the logger was meade protected, not private...
* you can extend the original class to suit your reports special requirements...
* Lets say in your report you are using an exception:

CLASS lcx_generic_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA: gv_error_message TYPE char200 READ-ONLY.
    METHODS constructor
      IMPORTING
        iv_textid        LIKE textid OPTIONAL
        io_previous      LIKE previous OPTIONAL
        iv_error_message LIKE gv_error_message OPTIONAL.
ENDCLASS.

CLASS lcx_generic_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      EXPORTING
        textid   = iv_textid
        previous = io_previous
    ).
    IF iv_error_message IS SUPPLIED AND iv_error_message IS NOT INITIAL.
      gv_error_message = iv_error_message.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

* ... and you want the logger to be able to process this exception directly, you can do it like this...

CLASS lcl_logger DEFINITION INHERITING FROM zcl_logger CREATE PUBLIC FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_log_object    TYPE  balobj_d
        iv_log_subobject TYPE  balsubobj OPTIONAL
        iv_extnumber     TYPE  balnrext.
    METHODS add_exception
      IMPORTING io_exception TYPE REF TO lcx_generic_error.
    METHODS log.
ENDCLASS.

CLASS lcl_logger IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      EXPORTING
        iv_log_object           = iv_log_object
        iv_log_subobject        = iv_log_subobject
        iv_extnumber            = iv_extnumber
      EXCEPTIONS
        log_header_inconsistent = 1
        logging_error           = 2
        OTHERS                  = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE 'Error initializing application log.' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
  METHOD add_exception.
    IF io_exception->previous IS NOT INITIAL.
      add_error( |{ io_exception->previous->get_longtext( ) WIDTH = 200 }| ).
    ELSEIF io_exception->gv_error_message IS NOT INITIAL.
      add_error( |{ io_exception->gv_error_message WIDTH = 200 }| ).
    ELSE.
      add_error( 'An unspecific error occured.' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*... Now the local logger implementation can be instantiated like this..
  DATA(lo_logger) = NEW lcl_logger(
                      iv_log_object           = 'ZTEST'
                      iv_log_subobject        = 'EMAILS'
                      iv_extnumber            = |{ lv_vbeln }|
                  ).
* . Here is an example how to log exceptions...
      TRY.
          DATA(lo_mail) = NEW lcl_mail(
              is_crequest  = lr_crequest->*
              is_cust      = ls_cust
              is_edidc     = lr_edidc->*
              ).
        CATCH lcx_generic_error INTO data(lx_error).
          lo_logger->add_exception( lx_error ).
      ENDTRY.


