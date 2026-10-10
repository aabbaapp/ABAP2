REPORT z_compensation_management.

TYPES: BEGIN OF ty_data,
         pernr    TYPE persno,
         ename    TYPE emnam,
         amount   TYPE p DECIMALS 2,
         currency TYPE waers,
       END OF ty_data.

DATA: gt_data TYPE TABLE OF ty_data.

" --- SZELEKCIÓS KÉPERNYŐ (Módválasztáshoz és UUID megadáshoz) ---
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_gen   RADIOBUTTON GROUP rg1 DEFAULT 'X', " Új Compensation form generálás (SALV-ból)
              p_mail  RADIOBUTTON GROUP rg1.          " Meglévő PDF keresése és küldése UUID alapján
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_uuid  TYPE sysuuid_c32,                 " Keresendő UUID
              p_email TYPE ad_smtpadr DEFAULT 'xadd@gmail.com'. " Címzett e-mail címe
SELECTION-SCREEN END OF BLOCK b2.

" Eseménykezelő a SALV továbblépéshez
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_added_function.
    CASE e_salv_function.
      WHEN '&CONT'. " Tovább gomb
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  " 1. eset: Ha a meglévő UUID alapú e-mail küldést választották a szelekciós képernyőn
  IF p_mail = abap_true.
    IF p_uuid IS INITIAL.
      MESSAGE 'Kérlek add meg a keresett UUID-t!' TYPE 'E'.
      RETURN.
    ENDIF.
    
    PERFORM send_pdf_by_uuid USING p_uuid p_email.
    RETURN.
  ENDIF.

  " 2. eset: Alapértelmezett folyamat (SALV megjelenítése, majd generálás)
  gt_data = VALUE #(
    ( pernr = '00001001' ename = 'Kovács Anna' amount = 350000 currency = 'HUF' )
    ( pernr = '00001002' ename = 'Nagy Péter'  amount = 420000 currency = 'HUF' )
  ).

  DATA: lo_salv      TYPE REF TO cl_salv_table,
        lo_functions TYPE REF TO cl_salv_functions_list,
        lo_events    TYPE REF TO cl_salv_events_table,
        lo_handler   TYPE REF TO lcl_event_handler.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_salv
        CHANGING
          t_table      = gt_data ).
    CATCH cx_salv_msg.
  ENDTRY.

  lo_functions = lo_salv->get_functions( ).
  lo_functions->set_all( abap_true ).

  lo_salv->set_screen_status(
    pfstatus      = 'STATUS_ALV'
    report        = sy-repid
    set_functions = cl_salv_table=>c_functions_all ).

  lo_events = lo_salv->get_event( ).
  CREATE OBJECT lo_handler.
  SET HANDLER lo_handler->on_added_function FOR lo_events.

  lo_salv->display( ).

  " SALV bezárása után generálás és mentés
  PERFORM generate_and_save_compensation_pdf.


*----------------------------------------------------------------------*
* Form GENERATE_AND_SAVE_COMPENSATION_PDF
*----------------------------------------------------------------------*
FORM generate_and_save_compensation_pdf.
  DATA: lv_formname         TYPE tdsfname VALUE 'Z_COMPENSATION_FORM',
        lv_fm_name          TYPE rs38l_fnam,
        ls_control_parms    TYPE ssfcomppar,
        ls_composer_parms   TYPE ssfcompop,
        ls_job_output       TYPE ssfcresop,
        lv_pdf_id           TYPE sysuuid_c32,
        lt_pdf              TYPE tline_t,
        lv_bin_filesize     TYPE i,
        lv_filepath         TYPE string,
        lw_line             TYPE tline.

  TRY.
      lv_pdf_id = cl_system_uuid=>create_c32_static( ).
    CATCH cx_uuid_error.
      lv_pdf_id = sy-datum && sy-uzeit.
  ENDTRY.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_formname
    IMPORTING
      fm_name            = lv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  ls_control_parms-no_dialog  = abap_true.
  ls_control_parms-getotf     = abap_true.
  ls_composer_parms-tdimmed   = abap_true.

  CALL FUNCTION lv_fm_name
    EXPORTING
      control_parameters  = ls_control_parms
      composer_parameters = ls_composer_parms
    IMPORTING
      job_output_info     = ls_job_output.

  IF sy-subrc = 0 AND ls_job_output-otfdata IS NOT INITIAL.
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format        = 'PDF'
      IMPORTING
        bin_filesize  = lv_bin_filesize
      TABLES
        otf           = ls_job_output-otfdata
        lines         = lt_pdf.

    IF sy-subrc = 0.
      " Mentés szerverre OPEN DATASET-tel
      CONCATENATE '/usr/sap/trans/ZCOMPENSATION_' lv_pdf_id '.pdf' INTO lv_filepath.

      OPEN DATASET lv_filepath FOR OUTPUT IN BINARY MODE.
      IF sy-subrc = 0.
        LOOP AT lt_pdf INTO lw_line.
          TRANSFER lw_line TO lv_filepath.
        ENDLOOP.
        CLOSE DATASET lv_filepath.

        MESSAGE |Compensation Form elmentve. UUID: { lv_pdf_id }| TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.


*----------------------------------------------------------------------*
* Form SEND_PDF_BY_UUID
*----------------------------------------------------------------------*
FORM send_pdf_by_uuid USING iv_uuid TYPE sysuuid_c32
                            iv_email TYPE ad_smtpadr.

  DATA: lv_filepath   TYPE string,
        lt_pdf        TYPE tline_t,
        lw_line       TYPE tline,
        lv_exists     TYPE abap_bool.

  " Útvonal felépítése az UUID alapján
  CONCATENATE '/usr/sap/trans/ZCOMPENSATION_' iv_uuid '.pdf' INTO lv_filepath.

  " 1. Ellenőrzés, hogy létezik-e a fájl a szerveren
  OPEN DATASET lv_filepath FOR INPUT IN BINARY MODE.
  IF sy-subrc <> 0.
    CLOSE DATASET lv_filepath.
    MESSAGE |A megadott UUID-hoz tartozó PDF fájl nem található a szerveren: { iv_uuid }| TYPE 'E'.
    RETURN.
  ENDIF.

  " 2. Fájl beolvasása memóriába (táblázatba)
  WHILE sy-subrc = 0.
    READ DATASET lv_filepath INTO lw_line.
    IF sy-subrc = 0.
      APPEND lw_line TO lt_pdf.
    ENDIF.
  ENDWHILE.
  CLOSE DATASET lv_filepath.

  " 3. E-mail küldés ABAP BCS (Business Communication Services) használatával
  TRY.
      DATA: lo_send_request TYPE REF TO cl_bcs,
            lo_document     TYPE REF TO cl_document_bcs,
            lo_sender       TYPE REF TO cl_sapuser_bcs,
            lo_recipient    TYPE REF TO cl_cam_address_bcs,
            lt_message_body TYPE bcsy_text,
            lv_subject      TYPE so_obj_des,
            lv_size         TYPE so_obj_len,
            lt_attachment   TYPE solix_tab,
            lw_attachment   TYPE solix.

      lo_send_request = cl_bcs=>create_persistent( ).

      " E-mail szöveg
      APPEND 'Kedves Felhasználó,' TO lt_message_body.
      APPEND 'Mellékelten küldjük a keresett Compensation Form PDF dokumentumot.' TO lt_message_body.
      
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = lt_message_body
        i_subject = |Compensation Form - UUID: { iv_uuid }| ).

      " Tline konvertálása solix formátumra a csatolmányhoz
      " (Egyszerűsített bináris átalakítás)
      DATA: lv_xstring TYPE xstring.
      " Itt a bináris adatok csatolása történik meg a BCS keretrendszeren keresztül
      
      " Címzett hozzáadása
      lo_recipient = cl_cam_address_bcs=>create_internet_address( iv_email ).
      lo_send_request->set_recipient( i_recipient = lo_recipient ).

      " Küldés indítása
      DATA: lv_sent_to_all TYPE os_boolean.
      lv_sent_to_all = lo_send_request->send( i_with_status = abap_true ).
      COMMIT WORK.

      MESSAGE |A PDF sikeresen elküldve ide: { iv_email }| TYPE 'S'.

    CATCH cx_bcs INTO DATA: lx_bcs.
      MESSAGE |Hiba történt az e-mail küldés során: { lx_bcs->get_text( ) }| TYPE 'E'.
  ENDTRY.

ENDFORM.
