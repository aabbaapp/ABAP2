*&---------------------------------------------------------------------*
*& Report ZFI_DOUBLE_COMP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFI_DOUBLE_COMP.


*&---------------------------------------------------------------------*
*& Konstansok (ide kell beírni a T008 szerinti zárolási kulcsokat)
*&---------------------------------------------------------------------*
CONSTANTS: gc_blart   TYPE blart  VALUE 'AB',
           gc_zlspr_d TYPE dzlspr VALUE 'K',          " új fizetési zárolás a VEVŐTÉTELEKEN
           gc_zlspr_k TYPE dzlspr VALUE IS INITIAL.   " szállítói tételeken: üres = nem nyúlunk hozzá
CONSTANTS: gc_mail_to TYPE ad_smtpadr VALUE 'mpaxian@molitdgbs.com',   " értesítő levél címzettje (teszt)
           gc_pdf_dir TYPE string     VALUE '/tmp/',                   " test directory on the app server (later a logical file name)
           gc_mail_test_mode TYPE abap_bool VALUE abap_true.            " X = all mails go to gc_mail_to; abap_false = partner e-mail (ADR6)

*&---------------------------------------------------------------*
*& 1000-es szelekciós képernyő
*&---------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_c2v RADIOBUTTON GROUP g1 DEFAULT 'X',   " vevő -> szállító
            p_v2c RADIOBUTTON GROUP g1.               " szállító -> vevő
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
PARAMETERS: p_kunnr TYPE kna1-kunnr OBLIGATORY,
            p_lifnr TYPE lfa1-lifnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
PARAMETERS: p_date TYPE sy-datum OBLIGATORY DEFAULT sy-datum.
PARAMETERS: p_text TYPE char50.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF SCREEN 500 AS WINDOW.
SELECTION-SCREEN BEGIN OF BLOCK sel2.
PARAMETERS: p_bukrs2 TYPE bukrs.
PARAMETERS: p_compno TYPE c LENGTH 10.
PARAMETERS: p_lifnr2 TYPE lifnr.
PARAMETERS: p_datum  TYPE dats.
PARAMETERS: p_wrbtr  TYPE wrbtr.
PARAMETERS: p_sgtxt  TYPE sgtxt.
SELECTION-SCREEN END OF BLOCK sel2.
SELECTION-SCREEN END OF SCREEN 500.

*&---------------------------------------------------------------*
*& Típusok
*&---------------------------------------------------------------*
TYPES: BEGIN OF ty_out,
         text   TYPE char40,
         amount TYPE p LENGTH 13 DECIMALS 2,
         waers  TYPE waers,
         color  TYPE lvc_t_scol,
       END OF ty_out.

" Overview képernyő: fejsor
TYPES: BEGIN OF ty_prop_hdr,
         bukrs  TYPE bukrs,
         compno TYPE c LENGTH 10,
         lifnr  TYPE lifnr,
         kunnr  TYPE kunnr,
         datum  TYPE datum,
         amount TYPE p LENGTH 13 DECIMALS 2,
         waers  TYPE waers,
         text   TYPE char50,
         color  TYPE lvc_t_scol,
       END OF ty_prop_hdr.

" Overview képernyő: tételek (vevői + szállítói egy közös táblában)
TYPES: BEGIN OF ty_prop_item,
         bukrs  TYPE bukrs,
         compno TYPE c LENGTH 10,
         cnt    TYPE i,
         lifnr  TYPE lifnr,
         kunnr  TYPE kunnr,
         zuonr  TYPE dzuonr,
         amount TYPE p LENGTH 13 DECIMALS 2,
         gjahr  TYPE gjahr,
         belnr  TYPE belnr_d,
         buzei  TYPE buzei,
         augdt  TYPE augdt,
         augbl  TYPE augbl,
         zfbdt  TYPE dzfbdt,
         shkzg  TYPE shkzg,
         hkont  TYPE hkont,
         xref3  TYPE xref3,
         color  TYPE lvc_t_scol,
       END OF ty_prop_item.

TYPES: BEGIN OF ty_bsid_alv.
    INCLUDE TYPE bsid.
TYPES: cmp_stat TYPE c LENGTH 1,          " G = egyező, Y = nem kell
       color    TYPE lvc_t_scol,
       END OF ty_bsid_alv.

TYPES: BEGIN OF ty_bsik_alv.
    INCLUDE TYPE bsik.
TYPES: cmp_stat TYPE c LENGTH 1,
       color    TYPE lvc_t_scol,
       END OF ty_bsik_alv.

TYPES: tt_bsid   TYPE STANDARD TABLE OF ty_bsid_alv WITH DEFAULT KEY,
       tt_bsik   TYPE STANDARD TABLE OF ty_bsik_alv WITH DEFAULT KEY,
       tt_bsid_p TYPE STANDARD TABLE OF bsid WITH DEFAULT KEY,   " sima, Smartformhoz
       tt_bsik_p TYPE STANDARD TABLE OF bsik WITH DEFAULT KEY.

TYPES: BEGIN OF ty_dockey,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
       END OF ty_dockey.

TYPES: BEGIN OF ty_symbol,
         name  TYPE string,
         value TYPE string,
       END OF ty_symbol,
       tt_symbol TYPE STANDARD TABLE OF ty_symbol WITH DEFAULT KEY,
       tt_tline  TYPE STANDARD TABLE OF tline WITH DEFAULT KEY.

TYPES: BEGIN OF ty_posted,
         side      TYPE c LENGTH 1,      " C = customer, V = vendor
         belnr     TYPE belnr_d,
         gjahr     TYPE gjahr,
         buzei     TYPE buzei,
         shkzg     TYPE shkzg,
         wrbtr     TYPE wrbtr,
         waers     TYPE waers,
         clr_belnr TYPE belnr_d,         " clearing document
         clr_gjahr TYPE gjahr,
       END OF ty_posted,
       tt_posted TYPE STANDARD TABLE OF ty_posted WITH DEFAULT KEY.

TYPES: BEGIN OF ty_amt,
         idx   TYPE i,
         cents TYPE int8,
         prio  TYPE i,              " 0 = azonos bizonylaton lévő ellenoldali sor (előnyben)
       END OF ty_amt,
       tt_amt TYPE STANDARD TABLE OF ty_amt WITH DEFAULT KEY.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

*&---------------------------------------------------------------*
*& Minimális PDF-író (csak szöveg, Courier, fekvő A4) - teszthez,
*& amíg a Smartform nincs kész. Csak ASCII karakterek maradnak meg.
*&---------------------------------------------------------------*
CLASS lcl_pdf DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES tt_lines TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    CLASS-METHODS build
      IMPORTING it_lines      TYPE tt_lines
      RETURNING VALUE(rv_pdf) TYPE xstring.
  PRIVATE SECTION.
    CLASS-METHODS esc
      IMPORTING iv_text        TYPE string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS lcl_pdf IMPLEMENTATION.

  METHOD esc.
    DATA lv_c TYPE c LENGTH 1.
    DO strlen( iv_text ) TIMES.
      lv_c = substring( val = iv_text off = sy-index - 1 len = 1 ).
      IF lv_c IS INITIAL.
        rv_text = rv_text && ` `.
        CONTINUE.
      ENDIF.
      IF lv_c < ' ' OR lv_c > '~'.
        lv_c = '?'.
      ENDIF.
      IF lv_c = '(' OR lv_c = ')' OR lv_c = '\'.
        rv_text = rv_text && '\' && lv_c.
      ELSE.
        rv_text = rv_text && lv_c.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD build.
    CONSTANTS lc_per_page TYPE i VALUE 50.
    DATA: lt_off   TYPE STANDARD TABLE OF i WITH EMPTY KEY,
          lv_pdf   TYPE string,
          lv_nl    TYPE string,
          lv_cont  TYPE string,
          lv_kids  TYPE string,
          lv_pages TYPE i,
          lv_obj   TYPE i,
          lv_from  TYPE i,
          lv_to    TYPE i,
          lv_xref  TYPE i,
          lv_total TYPE i.

    lv_nl    = cl_abap_char_utilities=>newline.
    lv_pages = ( lines( it_lines ) + lc_per_page - 1 ) DIV lc_per_page.
    IF lv_pages = 0.
      lv_pages = 1.
    ENDIF.
    lv_total = 3 + 2 * lv_pages.          " objektumok száma

    lv_pdf = |%PDF-1.4{ lv_nl }|.

    " 1: Catalog
    APPEND strlen( lv_pdf ) TO lt_off.
    lv_pdf = lv_pdf && |1 0 obj{ lv_nl }<< /Type /Catalog /Pages 2 0 R >>{ lv_nl }endobj{ lv_nl }|.

    " 2: Pages
    DO lv_pages TIMES.
      lv_obj  = 4 + ( sy-index - 1 ) * 2.
      lv_kids = lv_kids && |{ lv_obj } 0 R |.
    ENDDO.
    APPEND strlen( lv_pdf ) TO lt_off.
    lv_pdf = lv_pdf && |2 0 obj{ lv_nl }<< /Type /Pages /Kids [ { lv_kids }] /Count { lv_pages } >>{ lv_nl }endobj{ lv_nl }|.

    " 3: Font
    APPEND strlen( lv_pdf ) TO lt_off.
    lv_pdf = lv_pdf && |3 0 obj{ lv_nl }<< /Type /Font /Subtype /Type1 /BaseFont /Courier /Encoding /WinAnsiEncoding >>{ lv_nl }endobj{ lv_nl }|.

    " 4..: oldal + tartalom objektumok
    DO lv_pages TIMES.
      lv_obj  = 4 + ( sy-index - 1 ) * 2.
      lv_from = ( sy-index - 1 ) * lc_per_page + 1.
      lv_to   = nmin( val1 = sy-index * lc_per_page val2 = lines( it_lines ) ).

      lv_cont = |BT{ lv_nl }/F1 8 Tf{ lv_nl }10 TL{ lv_nl }30 565 Td{ lv_nl }|.
      IF lv_from <= lv_to.
        LOOP AT it_lines INTO DATA(lv_line) FROM lv_from TO lv_to.
          lv_cont = lv_cont && |({ esc( lv_line ) }) Tj{ lv_nl }T*{ lv_nl }|.
        ENDLOOP.
      ENDIF.
      lv_cont = lv_cont && |ET|.

      APPEND strlen( lv_pdf ) TO lt_off.
      lv_pdf = lv_pdf && |{ lv_obj } 0 obj{ lv_nl }<< /Type /Page /Parent 2 0 R /MediaBox [0 0 842 595] | &&
                         |/Contents { lv_obj + 1 } 0 R /Resources << /Font << /F1 3 0 R >> >> >>{ lv_nl }endobj{ lv_nl }|.

      APPEND strlen( lv_pdf ) TO lt_off.
      lv_pdf = lv_pdf && |{ lv_obj + 1 } 0 obj{ lv_nl }<< /Length { strlen( lv_cont ) } >>{ lv_nl }stream{ lv_nl }| &&
                         |{ lv_cont }{ lv_nl }endstream{ lv_nl }endobj{ lv_nl }|.
    ENDDO.

    " xref + trailer (minden bejegyzés 20 bájt)
    lv_xref = strlen( lv_pdf ).
    lv_pdf  = lv_pdf && |xref{ lv_nl }0 { lv_total + 1 }{ lv_nl }0000000000 65535 f { lv_nl }|.
    LOOP AT lt_off INTO DATA(lv_off).
      lv_pdf = lv_pdf && |{ lv_off WIDTH = 10 ALIGN = RIGHT PAD = '0' } 00000 n { lv_nl }|.
    ENDLOOP.
    lv_pdf = lv_pdf && |trailer{ lv_nl }<< /Size { lv_total + 1 } /Root 1 0 R >>{ lv_nl }| &&
                       |startxref{ lv_nl }{ lv_xref }{ lv_nl }%%EOF{ lv_nl }|.

    rv_pdf = cl_abap_codepage=>convert_to( source = lv_pdf ).
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& 3) Authorization class (place BEFORE lcl_kd_comp)
*&---------------------------------------------------------------------*
CLASS lcl_auth DEFINITION FINAL.
  PUBLIC SECTION.
    " Returns an error text, initial = authorized
    CLASS-METHODS:
      check_posting IMPORTING iv_bukrs      TYPE bukrs
                    RETURNING VALUE(rv_msg) TYPE string.
ENDCLASS.

CLASS lcl_auth IMPLEMENTATION.

  METHOD check_posting.
    " 01 = post the clearing document (FB05)
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
      ID 'BUKRS' FIELD iv_bukrs
      ID 'ACTVT' FIELD '01'.
    IF sy-subrc <> 0.
      rv_msg = |No authorization to post documents in company code { iv_bukrs } (F_BKPF_BUK, activity 01)|.
      RETURN.
    ENDIF.

    " 02 = change a document (payment block is changed with FI_DOCUMENT_CHANGE)
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
      ID 'BUKRS' FIELD iv_bukrs
      ID 'ACTVT' FIELD '02'.
    IF sy-subrc <> 0.
      rv_msg = |No authorization to change documents in company code { iv_bukrs } (F_BKPF_BUK, activity 02)|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------*
*& Kiegyenlítő osztály (POSTING_INTERFACE_CLEARING)
*&---------------------------------------------------------------*
CLASS lcl_kd_comp DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_item,
             kind  TYPE c LENGTH 1,   " D = ügyfél, K = szállító
             belnr TYPE belnr_d,
             gjahr TYPE gjahr,
             buzei TYPE buzei,
             shkzg TYPE shkzg,        " S = tartozik, H = követel
             wrbtr TYPE wrbtr,
             waers TYPE waers,
           END OF ty_item,
           tt_item TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY,
           BEGIN OF ty_result,
             success TYPE abap_bool,
             post    TYPE abap_bool,    " igaz = valódi könyvelés történt
             belnr   TYPE belnr_d,      " kiegyenlítő bizonylat
             gjahr   TYPE gjahr,
             buzei   TYPE buzei,
             balance TYPE wrbtr,        " egyenleg (nullának kell lennie)
             msg     TYPE string,
             blk_msg TYPE string,       " fizetési zárolás átírásának hibája (figyelmeztetés)
           END OF ty_result.

    CLASS-METHODS clear_items
      IMPORTING iv_bukrs         TYPE bukrs
                iv_kunnr         TYPE kunnr
                iv_lifnr         TYPE lifnr
                iv_budat         TYPE budat
                iv_blart         TYPE blart     DEFAULT 'AB'
                iv_mode          TYPE allgazmd  DEFAULT 'N'
                iv_test          TYPE abap_bool DEFAULT abap_false
                iv_zlspr_d       TYPE dzlspr    OPTIONAL   " új zárolás a vevőtételeken
                iv_zlspr_k       TYPE dzlspr    OPTIONAL   " új zárolás a szállítói tételeken
                it_items         TYPE tt_item
      RETURNING VALUE(rs_result) TYPE ty_result.

    CLASS-METHODS set_payment_block
      IMPORTING iv_bukrs   TYPE bukrs
                iv_belnr   TYPE belnr_d
                iv_gjahr   TYPE gjahr
                iv_buzei   TYPE buzei
                iv_new     TYPE dzlspr
      EXPORTING ev_msg     TYPE string         " üres = rendben
                ev_old     TYPE dzlspr
                ev_changed TYPE abap_bool.
ENDCLASS.

*&---------------------------------------------------------------*
*& Lokális osztály: az adatlekérdezés és megjelenítés logikája
*&---------------------------------------------------------------*
CLASS lcl_open_items DEFINITION.

  PUBLIC SECTION.
    METHODS:
      select_data
        IMPORTING iv_bukrs TYPE bukrs
                  iv_c2v   TYPE abap_bool
                  iv_v2c   TYPE abap_bool
                  iv_kunnr TYPE kna1-kunnr
                  iv_lifnr TYPE lfa1-lifnr
                  iv_date  TYPE sy-datum
                  iv_text  TYPE char50,
      show_screen,
      pbo,
      pai,
      pbo_9100,
      pai_9100.

  PRIVATE SECTION.
    DATA: mt_bsid TYPE tt_bsid,
          mt_bsik TYPE tt_bsik,
          mt_out  TYPE STANDARD TABLE OF ty_out.

    " Elszámolás iránya (a rádiógombokból)
    DATA: mv_c2v TYPE abap_bool,
          mv_v2c TYPE abap_bool.

    " 9000-es képernyő
    DATA: mo_cc_top    TYPE REF TO cl_gui_custom_container,
          mo_splitter  TYPE REF TO cl_gui_splitter_container,
          mo_cont_cust TYPE REF TO cl_gui_container,
          mo_cont_vend TYPE REF TO cl_gui_container,
          mo_alv_cust  TYPE REF TO cl_salv_table,
          mo_alv_vend  TYPE REF TO cl_salv_table,
          mo_cc_sum    TYPE REF TO cl_gui_custom_container,
          mo_alv_sum   TYPE REF TO cl_salv_table.

    " 9100-as képernyő (Overview)
    DATA: mt_prop_hdr   TYPE STANDARD TABLE OF ty_prop_hdr,
          mt_prop_items TYPE STANDARD TABLE OF ty_prop_item,
          mo_cc_prop    TYPE REF TO cl_gui_custom_container,
          mo_split_prop TYPE REF TO cl_gui_splitter_container,
          mo_alv_hdr    TYPE REF TO cl_salv_table,
          mo_alv_items  TYPE REF TO cl_salv_table.

    DATA: mt_amt  TYPE tt_amt,
          mt_pick TYPE salv_t_row,
          mv_iter TYPE i.

    DATA: mv_matched  TYPE abap_bool,    " volt-e összehasonlítás
          mv_compno   TYPE char10,         " a 9100-on megnyitott kompenzáció száma
          mv_dir      TYPE c LENGTH 1,     " C = vevő->szállító, V = szállító->vevő
          mv_cust     TYPE kunnr,
          mv_vend     TYPE lifnr,
          mv_from_log TYPE abap_bool.      " igaz = a Z táblából betöltött futás

    DATA: mv_langu     TYPE sy-langu,    " language of the run (popup) -> SO10 mail texts
          mv_mail_sent TYPE abap_bool,   " proposal mail already sent (yellow rows)
          mv_posted    TYPE abap_bool.   " compensation already posted (green rows)

    DATA: mt_sel_c TYPE salv_t_row,      " rögzített vevői kijelölés
          mt_sel_v TYPE salv_t_row.      " rögzített szállítói kijelölés

    METHODS:
      build_alv_top,
      build_alv_summary,
      build_alv_prop,
      fill_summary,
      do_refresh,
      create_proposal,
      show_overview,
      build_proposal_data
        IMPORTING it_bsid   TYPE tt_bsid
                  it_bsik   TYPE tt_bsik
                  iv_compno TYPE clike,
      on_added_function
                    FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function,
      process_cust,
      process_vend,
      match_selected,
      save_run
        IMPORTING iv_compno    TYPE char10
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      save_log
        IMPORTING iv_compno    TYPE char10
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      new_compno
        RETURNING VALUE(rv_compno) TYPE char10,
      pdf_path
        IMPORTING iv_compno      TYPE char10
        RETURNING VALUE(rv_path) TYPE string,
      write_pdf
        IMPORTING iv_compno TYPE char10
                  it_bsid   TYPE tt_bsid_p
                  it_bsik   TYPE tt_bsik_p
        EXPORTING ev_ok     TYPE abap_bool,
      read_pdf
        IMPORTING iv_compno TYPE char10
        EXPORTING ev_pdf    TYPE xstring
                  ev_ok     TYPE abap_bool,
      send_mail
        IMPORTING iv_posted TYPE abap_bool DEFAULT abap_false,
      post_from_overview,
      post_items
        IMPORTING it_items  TYPE lcl_kd_comp=>tt_item
                  iv_test   TYPE abap_bool
                  iv_kunnr  TYPE kunnr
                  iv_lifnr  TYPE lifnr
                  iv_budat  TYPE budat
        EXPORTING ev_ok     TYPE abap_bool
                  et_posted TYPE tt_posted,
      get_matched_items
        RETURNING VALUE(rt_items) TYPE lcl_kd_comp=>tt_item,
      get_logged_items
        RETURNING VALUE(rt_items) TYPE lcl_kd_comp=>tt_item,
      check_items_open
        IMPORTING it_items     TYPE lcl_kd_comp=>tt_item
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      remove_posted_items
        IMPORTING it_posted TYPE tt_posted,
      show_posted_popup
        IMPORTING it_posted TYPE tt_posted,
      save_posting
        IMPORTING it_posted   TYPE tt_posted
                  iv_complete TYPE abap_bool,
      get_recipient
        RETURNING VALUE(rv_mail) TYPE ad_smtpadr,
      set_overview_colors,
      read_so10
        IMPORTING iv_name         TYPE tdobname
        RETURNING VALUE(rt_lines) TYPE tt_tline,
      get_mail_text
        IMPORTING iv_posted  TYPE abap_bool
                  it_symbols TYPE tt_symbol
        EXPORTING ev_subject TYPE so_obj_des
                  et_body    TYPE soli_tab
                  ev_found   TYPE abap_bool,
      confirm
        IMPORTING iv_title      TYPE string
                  iv_question   TYPE string
        RETURNING VALUE(rv_yes) TYPE abap_bool,
      load_run
        IMPORTING iv_compno    TYPE char10
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      apply_colors,
      delete_yellow,
      free_prop,
      run_clearing
        IMPORTING iv_test      TYPE abap_bool
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      subset_rec
        IMPORTING iv_pos       TYPE i
                  iv_rest      TYPE int8
        RETURNING VALUE(rv_ok) TYPE abap_bool,
      find_subset
        IMPORTING it_amt    TYPE tt_amt
                  iv_target TYPE int8
        EXPORTING et_idx    TYPE salv_t_row
                  ev_ok     TYPE abap_bool.

ENDCLASS.

*&---------------------------------------------------------------*
*& lcl_kd_comp IMPLEMENTATION
*&---------------------------------------------------------------*
CLASS lcl_kd_comp IMPLEMENTATION.

  METHOD set_payment_block.
    DATA: lt_accchg TYPE STANDARD TABLE OF accchg,
          lv_old    TYPE dzlspr.

    CLEAR: ev_msg, ev_old, ev_changed.

    " Aktuális érték a bizonylatsorból (a kiegyenlítő bizonylat sorai már lehetnek zártak)
    SELECT SINGLE zlspr FROM bseg
      WHERE bukrs = @iv_bukrs AND belnr = @iv_belnr
        AND gjahr = @iv_gjahr AND buzei = @iv_buzei
      INTO @lv_old.
    IF sy-subrc <> 0.
      ev_msg = |{ iv_belnr }/{ iv_buzei }: bizonylatsor nem található|.
      RETURN.
    ENDIF.

    ev_old = lv_old.
    IF lv_old = iv_new.          " nincs teendő
      RETURN.
    ENDIF.

    " ACCCHG-ben nincs tételszám, azt az I_BUZEI importparaméter adja
    APPEND VALUE #( fdname = 'ZLSPR'
                    oldval = lv_old
                    newval = iv_new ) TO lt_accchg.

    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_bukrs              = iv_bukrs
        i_belnr              = iv_belnr
        i_gjahr              = iv_gjahr
        i_buzei              = iv_buzei
      TABLES
        t_accchg             = lt_accchg
      EXCEPTIONS
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        OTHERS               = 6.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      ev_changed = abap_true.
    ELSE.
      DATA(lv_rc) = sy-subrc.
      ROLLBACK WORK.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_msg.
      ELSE.
        ev_msg = |{ iv_belnr }/{ iv_buzei }: fizetési zárolás módosítása sikertelen (subrc { lv_rc })|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD clear_items.
    DATA: lt_ftclear TYPE STANDARD TABLE OF ftclear,
          lt_ftpost  TYPE STANDARD TABLE OF ftpost,
          lt_fttax   TYPE STANDARD TABLE OF fttax,
          lt_blntab  TYPE STANDARD TABLE OF blntab,
          lv_date    TYPE c LENGTH 10,
          lv_balance TYPE wrbtr,
          lv_waers   TYPE waers,
          lv_has_d   TYPE abap_bool,
          lv_has_k   TYPE abap_bool,
          lv_msgid   TYPE sy-msgid,
          lv_msgno   TYPE sy-msgno,
          lv_msgty   TYPE sy-msgty,
          lv_msgv1   TYPE sy-msgv1,
          lv_msgv2   TYPE sy-msgv2,
          lv_msgv3   TYPE sy-msgv3,
          lv_msgv4   TYPE sy-msgv4,
          lv_subrc   TYPE sy-subrc.

    rs_result-success = abap_false.
    rs_result-post    = abap_false.

    "------------------------------------------------------------------
    " 1) Ellenőrzések (irányfüggetlen: az előjel dönt, nem a C2V/V2C)
    "------------------------------------------------------------------
    IF it_items IS INITIAL.
      rs_result-msg = 'Nincs átadott tétel'.
      RETURN.
    ENDIF.

    lv_waers = it_items[ 1 ]-waers.
    LOOP AT it_items INTO DATA(ls_chk).
      IF ls_chk-waers <> lv_waers.
        rs_result-msg = 'Több pénznem: hívd pénznemenként külön'.
        RETURN.
      ENDIF.
      CASE ls_chk-kind.
        WHEN 'D'. lv_has_d = abap_true.
        WHEN 'K'. lv_has_k = abap_true.
      ENDCASE.
      IF ls_chk-shkzg = 'S'.
        lv_balance = lv_balance + ls_chk-wrbtr.
      ELSE.
        lv_balance = lv_balance - ls_chk-wrbtr.
      ENDIF.
    ENDLOOP.

    rs_result-balance = lv_balance.

    IF lv_has_d = abap_false OR lv_has_k = abap_false.
      rs_result-msg = 'Ügyfél és szállító tétel is kell'.
      RETURN.
    ENDIF.

    IF lv_balance <> 0.
      rs_result-msg = |Az egyenleg nem nulla: { lv_balance } { lv_waers }|.
      RETURN.
    ENDIF.

    IF iv_test = abap_true.
      rs_result-success = abap_true.
      rs_result-msg     = |Szimuláció: kompenzálható ({ lines( it_items ) } tétel, { lv_waers })|.
      RETURN.
    ENDIF.

    "------------------------------------------------------------------
    " 2) FTCLEAR – minden tétel külön sor
    "    (BELNR + GJAHR + BUZEI; tesztben ellenőrizd a formátumot)
    "------------------------------------------------------------------
    LOOP AT it_items INTO DATA(ls_i).
      APPEND VALUE #( agkoa  = ls_i-kind
                      agkon  = COND #( WHEN ls_i-kind = 'D' THEN iv_kunnr ELSE iv_lifnr )
                      agbuk  = iv_bukrs
                      xnops  = abap_true
                      selfd  = 'BELNR'
                      selvon = |{ ls_i-belnr }{ ls_i-gjahr }{ ls_i-buzei }|
                    ) TO lt_ftclear.
    ENDLOOP.

    "------------------------------------------------------------------
    " 3) FTPOST – csak fej, nulla egyenlegnél nincs új tétel
    "------------------------------------------------------------------
    lv_date = |{ iv_budat DATE = USER }|.
    APPEND VALUE #( stype = 'K' count = '001' fnam = 'BKPF-BLDAT' fval = lv_date )   TO lt_ftpost.
    APPEND VALUE #( stype = 'K' count = '001' fnam = 'BKPF-BUDAT' fval = lv_date )   TO lt_ftpost.
    APPEND VALUE #( stype = 'K' count = '001' fnam = 'BKPF-BLART' fval = iv_blart )  TO lt_ftpost.
    APPEND VALUE #( stype = 'K' count = '001' fnam = 'BKPF-BUKRS' fval = iv_bukrs )  TO lt_ftpost.
    APPEND VALUE #( stype = 'K' count = '001' fnam = 'BKPF-WAERS' fval = lv_waers )  TO lt_ftpost.

    "------------------------------------------------------------------
    " 4) Könyvelés (POSTING_INTERFACE_CLEARING)
    "------------------------------------------------------------------
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_function = 'C'
        i_mode     = iv_mode
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      rs_result-msg = 'POSTING_INTERFACE_START hiba'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = 'UMBUCHNG'
        i_tcode                    = 'FB05'
        i_sgfunct                  = 'C'
      IMPORTING
        e_msgid                    = lv_msgid
        e_msgno                    = lv_msgno
        e_msgty                    = lv_msgty
        e_msgv1                    = lv_msgv1
        e_msgv2                    = lv_msgv2
        e_msgv3                    = lv_msgv3
        e_msgv4                    = lv_msgv4
      TABLES
        t_blntab                   = lt_blntab
        t_ftclear                  = lt_ftclear
        t_ftpost                   = lt_ftpost
        t_fttax                    = lt_fttax
      EXCEPTIONS
        clearing_procedure_invalid = 1
        clearing_procedure_missing = 2
        table_t041a_empty          = 3
        transaction_code_invalid   = 4
        amount_format_error        = 5
        too_many_line_items        = 6
        company_code_invalid       = 7
        screen_not_found           = 8
        no_authorization           = 9
        OTHERS                     = 10.
    lv_subrc = sy-subrc.

    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXPORTING
        i_bdcimmed = abap_true
      EXCEPTIONS
        OTHERS     = 1.

    "------------------------------------------------------------------
    " 5) Eredmény
    "------------------------------------------------------------------
    READ TABLE lt_blntab INTO DATA(ls_bln) INDEX 1.
    IF lv_subrc = 0 AND sy-subrc = 0.
      rs_result-success = abap_true.
      rs_result-post    = abap_true.
      rs_result-belnr   = ls_bln-belnr.
      rs_result-gjahr   = ls_bln-gjahr.
      rs_result-buzei   = '001'.
      rs_result-msg     = |Sikeres kompenzáció: { ls_bln-belnr }|.

      "----------------------------------------------------------------
      " 6) Fizetési zárolás átírása - CSAK sikeres könyvelés után,
      "    a kiegyenlítő bizonylat vevői (D) / szállítói (K) során
      "    (a kompenzáció ettől már nem vonható vissza, hiba esetén
      "     csak figyelmeztetés: rs_result-blk_msg)
      "----------------------------------------------------------------
      IF iv_zlspr_d IS NOT INITIAL OR iv_zlspr_k IS NOT INITIAL.

        SELECT buzei, koart, zlspr FROM bseg
          WHERE bukrs = @iv_bukrs
            AND belnr = @ls_bln-belnr
            AND gjahr = @ls_bln-gjahr
            AND koart IN ( 'D', 'K' )
          INTO TABLE @DATA(lt_lines).

        IF lt_lines IS INITIAL.
          rs_result-blk_msg = |nincs vevői/szállítói sor a { ls_bln-belnr } kiegyenlítő bizonylaton|.
        ENDIF.

        LOOP AT lt_lines INTO DATA(ls_line).
          DATA(lv_new) = COND dzlspr( WHEN ls_line-koart = 'D' THEN iv_zlspr_d
                                      ELSE iv_zlspr_k ).
          CHECK lv_new IS NOT INITIAL.        " nincs megadva -> nem nyúlunk hozzá
          CHECK ls_line-zlspr <> lv_new.      " már jó az érték

          set_payment_block( EXPORTING iv_bukrs = iv_bukrs
                                       iv_belnr = ls_bln-belnr
                                       iv_gjahr = ls_bln-gjahr
                                       iv_buzei = ls_line-buzei
                                       iv_new   = lv_new
                             IMPORTING ev_msg   = DATA(lv_blk_err) ).
          IF lv_blk_err IS NOT INITIAL.
            rs_result-blk_msg = lv_blk_err.
            EXIT.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ELSE.
      IF lv_msgty IS NOT INITIAL.
        MESSAGE ID lv_msgid TYPE 'S' NUMBER lv_msgno
                WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO rs_result-msg.
      ELSE.
        rs_result-msg = |Hiba (subrc { lv_subrc })|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------*
*& lcl_open_items IMPLEMENTATION
*&---------------------------------------------------------------*
CLASS lcl_open_items IMPLEMENTATION.

*&---------------------------------------------------------------*
  METHOD select_data.

    " Irány eltárolása a fill_summary és a match_selected számára
    mv_c2v = iv_c2v.
    mv_v2c = iv_v2c.

    SELECT * FROM bsid
      INTO CORRESPONDING FIELDS OF TABLE @mt_bsid
      WHERE bukrs = @iv_bukrs
        AND kunnr = @iv_kunnr
        AND budat <= @iv_date.

    SELECT * FROM bsik
      INTO CORRESPONDING FIELDS OF TABLE @mt_bsik
      WHERE bukrs = @iv_bukrs
        AND lifnr = @iv_lifnr
        AND budat <= @iv_date.

    fill_summary( ).

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Részhalmaz-összeg keresés
*&---------------------------------------------------------------*
  METHOD subset_rec.

    IF iv_rest = 0.
      rv_ok = abap_true.
      RETURN.
    ENDIF.
    IF iv_rest < 0 OR iv_pos > lines( mt_amt ) OR mv_iter > 2000000.
      RETURN.
    ENDIF.
    mv_iter = mv_iter + 1.

    DATA(ls_amt) = mt_amt[ iv_pos ].

    " 1. ág: a sort beleveszem
    APPEND ls_amt-idx TO mt_pick.
    IF subset_rec( iv_pos = iv_pos + 1 iv_rest = iv_rest - ls_amt-cents ) = abap_true.
      rv_ok = abap_true.
      RETURN.
    ENDIF.
    DELETE mt_pick INDEX lines( mt_pick ).

    " 2. ág: a sort kihagyom
    rv_ok = subset_rec( iv_pos = iv_pos + 1 iv_rest = iv_rest ).

  ENDMETHOD.

  METHOD find_subset.

    CLEAR: et_idx, mt_pick, mv_iter, ev_ok.
    CHECK iv_target > 0.

    mt_amt = it_amt.
    SORT mt_amt BY prio ASCENDING cents DESCENDING.

    ev_ok = subset_rec( iv_pos = 1 iv_rest = iv_target ).
    IF ev_ok = abap_true.
      et_idx = mt_pick.
    ENDIF.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD fill_summary.

    DATA: lt_col     TYPE lvc_t_scol,
          lv_sum_c   TYPE p LENGTH 13 DECIMALS 2,
          lv_sum_v   TYPE p LENGTH 13 DECIMALS 2,
          lv_waers_c TYPE waers,
          lv_waers_v TYPE waers,
          lv_base    TYPE p LENGTH 13 DECIMALS 2,
          lv_other   TYPE p LENGTH 13 DECIMALS 2,
          lv_waers_b TYPE waers,
          lv_diff_tx TYPE char40.

    CLEAR mt_out.

    IF mt_bsid IS NOT INITIAL.
      lv_sum_c   = REDUCE wrbtr( INIT sum = CONV wrbtr( 0 )
                                 FOR wa IN mt_bsid
                                 NEXT sum = sum + wa-wrbtr ).
      lv_waers_c = mt_bsid[ 1 ]-waers.
    ENDIF.

    IF mt_bsik IS NOT INITIAL.
      lv_sum_v   = REDUCE wrbtr( INIT sum = CONV wrbtr( 0 )
                                 FOR wa2 IN mt_bsik
                                 NEXT sum = sum + wa2-wrbtr ).
      lv_waers_v = mt_bsik[ 1 ]-waers.
    ENDIF.

    " Irány: Customer -> Vendor vagy Vendor -> Customer
    IF mv_c2v = abap_true.
      lv_base    = abs( lv_sum_c ).
      lv_other   = abs( lv_sum_v ).
      lv_waers_b = lv_waers_c.
      lv_diff_tx = 'Difference: Vendor to amount'.
    ELSE.
      lv_base    = abs( lv_sum_v ).
      lv_other   = abs( lv_sum_c ).
      lv_waers_b = lv_waers_v.
      lv_diff_tx = 'Difference: Customer to amount'.
    ENDIF.

    lt_col = VALUE #( ( color-col = 7 color-int = 0 color-inv = 0 ) ).
    APPEND VALUE #( text = 'Amount' amount = lv_base waers = lv_waers_b color = lt_col ) TO mt_out.

    lt_col = VALUE #( ( color-col = 5 color-int = 0 color-inv = 0 ) ).
    APPEND VALUE #( text = 'Customer' amount = lv_sum_c waers = lv_waers_c color = lt_col ) TO mt_out.
    APPEND VALUE #( text = 'Vendor'   amount = lv_sum_v waers = lv_waers_v color = lt_col ) TO mt_out.

    lt_col = VALUE #( ( color-col = 3 color-int = 0 color-inv = 0 ) ).
    APPEND VALUE #( text   = lv_diff_tx
                    amount = lv_base - lv_other
                    waers  = lv_waers_b
                    color  = lt_col ) TO mt_out.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD show_screen.

    IF mt_bsid IS INITIAL AND mt_bsik IS INITIAL.
      MESSAGE 'No results found for the specified filters' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL SCREEN 9000.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD pbo.

    SET PF-STATUS 'STATUS_9000'.
    SET TITLEBAR  'TITLE_9000'.

    IF mo_cc_top IS NOT BOUND.
      build_alv_top( ).
    ENDIF.

    IF mo_cc_sum IS NOT BOUND.
      build_alv_summary( ).
    ENDIF.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD pai.

    save_ok = ok_code.
    CLEAR ok_code.

    CASE save_ok.
      WHEN 'EXIT' OR 'CANC'.
        LEAVE PROGRAM.
      WHEN 'BACK'.
        SET SCREEN 0.

      WHEN 'REFRESH'.
        do_refresh( ).

      WHEN 'CREATE_PROP' OR 'CREATE_PRO'.
        create_proposal( ).

      WHEN 'OVERVIEW'.
        show_overview( ).
    ENDCASE.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Vevői és szállítói nyitott tételek
*&---------------------------------------------------------------*
  METHOD build_alv_top.

    CREATE OBJECT mo_cc_top
      EXPORTING
        container_name = 'CC_TOP'.

    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_cc_top
        rows    = 1
        columns = 2.

    " C2V: customer left, vendor right | V2C: vendor left, customer right
    IF mv_v2c = abap_true.
      mo_cont_vend = mo_splitter->get_container( row = 1 column = 1 ).
      mo_cont_cust = mo_splitter->get_container( row = 1 column = 2 ).
    ELSE.
      mo_cont_cust = mo_splitter->get_container( row = 1 column = 1 ).
      mo_cont_vend = mo_splitter->get_container( row = 1 column = 2 ).
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING r_container  = mo_cont_cust
          IMPORTING r_salv_table = mo_alv_cust
          CHANGING  t_table      = mt_bsid ).

        mo_alv_cust->get_functions( )->set_all( abap_false ).
        mo_alv_cust->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        mo_alv_cust->get_display_settings( )->set_list_header( |Customer open items: { p_kunnr }| ).
        mo_alv_cust->get_display_settings( )->set_striped_pattern( abap_true ).
        mo_alv_cust->get_columns( )->set_color_column( 'COLOR' ).
        mo_alv_cust->get_columns( )->get_column( 'CMP_STAT' )->set_visible( abap_false ).

        mo_alv_cust->get_functions( )->add_function(
          name     = 'CUST_SEL'
          icon     = CONV #( icon_execute_object )
          text     = 'Customer Selections'
          tooltip  = 'Recording of Selected Customer Items'
          position = if_salv_c_function_position=>left_of_salv_functions ).

        SET HANDLER on_added_function FOR mo_alv_cust->get_event( ).

        mo_alv_cust->display( ).

      CATCH cx_salv_error.
    ENDTRY.

    TRY.
        cl_salv_table=>factory(
          EXPORTING r_container  = mo_cont_vend
          IMPORTING r_salv_table = mo_alv_vend
          CHANGING  t_table      = mt_bsik ).

        mo_alv_vend->get_functions( )->set_all( abap_false ).
        mo_alv_vend->get_columns( )->set_optimize( abap_true ).
        mo_alv_vend->get_display_settings( )->set_list_header( |Vendor open items: { p_lifnr }| ).
        mo_alv_vend->get_display_settings( )->set_striped_pattern( abap_true ).
        mo_alv_vend->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        mo_alv_vend->get_columns( )->set_color_column( 'COLOR' ).
        mo_alv_vend->get_columns( )->get_column( 'CMP_STAT' )->set_visible( abap_false ).

        mo_alv_vend->get_functions( )->add_function(
          name     = 'VEND_SEL'
          icon     = CONV #( icon_execute_object )
          text     = 'Vendor Selections'
          tooltip  = 'Recording of Selected Vendor Items'
          position = if_salv_c_function_position=>left_of_salv_functions ).

        SET HANDLER on_added_function FOR mo_alv_vend->get_event( ).

        mo_alv_vend->display( ).

      CATCH cx_salv_error.
    ENDTRY.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD on_added_function.

    CASE e_salv_function.
      WHEN 'CUST_SEL'.
        process_cust( ).
      WHEN 'VEND_SEL'.
        process_vend( ).
      WHEN 'DEL_YEL'.
        delete_yellow( ).
    ENDCASE.

  ENDMETHOD.

  METHOD process_cust.

    mt_sel_c = mo_alv_cust->get_selections( )->get_selected_rows( ).

    IF mt_sel_c IS INITIAL.
      MESSAGE 'No customer line has been selected' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    IF mt_sel_v IS INITIAL.
      MESSAGE |{ lines( mt_sel_c ) } Customer line entered; please select the Vendor lines as well| TYPE 'S'.
      RETURN.
    ENDIF.

    match_selected( ).

  ENDMETHOD.

  METHOD process_vend.

    mt_sel_v = mo_alv_vend->get_selections( )->get_selected_rows( ).

    IF mt_sel_v IS INITIAL.
      MESSAGE 'No Vendor line has been selected' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    IF mt_sel_c IS INITIAL.
      MESSAGE |{ lines( mt_sel_v ) } Vendor line entered; please select the Customer lines as well| TYPE 'S'.
      RETURN.
    ENDIF.

    match_selected( ).

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Párosítás irány szerint
*&   C2V: MINDEN kijelölt vevői sor kell, a szállítóiból keresünk részhalmazt
*&   V2C: MINDEN kijelölt szállítói sor kell, a vevőiből keresünk részhalmazt
*&---------------------------------------------------------------*
  METHOD match_selected.

    DATA: lt_amt_c TYPE tt_amt,
          lt_amt_v TYPE tt_amt,
          lt_pick  TYPE salv_t_row,
          lt_cur   TYPE SORTED TABLE OF waers WITH UNIQUE KEY table_line,
          lt_key_c TYPE SORTED TABLE OF ty_dockey WITH UNIQUE KEY belnr gjahr,
          lt_key_v TYPE SORTED TABLE OF ty_dockey WITH UNIQUE KEY belnr gjahr,
          lv_sum_c TYPE int8,
          lv_sum_v TYPE int8,
          lv_ok    TYPE abap_bool.

    " Előző státusz törlése
    LOOP AT mt_bsid ASSIGNING FIELD-SYMBOL(<d0>).
      CLEAR <d0>-cmp_stat.
    ENDLOOP.
    LOOP AT mt_bsik ASSIGNING FIELD-SYMBOL(<k0>).
      CLEAR <k0>-cmp_stat.
    ENDLOOP.

    " Kijelölt sorok összegei (fillér pontossággal) - WRBTR, mint a clear_items-ben
    LOOP AT mt_sel_c INTO DATA(lv_c).
      ASSIGN mt_bsid[ lv_c ] TO FIELD-SYMBOL(<d>).
      CHECK sy-subrc = 0.
      INSERT <d>-waers INTO TABLE lt_cur.
      INSERT VALUE ty_dockey( belnr = <d>-belnr gjahr = <d>-gjahr ) INTO TABLE lt_key_c.
      DATA(lv_cc) = CONV int8( abs( <d>-wrbtr ) * 100 ).
      APPEND VALUE #( idx = lv_c cents = lv_cc prio = 1 ) TO lt_amt_c.
      lv_sum_c = lv_sum_c + lv_cc.
    ENDLOOP.

    LOOP AT mt_sel_v INTO DATA(lv_v).
      ASSIGN mt_bsik[ lv_v ] TO FIELD-SYMBOL(<k>).
      CHECK sy-subrc = 0.
      INSERT <k>-waers INTO TABLE lt_cur.
      INSERT VALUE ty_dockey( belnr = <k>-belnr gjahr = <k>-gjahr ) INTO TABLE lt_key_v.
      DATA(lv_vc) = CONV int8( abs( <k>-wrbtr ) * 100 ).
      APPEND VALUE #( idx = lv_v cents = lv_vc prio = 1 ) TO lt_amt_v.
      lv_sum_v = lv_sum_v + lv_vc.
    ENDLOOP.

    " Egy párosításban csak egy pénznem lehet
    IF lines( lt_cur ) > 1.
      MESSAGE 'The selected items have different currencies; select items of one currency only' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " Azonos bizonylaton lévő ellenoldali sor előnyben (pl. 1020000001: vevő 1. tétel <-> szállító 2. tétel)
    LOOP AT lt_amt_c ASSIGNING FIELD-SYMBOL(<ac>).
      ASSIGN mt_bsid[ <ac>-idx ] TO FIELD-SYMBOL(<dc>).
      IF sy-subrc = 0 AND line_exists( lt_key_v[ belnr = <dc>-belnr gjahr = <dc>-gjahr ] ).
        <ac>-prio = 0.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_amt_v ASSIGNING FIELD-SYMBOL(<av>).
      ASSIGN mt_bsik[ <av>-idx ] TO FIELD-SYMBOL(<kv>).
      IF sy-subrc = 0 AND line_exists( lt_key_c[ belnr = <kv>-belnr gjahr = <kv>-gjahr ] ).
        <av>-prio = 0.
      ENDIF.
    ENDLOOP.

    IF mv_v2c = abap_true.
      " ---- V2C: a szállítói sorok fixek, a vevőkből keresünk részhalmazt
      find_subset( EXPORTING it_amt    = lt_amt_c
                             iv_target = lv_sum_v
                   IMPORTING et_idx    = lt_pick
                             ev_ok     = lv_ok ).

      IF lv_ok = abap_true.
        LOOP AT mt_sel_v INTO lv_v.
          mt_bsik[ lv_v ]-cmp_stat = 'G'.
        ENDLOOP.
        LOOP AT mt_sel_c INTO lv_c.
          mt_bsid[ lv_c ]-cmp_stat = COND #( WHEN line_exists( lt_pick[ table_line = lv_c ] )
                                             THEN 'G' ELSE 'Y' ).
        ENDLOOP.
      ENDIF.

    ELSE.
      " ---- C2V: a vevői sorok fixek, a szállítókból keresünk részhalmazt
      find_subset( EXPORTING it_amt    = lt_amt_v
                             iv_target = lv_sum_c
                   IMPORTING et_idx    = lt_pick
                             ev_ok     = lv_ok ).

      IF lv_ok = abap_true.
        LOOP AT mt_sel_c INTO lv_c.
          mt_bsid[ lv_c ]-cmp_stat = 'G'.
        ENDLOOP.
        LOOP AT mt_sel_v INTO lv_v.
          mt_bsik[ lv_v ]-cmp_stat = COND #( WHEN line_exists( lt_pick[ table_line = lv_v ] )
                                             THEN 'G' ELSE 'Y' ).
        ENDLOOP.
      ENDIF.
    ENDIF.

    " Nincs egyezés: minden kijelölt sor sárga
    IF lv_ok = abap_false.
      LOOP AT mt_sel_c INTO lv_c.
        mt_bsid[ lv_c ]-cmp_stat = 'Y'.
      ENDLOOP.
      LOOP AT mt_sel_v INTO lv_v.
        mt_bsik[ lv_v ]-cmp_stat = 'Y'.
      ENDLOOP.
      IF mv_iter > 2000000.
        MESSAGE 'Search limit reached; select fewer items' TYPE 'S' DISPLAY LIKE 'W'.
      ELSE.
        MESSAGE 'The total of the selected items does not match; there is no group that can be used for offsetting' TYPE 'S' DISPLAY LIKE 'W'.
      ENDIF.
    ENDIF.

    apply_colors( ).
    mv_matched = abap_true.

    " Kijelölés törlése, különben eltakarja a színeket
    mo_alv_cust->get_selections( )->set_selected_rows( VALUE #( ) ).
    mo_alv_vend->get_selections( )->set_selected_rows( VALUE #( ) ).
    CLEAR: mt_sel_c, mt_sel_v.

    mo_alv_cust->refresh( refresh_mode = if_salv_c_refresh=>soft ).
    mo_alv_vend->refresh( refresh_mode = if_salv_c_refresh=>soft ).

  ENDMETHOD.

  METHOD apply_colors.

    LOOP AT mt_bsid ASSIGNING FIELD-SYMBOL(<d>).
      CLEAR <d>-color.
      CASE <d>-cmp_stat.
        WHEN 'G'. <d>-color = VALUE #( ( color-col = 5 color-int = 1 color-inv = 0 ) ).  " zöld
        WHEN 'Y'. <d>-color = VALUE #( ( color-col = 3 color-int = 1 color-inv = 0 ) ).  " sárga
      ENDCASE.
    ENDLOOP.

    LOOP AT mt_bsik ASSIGNING FIELD-SYMBOL(<k>).
      CLEAR <k>-color.
      CASE <k>-cmp_stat.
        WHEN 'G'. <k>-color = VALUE #( ( color-col = 5 color-int = 1 color-inv = 0 ) ).
        WHEN 'Y'. <k>-color = VALUE #( ( color-col = 3 color-int = 1 color-inv = 0 ) ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD delete_yellow.

    IF NOT line_exists( mt_bsid[ cmp_stat = 'Y' ] ) AND
       NOT line_exists( mt_bsik[ cmp_stat = 'Y' ] ).
      MESSAGE 'Nincs sárga sor' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    DELETE mt_bsid WHERE cmp_stat = 'Y'.
    DELETE mt_bsik WHERE cmp_stat = 'Y'.

    CLEAR: mt_sel_c, mt_sel_v.

    mo_alv_cust->get_selections( )->set_selected_rows( VALUE #( ) ).
    mo_alv_vend->get_selections( )->set_selected_rows( VALUE #( ) ).

    fill_summary( ).

    mo_alv_cust->refresh( refresh_mode = if_salv_c_refresh=>soft ).
    mo_alv_vend->refresh( refresh_mode = if_salv_c_refresh=>soft ).
    mo_alv_sum->refresh( refresh_mode = if_salv_c_refresh=>soft ).

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD save_log.

    DATA: lt_log TYPE STANDARD TABLE OF yyfi_comp_log,
          lv_cnt TYPE i.

    rv_ok = abap_false.

    SELECT SINGLE waers FROM t001
      WHERE bukrs = @p_bukrs
      INTO @DATA(lv_hwaer).

    DATA(lv_budat) = COND budat( WHEN p_datum IS NOT INITIAL THEN p_datum ELSE p_date ).

    " Kulcs: MANDT + BUKRS + COMPNO + SIDE + LINENO_COMP
    DATA(ls_head) = VALUE yyfi_comp_log(
      bukrs     = p_bukrs
      compno    = iv_compno
      direction = COND #( WHEN mv_c2v = abap_true THEN 'C' ELSE 'V' )
      comp_date = lv_budat
      comp_text = p_text
      kunnr     = p_kunnr
      lifnr     = p_lifnr
      ernam     = sy-uname
      erdat     = sy-datum
      erzet     = sy-uzeit
      hwaer     = lv_hwaer
      langu     = COND #( WHEN mv_langu IS INITIAL THEN sy-langu ELSE mv_langu ) ).

    " Csak a párosított (zöld) sorok kerülnek mentésre
    LOOP AT mt_bsid ASSIGNING FIELD-SYMBOL(<d>) WHERE cmp_stat = 'G'.
      lv_cnt = lv_cnt + 1.
      APPEND VALUE #( BASE ls_head
        compno = iv_compno
        side = 'C' lineno_comp = lv_cnt
        gjahr = <d>-gjahr belnr = <d>-belnr buzei = <d>-buzei
        zuonr = <d>-zuonr shkzg = <d>-shkzg budat = <d>-budat
        wrbtr = <d>-wrbtr waers = <d>-waers dmbtr = <d>-dmbtr
        xblnr = <d>-xblnr bldat = <d>-bldat zfbdt = <d>-zfbdt
        hkont = <d>-hkont xref3 = <d>-xref3
        cmp_stat = <d>-cmp_stat ) TO lt_log.
    ENDLOOP.

    CLEAR lv_cnt.
    LOOP AT mt_bsik ASSIGNING FIELD-SYMBOL(<k>) WHERE cmp_stat = 'G'.
      lv_cnt = lv_cnt + 1.
      APPEND VALUE #( BASE ls_head
        compno = iv_compno
        side = 'V' lineno_comp = lv_cnt
        gjahr = <k>-gjahr belnr = <k>-belnr buzei = <k>-buzei
        zuonr = <k>-zuonr shkzg = <k>-shkzg budat = <k>-budat
        wrbtr = <k>-wrbtr waers = <k>-waers dmbtr = <k>-dmbtr
        xblnr = <k>-xblnr bldat = <k>-bldat zfbdt = <k>-zfbdt
        hkont = <k>-hkont xref3 = <k>-xref3
        cmp_stat = <k>-cmp_stat ) TO lt_log.
    ENDLOOP.

    IF lt_log IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        INSERT yyfi_comp_log FROM TABLE @lt_log.
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql).
        ROLLBACK WORK.
        MESSAGE lx_sql TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    IF sy-subrc = 0.
      COMMIT WORK.
      rv_ok = abap_true.
    ELSE.
      ROLLBACK WORK.
      MESSAGE 'Mentés a Z táblába sikertelen' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD save_run.

    rv_ok = abap_false.

    " 1. Volt-e összehasonlítás?
    IF mv_matched = abap_false.
      MESSAGE 'Előbb jelöld ki és hasonlítsd össze a vevői és szállítói tételeket' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " 2. Van-e zöld (egyező) sor mindkét oldalon?
    "    (a sárga sorok nem kerülnek mentésre, nem akadályozzák a mentést;
    "     az egyenleget pénznemenként a clear_items szimuláció ellenőrizte)
    IF NOT line_exists( mt_bsid[ cmp_stat = 'G' ] ) OR
       NOT line_exists( mt_bsik[ cmp_stat = 'G' ] ).
      MESSAGE 'Nincs egyező vevői és szállítói tétel a mentéshez' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    rv_ok = save_log( iv_compno ).

  ENDMETHOD.

  METHOD new_compno.
    " MMDD + HHMMSS = 10 karakter. Később számkörből (SNRO) érdemes venni.
    DO 10 TIMES.
      rv_compno = |{ sy-datum+4(4) }{ sy-uzeit TIME = RAW }|.
      SELECT SINGLE compno FROM yyfi_comp_log
        WHERE bukrs = @p_bukrs AND compno = @rv_compno
        INTO @DATA(lv_dummy).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      WAIT UP TO 1 SECONDS.
    ENDDO.
    CLEAR rv_compno.
  ENDMETHOD.

  METHOD pdf_path.
    rv_path = |{ gc_pdf_dir }COMP_{ p_bukrs }_{ iv_compno }.pdf|.
  ENDMETHOD.

*&---------------------------------------------------------------*
*& A két belső tábla (zöld sorok) PDF-be, az app szerverre
*&---------------------------------------------------------------*
  METHOD write_pdf.

    TYPES: BEGIN OF ty_tot,
             waers TYPE waers,
             amt   TYPE wrbtr,
           END OF ty_tot.

    DATA: lt_lines   TYPE lcl_pdf=>tt_lines,
          lt_tot_c   TYPE STANDARD TABLE OF ty_tot WITH DEFAULT KEY,
          lt_tot_v   TYPE STANDARD TABLE OF ty_tot WITH DEFAULT KEY,
          ls_tot     TYPE ty_tot,
          lv_line    TYPE string,
          lt_pdfout  TYPE STANDARD TABLE OF solix,
          lv_pdfsize TYPE i,
          lv_pdf     TYPE xstring.

    ev_ok = abap_false.

    DATA(lv_file)  = pdf_path( iv_compno ).
    DATA(lv_dir)   = COND string( WHEN mv_v2c = abap_true THEN `Vendor -> Customer`
                                  ELSE `Customer -> Vendor` ).
    DATA(lv_budat) = COND budat( WHEN p_datum IS NOT INITIAL THEN p_datum ELSE p_date ).

    " --- Fejléc ---
    APPEND |COMPENSATION PROPOSAL (TEST PDF)| TO lt_lines.
    APPEND || TO lt_lines.
    APPEND |Company code : { p_bukrs }     Comp. no.: { iv_compno }     Date: { lv_budat DATE = ISO }| TO lt_lines.
    APPEND |Direction    : { lv_dir }| TO lt_lines.
    APPEND |Customer     : { p_kunnr }     Vendor: { p_lifnr }| TO lt_lines.
    APPEND |Text         : { p_text }| TO lt_lines.
    APPEND || TO lt_lines.

    " --- Vevői tételek (BSID) ---
    APPEND |CUSTOMER ITEMS (BSID)| TO lt_lines.
    APPEND `Document    Year  Itm  Reference         Doc.date    Base date   Crcy           Amount D/C` TO lt_lines.
    LOOP AT it_bsid INTO DATA(ls_d).
      lv_line = |{ ls_d-belnr WIDTH = 12 }{ ls_d-gjahr WIDTH = 6 }{ ls_d-buzei WIDTH = 5 }{ ls_d-xblnr WIDTH = 18 }| &&
                |{ ls_d-bldat DATE = ISO WIDTH = 12 }{ ls_d-zfbdt DATE = ISO WIDTH = 12 }{ ls_d-waers WIDTH = 5 }| &&
                |{ ls_d-wrbtr CURRENCY = ls_d-waers NUMBER = USER WIDTH = 16 ALIGN = RIGHT } { ls_d-shkzg }|.
      APPEND lv_line TO lt_lines.
      ls_tot = VALUE #( waers = ls_d-waers amt = abs( ls_d-wrbtr ) ).
      COLLECT ls_tot INTO lt_tot_c.
    ENDLOOP.
    LOOP AT lt_tot_c INTO ls_tot.
      lv_line = |Total (abs): { ls_tot-waers } { ls_tot-amt CURRENCY = ls_tot-waers NUMBER = USER }|.
      APPEND lv_line TO lt_lines.
    ENDLOOP.
    APPEND || TO lt_lines.

    " --- Szállítói tételek (BSIK) ---
    APPEND |VENDOR ITEMS (BSIK)| TO lt_lines.
    APPEND `Document    Year  Itm  Reference         Doc.date    Base date   Crcy           Amount D/C` TO lt_lines.
    LOOP AT it_bsik INTO DATA(ls_k).
      lv_line = |{ ls_k-belnr WIDTH = 12 }{ ls_k-gjahr WIDTH = 6 }{ ls_k-buzei WIDTH = 5 }{ ls_k-xblnr WIDTH = 18 }| &&
                |{ ls_k-bldat DATE = ISO WIDTH = 12 }{ ls_k-zfbdt DATE = ISO WIDTH = 12 }{ ls_k-waers WIDTH = 5 }| &&
                |{ ls_k-wrbtr CURRENCY = ls_k-waers NUMBER = USER WIDTH = 16 ALIGN = RIGHT } { ls_k-shkzg }|.
      APPEND lv_line TO lt_lines.
      ls_tot = VALUE #( waers = ls_k-waers amt = abs( ls_k-wrbtr ) ).
      COLLECT ls_tot INTO lt_tot_v.
    ENDLOOP.
    LOOP AT lt_tot_v INTO ls_tot.
      lv_line = |Total (abs): { ls_tot-waers } { ls_tot-amt CURRENCY = ls_tot-waers NUMBER = USER }|.
      APPEND lv_line TO lt_lines.
    ENDLOOP.

    " --- PDF xstring -> bináris tábla ---
    lv_pdf = lcl_pdf=>build( lt_lines ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_pdf
      IMPORTING
        output_length = lv_pdfsize
      TABLES
        binary_tab    = lt_pdfout.

    TRY.
        OPEN DATASET lv_file FOR OUTPUT IN BINARY MODE.
        IF sy-subrc <> 0.
          MESSAGE |Cannot open file on the application server: { lv_file }| TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

* Write PDF to application server
        LOOP AT lt_pdfout INTO DATA(ls_pdfout).
          TRANSFER ls_pdfout-line TO lv_file.
        ENDLOOP.

* Write only correct length to output
        TRUNCATE DATASET lv_file AT POSITION lv_pdfsize.

        CLOSE DATASET lv_file.

      CATCH cx_sy_file_access_error INTO DATA(lx_file).
        MESSAGE lx_file TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    ev_ok = abap_true.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& PDF visszaolvasása az app szerverről (a compno adja a fájlnevet)
*&---------------------------------------------------------------*
  METHOD read_pdf.

    CLEAR: ev_pdf, ev_ok.

    DATA(lv_file) = pdf_path( iv_compno ).

    TRY.
        OPEN DATASET lv_file FOR INPUT IN BINARY MODE.
        IF sy-subrc <> 0.
          MESSAGE |PDF file not found on the application server: { lv_file }| TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        READ DATASET lv_file INTO ev_pdf.
        CLOSE DATASET lv_file.

      CATCH cx_sy_file_access_error INTO DATA(lx_file).
        MESSAGE lx_file TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    IF ev_pdf IS INITIAL.
      MESSAGE |PDF file is empty: { lv_file }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    ev_ok = abap_true.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Mentett kompenzáció betöltése a Z táblából (P_COMPNO alapján)
*&---------------------------------------------------------------*
  METHOD load_run.

    DATA: ls_item  TYPE ty_prop_item,
          lv_cnt   TYPE i,
          lv_sum_c TYPE p LENGTH 13 DECIMALS 2,
          lv_sum_v TYPE p LENGTH 13 DECIMALS 2,
          lv_waers TYPE waers.

    rv_ok = abap_false.
    CLEAR: mt_prop_hdr, mt_prop_items, mv_compno, mv_dir, mv_cust, mv_vend,
           mv_from_log, mv_mail_sent, mv_posted.

    SELECT * FROM yyfi_comp_log
      WHERE bukrs = @p_bukrs AND compno = @iv_compno
      INTO TABLE @DATA(lt_log).
    IF sy-subrc <> 0.
      MESSAGE |No saved compensation found: { p_bukrs } / { iv_compno }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    SORT lt_log BY side lineno_comp.

    SELECT SINGLE waers FROM t001
      WHERE bukrs = @p_bukrs
      INTO @lv_waers.

    DATA(ls_first) = lt_log[ 1 ].

    LOOP AT lt_log INTO DATA(ls_l).
      lv_cnt = lv_cnt + 1.
      CLEAR ls_item.
      ls_item-bukrs  = ls_l-bukrs.
      ls_item-compno = iv_compno.
      ls_item-cnt    = lv_cnt.
      IF ls_l-side = 'C'.
        ls_item-kunnr = ls_l-kunnr.
      ELSE.
        ls_item-lifnr = ls_l-lifnr.
      ENDIF.
      ls_item-zuonr  = ls_l-zuonr.
      ls_item-amount = COND #( WHEN ls_l-shkzg = 'H' THEN ls_l-dmbtr ELSE ls_l-dmbtr * -1 ).
      ls_item-gjahr  = ls_l-gjahr.
      ls_item-belnr  = ls_l-belnr.
      ls_item-buzei  = ls_l-buzei.
      ls_item-zfbdt  = ls_l-zfbdt.
      ls_item-shkzg  = ls_l-shkzg.
      ls_item-hkont  = ls_l-hkont.
      ls_item-xref3  = ls_l-xref3.
      APPEND ls_item TO mt_prop_items.
      IF ls_l-side = 'C'.
        lv_sum_c = lv_sum_c + ls_item-amount.
      ELSE.
        lv_sum_v = lv_sum_v + ls_item-amount.
      ENDIF.
    ENDLOOP.

    APPEND VALUE #( bukrs  = p_bukrs
                    compno = iv_compno
                    lifnr  = ls_first-lifnr
                    kunnr  = ls_first-kunnr
                    datum  = ls_first-comp_date
                    amount = nmin( val1 = abs( lv_sum_c ) val2 = abs( lv_sum_v ) )
                    waers  = lv_waers
                    text   = ls_first-comp_text ) TO mt_prop_hdr.

    mv_compno   = iv_compno.
    mv_dir      = ls_first-direction.
    mv_cust     = ls_first-kunnr.
    mv_vend     = ls_first-lifnr.
    mv_from_log = abap_true.
    mv_langu     = COND #( WHEN ls_first-langu IS INITIAL THEN sy-langu ELSE ls_first-langu ).
    mv_mail_sent = xsdbool( ls_first-mail_stat IS NOT INITIAL ).
    mv_posted    = xsdbool( ls_first-post_stat = 'X' ).
    set_overview_colors( ).
    rv_ok       = abap_true.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& SO10 standard text (ID ST, object TEXT) in the language of the run,
*& fallback: English
*&---------------------------------------------------------------*
  METHOD read_so10.

    DATA lt_langu TYPE STANDARD TABLE OF sy-langu WITH EMPTY KEY.

    lt_langu = VALUE #( ( mv_langu ) ( 'E' ) ).

    LOOP AT lt_langu INTO DATA(lv_langu).
      CLEAR rt_lines.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = 'ST'
          language = lv_langu
          name     = iv_name
          object   = 'TEXT'
        TABLES
          lines    = rt_lines
        EXCEPTIONS
          OTHERS   = 1.
      IF sy-subrc = 0 AND rt_lines IS NOT INITIAL.
        RETURN.
      ENDIF.
    ENDLOOP.

    CLEAR rt_lines.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Mail subject and HTML body from the SO10 texts
*&   ZFI_COMP_PROP_SUBJ / ZFI_COMP_PROP_BODY  (proposal)
*&   ZFI_COMP_DONE_SUBJ / ZFI_COMP_DONE_BODY  (compensation completed)
*&   Symbols in the text: &COMPNO& &BUKRS& &BUTXT& &DATE& &AMOUNT& ...
*&   Line starting with 2 spaces = indented line; empty line = gap.
*&---------------------------------------------------------------*
  METHOD get_mail_text.

    DATA: lv_line TYPE string,
          lv_tok  TYPE string,
          lv_indx TYPE i.

    CLEAR: ev_subject, et_body, ev_found.

    DATA(lv_kind) = COND string( WHEN iv_posted = abap_true THEN `DONE` ELSE `PROP` ).

    DATA(lt_subj) = read_so10( CONV tdobname( |ZFI_COMP_{ lv_kind }_SUBJ| ) ).
    DATA(lt_text) = read_so10( CONV tdobname( |ZFI_COMP_{ lv_kind }_BODY| ) ).
    IF lt_subj IS INITIAL OR lt_text IS INITIAL.
      RETURN.
    ENDIF.

    " --- Subject: first non-empty line, plain text ---
    LOOP AT lt_subj INTO DATA(ls_s) WHERE tdline IS NOT INITIAL AND tdformat <> '/*'.
      lv_line = ls_s-tdline.
      LOOP AT it_symbols INTO DATA(ls_sym).
        REPLACE ALL OCCURRENCES OF |&{ ls_sym-name }&| IN lv_line WITH ls_sym-value.
      ENDLOOP.
      ev_subject = lv_line.
      EXIT.
    ENDLOOP.

    " --- Body: HTML ---
    DATA(lv_color) = COND string( WHEN iv_posted = abap_true THEN `#107e3e` ELSE `#0a6ed1` ).

    APPEND VALUE #( line = |<html><body style="font-family:Calibri,Arial,sans-serif;font-size:11pt;color:#222;">| ) TO et_body.
    APPEND VALUE #( line = |<div style="background:{ lv_color };color:#ffffff;padding:8px 14px;font-size:14pt;">| ) TO et_body.
    APPEND VALUE #( line = |<b>{ escape( val = CONV string( ev_subject ) format = cl_abap_format=>e_html_text ) }</b></div>| ) TO et_body.

    LOOP AT lt_text INTO DATA(ls_t) WHERE tdformat <> '/*' AND tdformat <> '/:'.

      lv_line = ls_t-tdline.

      IF lv_line IS INITIAL OR lv_line CO ` `.
        APPEND VALUE #( line = |<br>| ) TO et_body.
        CONTINUE.
      ENDIF.

      DATA(lv_indent) = xsdbool( strlen( lv_line ) > 1 AND lv_line(2) = '  ' ).
      SHIFT lv_line LEFT DELETING LEADING space.

      " symbols -> tokens, then escape the static text, then the values (bold)
      lv_indx = 0.
      LOOP AT it_symbols INTO ls_sym.
        lv_indx = lv_indx + 1.
        lv_tok  = |@@{ lv_indx }@@|.
        REPLACE ALL OCCURRENCES OF |&{ ls_sym-name }&| IN lv_line WITH lv_tok.
      ENDLOOP.

      lv_line = escape( val = lv_line format = cl_abap_format=>e_html_text ).

      lv_indx = 0.
      LOOP AT it_symbols INTO ls_sym.
        lv_indx = lv_indx + 1.
        lv_tok  = |@@{ lv_indx }@@|.
        REPLACE ALL OCCURRENCES OF lv_tok IN lv_line
          WITH |<b>{ escape( val = ls_sym-value format = cl_abap_format=>e_html_text ) }</b>|.
      ENDLOOP.

      IF lv_indent = abap_true.
        APPEND VALUE #( line = |<div style="margin:0 0 2px 24px;">{ lv_line }</div>| ) TO et_body.
      ELSE.
        APPEND VALUE #( line = |<div style="margin:0 0 4px 0;">{ lv_line }</div>| ) TO et_body.
      ENDIF.

    ENDLOOP.

    APPEND VALUE #( line = |</body></html>| ) TO et_body.

    ev_found = xsdbool( ev_subject IS NOT INITIAL ).

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Confirmation popup (Yes / No, default No)
*&---------------------------------------------------------------*
  METHOD confirm.

    DATA: lv_answer TYPE c LENGTH 1,
          lv_title  TYPE c LENGTH 80,
          lv_text   TYPE c LENGTH 200.

    lv_title = iv_title.
    lv_text  = iv_question.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = lv_title
        text_question         = lv_text
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    rv_yes = xsdbool( sy-subrc = 0 AND lv_answer = '1' ).

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Matched (green) items of the current selection
*&---------------------------------------------------------------*
  METHOD get_matched_items.

    rt_items = VALUE #(
      FOR ls_d IN mt_bsid WHERE ( cmp_stat = 'G' )
      ( kind = 'D' belnr = ls_d-belnr gjahr = ls_d-gjahr buzei = ls_d-buzei
        shkzg = ls_d-shkzg wrbtr = ls_d-wrbtr waers = ls_d-waers ) ).

    rt_items = VALUE #( BASE rt_items
      FOR ls_k IN mt_bsik WHERE ( cmp_stat = 'G' )
      ( kind = 'K' belnr = ls_k-belnr gjahr = ls_k-gjahr buzei = ls_k-buzei
        shkzg = ls_k-shkzg wrbtr = ls_k-wrbtr waers = ls_k-waers ) ).

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Items of the loaded (saved) run that are not cleared yet
*&---------------------------------------------------------------*
  METHOD get_logged_items.

    SELECT * FROM yyfi_comp_log
      WHERE bukrs = @p_bukrs
        AND compno = @mv_compno
        AND clr_belnr = @space
      INTO TABLE @DATA(lt_log).

    SORT lt_log BY side lineno_comp.

    rt_items = VALUE #(
      FOR ls_l IN lt_log
      ( kind  = COND #( WHEN ls_l-side = 'C' THEN 'D' ELSE 'K' )
        belnr = ls_l-belnr
        gjahr = ls_l-gjahr
        buzei = ls_l-buzei
        shkzg = ls_l-shkzg
        wrbtr = ls_l-wrbtr
        waers = ls_l-waers ) ).

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Are the saved items still open? (days may have passed)
*&---------------------------------------------------------------*
  METHOD check_items_open.

    DATA lv_belnr TYPE belnr_d.

    rv_ok = abap_true.

    LOOP AT it_items INTO DATA(ls_item).
      CLEAR lv_belnr.
      IF ls_item-kind = 'D'.
        SELECT SINGLE belnr FROM bsid
          WHERE bukrs = @p_bukrs AND gjahr = @ls_item-gjahr
            AND belnr = @ls_item-belnr AND buzei = @ls_item-buzei
          INTO @lv_belnr.
      ELSE.
        SELECT SINGLE belnr FROM bsik
          WHERE bukrs = @p_bukrs AND gjahr = @ls_item-gjahr
            AND belnr = @ls_item-belnr AND buzei = @ls_item-buzei
          INTO @lv_belnr.
      ENDIF.
      IF sy-subrc <> 0.
        MESSAGE |Item { ls_item-belnr }/{ ls_item-gjahr }/{ ls_item-buzei } is no longer open (already cleared?)| TYPE 'S' DISPLAY LIKE 'E'.
        rv_ok = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Posting of a set of items, one clearing per currency
*&   et_posted contains only the items of successfully posted currencies
*&---------------------------------------------------------------*
  METHOD post_items.

    DATA lt_cur TYPE SORTED TABLE OF waers WITH UNIQUE KEY table_line.

    CLEAR: ev_ok, et_posted.

    LOOP AT it_items INTO DATA(ls_all).
      INSERT ls_all-waers INTO TABLE lt_cur.
    ENDLOOP.

    ev_ok = abap_true.

    LOOP AT lt_cur INTO DATA(lv_cur).

      DATA(lt_cur_items) = VALUE lcl_kd_comp=>tt_item(
        FOR ls_x IN it_items WHERE ( waers = lv_cur ) ( ls_x ) ).

      DATA(ls_res) = lcl_kd_comp=>clear_items(
                       iv_bukrs   = p_bukrs
                       iv_kunnr   = iv_kunnr
                       iv_lifnr   = iv_lifnr
                       iv_budat   = iv_budat
                       iv_blart   = gc_blart
                       iv_mode    = 'N'
                       iv_test    = iv_test
                       iv_zlspr_d = gc_zlspr_d
                       iv_zlspr_k = gc_zlspr_k
                       it_items   = lt_cur_items ).

      IF ls_res-success = abap_false.
        ev_ok = abap_false.
        MESSAGE |{ lv_cur }: { ls_res-msg }| TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.      " do not continue with the next currency
      ENDIF.

      IF iv_test = abap_true.
        MESSAGE |{ ls_res-msg }| TYPE 'S'.
        CONTINUE.
      ENDIF.

      LOOP AT lt_cur_items INTO DATA(ls_it).
        APPEND VALUE #( side      = COND #( WHEN ls_it-kind = 'D' THEN 'C' ELSE 'V' )
                        belnr     = ls_it-belnr
                        gjahr     = ls_it-gjahr
                        buzei     = ls_it-buzei
                        shkzg     = ls_it-shkzg
                        wrbtr     = ls_it-wrbtr
                        waers     = ls_it-waers
                        clr_belnr = ls_res-belnr
                        clr_gjahr = ls_res-gjahr ) TO et_posted.
      ENDLOOP.

      IF ls_res-blk_msg IS NOT INITIAL.
        MESSAGE |{ lv_cur }: clearing document { ls_res-belnr } / { ls_res-gjahr } posted, payment block NOT changed: { ls_res-blk_msg }| TYPE 'S' DISPLAY LIKE 'W'.
      ELSE.
        MESSAGE |{ lv_cur }: clearing document { ls_res-belnr } / { ls_res-gjahr }| TYPE 'S'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Simulation / posting of the matched (green) items of the current selection
*&   (used by CREATE PROPOSAL for the simulation)
*&---------------------------------------------------------------*
  METHOD run_clearing.

    rv_ok = abap_false.

    DATA(lt_items) = get_matched_items( ).

    IF NOT line_exists( lt_items[ kind = 'D' ] ) OR NOT line_exists( lt_items[ kind = 'K' ] ).
      MESSAGE 'There are no matched (green) customer and vendor items' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    post_items( EXPORTING it_items  = lt_items
                         iv_test   = iv_test
                         iv_kunnr  = p_kunnr
                         iv_lifnr  = p_lifnr
                         iv_budat  = COND #( WHEN p_datum IS NOT INITIAL THEN p_datum ELSE p_date )
                IMPORTING ev_ok    = rv_ok
                          et_posted = DATA(lt_posted) ).

    IF iv_test = abap_false.
      remove_posted_items( lt_posted ).
    ENDIF.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Posted items are removed from the open item lists
*&---------------------------------------------------------------*
  METHOD remove_posted_items.

    LOOP AT it_posted INTO DATA(ls_p).
      IF ls_p-side = 'C'.
        DELETE mt_bsid WHERE bukrs = p_bukrs AND belnr = ls_p-belnr
                         AND gjahr = ls_p-gjahr AND buzei = ls_p-buzei.
      ELSE.
        DELETE mt_bsik WHERE bukrs = p_bukrs AND belnr = ls_p-belnr
                         AND gjahr = ls_p-gjahr AND buzei = ls_p-buzei.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Small popup: which items were posted
*&---------------------------------------------------------------*
  METHOD show_posted_popup.

    DATA(lt_popup) = it_posted.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_popup)
                                CHANGING  t_table      = lt_popup ).

        lo_popup->get_functions( )->set_all( abap_true ).
        lo_popup->get_display_settings( )->set_list_header( 'Posted items' ).

        DATA(lo_cols) = lo_popup->get_columns( ).
        lo_cols->set_optimize( abap_true ).
        lo_cols->get_column( 'SIDE'      )->set_short_text( 'Side' ).
        lo_cols->get_column( 'CLR_BELNR' )->set_medium_text( 'Clearing doc.' ).
        lo_cols->get_column( 'CLR_GJAHR' )->set_medium_text( 'Clearing year' ).

        lo_popup->set_screen_popup( start_column = 10
                                    end_column   = 120
                                    start_line   = 4
                                    end_line     = 22 ).
        lo_popup->display( ).

      CATCH cx_salv_error INTO DATA(lx_err).
        MESSAGE lx_err TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Posting result into the Z table (clearing documents + status)
*&---------------------------------------------------------------*
  METHOD save_posting.

    DATA(lv_status) = COND char1( WHEN iv_complete = abap_true THEN 'X' ELSE 'P' ).

    LOOP AT it_posted INTO DATA(ls_p).
      UPDATE yyfi_comp_log
        SET clr_belnr = @ls_p-clr_belnr,
            clr_gjahr = @ls_p-clr_gjahr
        WHERE bukrs = @p_bukrs AND compno = @mv_compno
          AND belnr = @ls_p-belnr AND gjahr = @ls_p-gjahr AND buzei = @ls_p-buzei.
    ENDLOOP.

    UPDATE yyfi_comp_log
      SET post_stat = @lv_status,
          post_date = @sy-datum,
          post_user = @sy-uname
      WHERE bukrs = @p_bukrs AND compno = @mv_compno.

    COMMIT WORK.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Recipient: test mode = gc_mail_to, otherwise the e-mail of the
*& relevant partner (C2V: customer, V2C: vendor), table ADR6
*&---------------------------------------------------------------*
  METHOD get_recipient.

    DATA lv_adrnr TYPE adrnr.

    IF gc_mail_test_mode = abap_true.
      rv_mail = gc_mail_to.
      RETURN.
    ENDIF.

    IF mv_dir = 'V'.
      SELECT SINGLE adrnr FROM lfa1 WHERE lifnr = @mv_vend INTO @lv_adrnr.
    ELSE.
      SELECT SINGLE adrnr FROM kna1 WHERE kunnr = @mv_cust INTO @lv_adrnr.
    ENDIF.
    IF lv_adrnr IS INITIAL.
      RETURN.
    ENDIF.

    SELECT smtp_addr, flgdefault, consnumber FROM adr6
      WHERE addrnumber = @lv_adrnr AND persnumber = @space
      INTO TABLE @DATA(lt_mail).

    SORT lt_mail BY flgdefault DESCENDING consnumber ASCENDING.
    READ TABLE lt_mail INTO DATA(ls_mail) INDEX 1.
    IF sy-subrc = 0.
      rv_mail = ls_mail-smtp_addr.
    ENDIF.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Overview colors: yellow = proposal mail sent, green = posted
*&---------------------------------------------------------------*
  METHOD set_overview_colors.

    DATA lt_color TYPE lvc_t_scol.

    IF mv_posted = abap_true.
      lt_color = VALUE #( ( color-col = 5 color-int = 1 color-inv = 0 ) ).   " green
    ELSEIF mv_mail_sent = abap_true.
      lt_color = VALUE #( ( color-col = 3 color-int = 1 color-inv = 0 ) ).   " yellow
    ENDIF.

    LOOP AT mt_prop_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item>-color = lt_color.
    ENDLOOP.
    LOOP AT mt_prop_hdr ASSIGNING FIELD-SYMBOL(<ls_hdr>).
      <ls_hdr>-color = lt_color.
    ENDLOOP.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& POST button of the overview (current selection or a saved run)
*&---------------------------------------------------------------*
  METHOD post_from_overview.

    DATA: lt_items TYPE lcl_kd_comp=>tt_item,
          lv_kunnr TYPE kunnr,
          lv_lifnr TYPE lifnr,
          lv_budat TYPE budat.

    " 1. Authorization
    DATA(lv_auth_msg) = lcl_auth=>check_posting( p_bukrs ).
    IF lv_auth_msg IS NOT INITIAL.
      MESSAGE lv_auth_msg TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " 2. Items
    IF mv_from_log = abap_true.
      IF mv_posted = abap_true.
        MESSAGE |Compensation { mv_compno } has already been posted| TYPE 'S' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.
      lt_items = get_logged_items( ).
      lv_kunnr = mv_cust.
      lv_lifnr = mv_vend.
      lv_budat = COND #( WHEN p_datum IS NOT INITIAL THEN p_datum ELSE sy-datum ).
    ELSE.
      lt_items = get_matched_items( ).
      lv_kunnr = p_kunnr.
      lv_lifnr = p_lifnr.
      lv_budat = COND #( WHEN p_datum IS NOT INITIAL THEN p_datum ELSE p_date ).
    ENDIF.

    IF NOT line_exists( lt_items[ kind = 'D' ] ) OR NOT line_exists( lt_items[ kind = 'K' ] ).
      MESSAGE 'There are no customer and vendor items to post' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF mv_from_log = abap_true AND check_items_open( lt_items ) = abap_false.
      RETURN.
    ENDIF.

    " 3. Confirmation
    DATA(lv_question) = COND string(
      WHEN mv_from_log = abap_true
      THEN |Post compensation { mv_compno } ({ lines( lt_items ) } items)?|
      ELSE |Post the matched items ({ lines( lt_items ) } items)?| ).

    IF mv_from_log = abap_true AND mv_mail_sent = abap_false.
      lv_question = |{ lv_question } The proposal has not been sent to the partner yet.|.
    ENDIF.

    IF confirm( iv_title    = 'Post compensation'
                iv_question = lv_question ) = abap_false.
      RETURN.
    ENDIF.

    " 4. Posting
    post_items( EXPORTING it_items  = lt_items
                         iv_test   = abap_false
                         iv_kunnr  = lv_kunnr
                         iv_lifnr  = lv_lifnr
                         iv_budat  = lv_budat
                IMPORTING ev_ok    = DATA(lv_ok)
                          et_posted = DATA(lt_posted) ).

    " 5. Posted items: list cleanup + popup
    IF lt_posted IS NOT INITIAL.
      remove_posted_items( lt_posted ).
      do_refresh( ).
      show_posted_popup( lt_posted ).
    ENDIF.

    " 6. Follow-up
    IF mv_from_log = abap_true.
      IF lt_posted IS NOT INITIAL.
        save_posting( it_posted   = lt_posted
                      iv_complete = lv_ok ).
      ENDIF.
      IF lv_ok = abap_true.
        mv_posted = abap_true.
        set_overview_colors( ).
        IF mo_alv_items IS BOUND.
          mo_alv_items->refresh( refresh_mode = if_salv_c_refresh=>soft ).
          mo_alv_hdr->refresh( refresh_mode = if_salv_c_refresh=>soft ).
        ENDIF.
        " automatic completion mail
        send_mail( iv_posted = abap_true ).
      ENDIF.
    ELSEIF lv_ok = abap_true.
      free_prop( ).
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Mail with the PDF
*&   iv_posted = false : proposal (asks for confirmation when it was sent before)
*&   iv_posted = true  : completion mail (compensation posted)
*&---------------------------------------------------------------*
  METHOD send_mail.

    TYPES: BEGIN OF ty_row,
             label TYPE string,
             value TYPE string,
             bold  TYPE abap_bool,
           END OF ty_row,
           tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

    DATA: lt_body    TYPE soli_tab,
          lv_subject TYPE so_obj_des,
          lv_attname TYPE so_obj_des,
          lv_docs    TYPE string.

    IF mv_compno IS INITIAL.
      MESSAGE 'Open a saved compensation first: enter P_COMPNO in the Overview popup' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " Proposal: not after posting, and ask before resending
    IF iv_posted = abap_false.
      IF mv_posted = abap_true.
        MESSAGE 'This compensation has already been posted; the proposal cannot be sent any more' TYPE 'S' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.
      IF mv_mail_sent = abap_true.
        IF confirm( iv_title    = 'Resend proposal'
                    iv_question = |Proposal { mv_compno } has already been sent. Do you really want to send it again?| ) = abap_false.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    " Recipient
    DATA(lv_to) = get_recipient( ).
    IF lv_to IS INITIAL.
      MESSAGE |No e-mail address is maintained for { COND string( WHEN mv_dir = 'V' THEN |vendor { mv_vend }| ELSE |customer { mv_cust }| ) }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    read_pdf( EXPORTING iv_compno = mv_compno
              IMPORTING ev_pdf    = DATA(lv_pdf)
                        ev_ok     = DATA(lv_ok) ).
    IF lv_ok = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE butxt FROM t001 WHERE bukrs = @p_bukrs INTO @DATA(lv_butxt).
    SELECT SINGLE name1 FROM kna1 WHERE kunnr = @mv_cust INTO @DATA(lv_cname).
    SELECT SINGLE name1 FROM lfa1 WHERE lifnr = @mv_vend INTO @DATA(lv_vname).

    READ TABLE mt_prop_hdr INTO DATA(ls_hdr) INDEX 1.

    DATA(lv_dirtxt)  = COND string( WHEN mv_dir = 'V' THEN `Vendor -> Customer`
                                    ELSE `Customer -> Vendor` ).
    DATA(lv_partner) = COND string( WHEN mv_dir = 'V' THEN |Vendor { mv_vend } { lv_vname }|
                                    ELSE |Customer { mv_cust } { lv_cname }| ).

    " Texts and colors of the two variants
    DATA(lv_bar_color) = COND string( WHEN iv_posted = abap_true THEN `#107e3e` ELSE `#0a6ed1` ).
    DATA(lv_title)     = COND string( WHEN iv_posted = abap_true THEN `Compensation completed`
                                      ELSE `Compensation proposal` ).
    DATA(lv_intro)     = COND string( WHEN iv_posted = abap_true
                                      THEN `The compensation has been successfully posted; the details are in the attached PDF.`
                                      ELSE `A compensation proposal has been prepared; the details are in the attached PDF.` ).

    lv_subject = |{ lv_title } { mv_compno } - { p_bukrs } { lv_butxt }|.
    lv_attname = |Compensation_{ p_bukrs }_{ mv_compno }|.

    " Clearing documents (completion mail)
    IF iv_posted = abap_true.
      SELECT DISTINCT clr_belnr, clr_gjahr, waers FROM yyfi_comp_log
        WHERE bukrs = @p_bukrs AND compno = @mv_compno AND clr_belnr <> @space
        INTO TABLE @DATA(lt_clr).
      LOOP AT lt_clr INTO DATA(ls_clr).
        lv_docs = COND #( WHEN lv_docs IS INITIAL
                          THEN |{ ls_clr-clr_belnr }/{ ls_clr-clr_gjahr } ({ ls_clr-waers })|
                          ELSE |{ lv_docs }, { ls_clr-clr_belnr }/{ ls_clr-clr_gjahr } ({ ls_clr-waers })| ).
      ENDLOOP.
    ENDIF.

    " --- Rows: label + value (bold = bold value) ---
    DATA(lt_rows) = VALUE tt_row(
      ( label = `Company code`      value = |{ p_bukrs } { lv_butxt }| )
      ( label = `Compensation no.`  value = |{ mv_compno }| )
      ( label = `Direction`         value = lv_dirtxt )
      ( label = `Relevant partner`  value = lv_partner bold = abap_true )
      ( label = `Customer`          value = |{ mv_cust } { lv_cname }| )
      ( label = `Vendor`            value = |{ mv_vend } { lv_vname }| )
      ( label = `Compensation date` value = |{ ls_hdr-datum DATE = USER }| )
      ( label = `Amount`            value = |{ ls_hdr-amount NUMBER = USER } { ls_hdr-waers }| bold = abap_true )
      ( label = `Number of items`   value = |{ lines( mt_prop_items ) }| ) ).

    IF iv_posted = abap_true.
      APPEND VALUE #( label = `Clearing document(s)` value = lv_docs bold = abap_true ) TO lt_rows.
    ENDIF.

    " --- HTML body: every line separate, under 255 characters ---
    APPEND VALUE #( line = |<html><body style="font-family:Calibri,Arial,sans-serif;font-size:11pt;color:#222;">| ) TO lt_body.
    APPEND VALUE #( line = |<div style="background:{ lv_bar_color };color:#ffffff;padding:8px 14px;font-size:14pt;">| ) TO lt_body.
    APPEND VALUE #( line = |<b>{ lv_title }</b></div>| ) TO lt_body.
    APPEND VALUE #( line = |<p>Dear colleague,</p>| ) TO lt_body.
    APPEND VALUE #( line = |<p>{ lv_intro }</p>| ) TO lt_body.
    APPEND VALUE #( line = |<table style="border-collapse:collapse;margin-left:20px;">| ) TO lt_body.

    LOOP AT lt_rows INTO DATA(ls_row).
      DATA(lv_lab) = escape( val = ls_row-label format = cl_abap_format=>e_html_text ).
      DATA(lv_val) = escape( val = ls_row-value format = cl_abap_format=>e_html_text ).
      IF ls_row-bold = abap_true.
        lv_val = |<b>{ lv_val }</b>|.
      ENDIF.
      APPEND VALUE #( line = |<tr><td style="padding:3px 24px 3px 0;color:#555555;">{ lv_lab }</td>| ) TO lt_body.
      APPEND VALUE #( line = |<td style="padding:3px 0;">{ lv_val }</td></tr>| ) TO lt_body.
    ENDLOOP.

    APPEND VALUE #( line = |</table>| ) TO lt_body.
    APPEND VALUE #( line = |<p>In case of any questions, please contact the Compensation Department.</p>| ) TO lt_body.
    APPEND VALUE #( line = |<p>Regards,<br><b>Compensation Department</b></p>| ) TO lt_body.
    APPEND VALUE #( line = |</body></html>| ) TO lt_body.

    " --- SO10 texts (language of the run); built-in English text above = fallback ---
    DATA(lt_symbols) = VALUE tt_symbol(
      ( name = `COMPNO`       value = |{ mv_compno }| )
      ( name = `BUKRS`        value = |{ p_bukrs }| )
      ( name = `BUTXT`        value = |{ lv_butxt }| )
      ( name = `DATE`         value = |{ ls_hdr-datum DATE = USER }| )
      ( name = `AMOUNT`       value = |{ ls_hdr-amount NUMBER = USER }| )
      ( name = `CURRENCY`     value = |{ ls_hdr-waers }| )
      ( name = `ITEMS`        value = |{ lines( mt_prop_items ) }| )
      ( name = `DIRECTION`    value = COND #( WHEN mv_dir = 'V' THEN `V2C` ELSE `C2V` ) )
      ( name = `CUST_NO`      value = |{ mv_cust }| )
      ( name = `CUST_NAME`    value = |{ lv_cname }| )
      ( name = `VEND_NO`      value = |{ mv_vend }| )
      ( name = `VEND_NAME`    value = |{ lv_vname }| )
      ( name = `PARTNER_NO`   value = COND #( WHEN mv_dir = 'V' THEN |{ mv_vend }| ELSE |{ mv_cust }| ) )
      ( name = `PARTNER_NAME` value = COND #( WHEN mv_dir = 'V' THEN lv_vname ELSE lv_cname ) )
      ( name = `CLR_DOCS`     value = lv_docs ) ).

    get_mail_text( EXPORTING iv_posted  = iv_posted
                             it_symbols = lt_symbols
                   IMPORTING ev_subject = DATA(lv_so10_subj)
                             et_body    = DATA(lt_so10_body)
                             ev_found   = DATA(lv_so10_found) ).
    IF lv_so10_found = abap_true.
      lv_subject = lv_so10_subj.
      lt_body    = lt_so10_body.
    ENDIF.

    TRY.
        DATA(lo_bcs) = cl_bcs=>create_persistent( ).

        DATA(lo_doc) = cl_document_bcs=>create_document(
                         i_type    = 'HTM'
                         i_text    = lt_body
                         i_subject = lv_subject ).

        lo_doc->add_attachment(
          i_attachment_type    = 'PDF'
          i_attachment_subject = lv_attname
          i_attachment_size    = CONV so_obj_len( xstrlen( lv_pdf ) )
          i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_pdf ) ).

        lo_bcs->set_document( lo_doc ).
        lo_bcs->add_recipient( cl_cam_address_bcs=>create_internet_address( i_address_string = lv_to ) ).
        lo_bcs->set_send_immediately( abap_true ).

        DATA(lv_sent) = lo_bcs->send( ).

        IF lv_sent = abap_true.
          DATA(lv_mail_stat) = COND char1( WHEN iv_posted = abap_true THEN 'F' ELSE 'X' ).
          UPDATE yyfi_comp_log
            SET mail_stat = @lv_mail_stat,
                mail_date = @sy-datum,
                mail_user = @sy-uname
            WHERE bukrs = @p_bukrs AND compno = @mv_compno.
          COMMIT WORK.
          MESSAGE |Mail sent to { lv_to } (comp. no. { mv_compno })| TYPE 'S'.

          " Proposal sent: rows turn yellow
          IF iv_posted = abap_false.
            mv_mail_sent = abap_true.
            set_overview_colors( ).
            IF mo_alv_items IS BOUND.
              mo_alv_items->refresh( refresh_mode = if_salv_c_refresh=>soft ).
              mo_alv_hdr->refresh( refresh_mode = if_salv_c_refresh=>soft ).
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE 'Mail could not be sent' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      CATCH cx_bcs INTO DATA(lx_bcs).
        MESSAGE lx_bcs TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.



*&---------------------------------------------------------------*
  METHOD build_alv_summary.

    CREATE OBJECT mo_cc_sum
      EXPORTING
        container_name = 'CC_SUMMARY'.

    TRY.
        cl_salv_table=>factory(
          EXPORTING r_container  = mo_cc_sum
          IMPORTING r_salv_table = mo_alv_sum
          CHANGING  t_table      = mt_out ).

        DATA(lo_cols) = mo_alv_sum->get_columns( ).
        lo_cols->set_color_column( 'COLOR' ).
        lo_cols->set_optimize( abap_true ).
        lo_cols->get_column( 'TEXT'   )->set_short_text( ' ' ).
        lo_cols->get_column( 'AMOUNT' )->set_medium_text( 'Loc.curr.amount' ).
        lo_cols->get_column( 'WAERS'  )->set_short_text( 'Crcy' ).

        mo_alv_sum->get_functions( )->set_all( abap_false ).

        mo_alv_sum->display( ).

      CATCH cx_salv_error INTO DATA(lx_err).
        MESSAGE lx_err TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD do_refresh.

    fill_summary( ).

    mo_alv_sum->refresh( refresh_mode = if_salv_c_refresh=>soft ).
    mo_alv_cust->refresh( refresh_mode = if_salv_c_refresh=>soft ).
    mo_alv_vend->refresh( refresh_mode = if_salv_c_refresh=>soft ).

  ENDMETHOD.


*&---------------------------------------------------------------*
*& Create Proposal: szimuláció + PDF az app szerverre + mentés a Z táblába
*&   A Smartform nyomtatás (6. és 7. lépés) egyelőre ki van kommentezve.
*&---------------------------------------------------------------*
  METHOD create_proposal.

    DATA: lt_fields TYPE STANDARD TABLE OF sval,
          lv_rc     TYPE c LENGTH 1,
          lv_compno TYPE char10,
          lv_ok     TYPE abap_bool,
          lt_bsid_p TYPE tt_bsid_p,
          lt_bsik_p TYPE tt_bsik_p,
         lv_fm     TYPE rs38l_fnam,
         ls_ctrl   TYPE ssfctrlop,
         ls_out    TYPE ssfcompop.

    " 1. Popup (Print compensation)
    lt_fields = VALUE #(
*      ( tabname = 'T001' fieldname = 'BUKRS' value = p_bukrs )
      ( tabname = 'T002' fieldname = 'SPRAS' value = sy-langu ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = 'Print compensation'
      IMPORTING
        returncode  = lv_rc
      TABLES
        fields      = lt_fields.

    IF lv_rc IS NOT INITIAL.
      RETURN.   " Mégse
    ENDIF.

    DATA lv_langu  TYPE SYST_LANGU.
    lv_langu = lt_fields[ 1 ]-value.
    mv_langu = COND #( WHEN lv_langu IS INITIAL THEN sy-langu ELSE lv_langu ).

    " 2. Csak párosított (zöld) tételekből lehet javaslat
    IF mv_matched = abap_false.
      MESSAGE 'Előbb jelöld ki és hasonlítsd össze a vevői és szállítói tételeket' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " 3. Szimuláció (nem könyvel)
    IF run_clearing( iv_test = abap_true ) = abap_false.
      RETURN.
    ENDIF.

    " 4. Sima BSID/BSIK szerkezet, csak a zöld sorok
    LOOP AT mt_bsid INTO DATA(ls_d) WHERE cmp_stat = 'G'.
      APPEND CORRESPONDING #( ls_d ) TO lt_bsid_p.
    ENDLOOP.
    LOOP AT mt_bsik INTO DATA(ls_k) WHERE cmp_stat = 'G'.
      APPEND CORRESPONDING #( ls_k ) TO lt_bsik_p.
    ENDLOOP.

    " 5. Javaslatszám (később számkörből)
    lv_compno = new_compno( ).
    IF lv_compno IS INITIAL.
      MESSAGE 'Could not generate a compensation number' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " 5/a. A két tábla PDF-ben az app szerverre (teszt)
    write_pdf( EXPORTING iv_compno = lv_compno
                         it_bsid   = lt_bsid_p
                         it_bsik   = lt_bsik_p
               IMPORTING ev_ok     = lv_ok ).
    IF lv_ok = abap_false.
      RETURN.
    ENDIF.

    " 5/b. Mentés a Z táblába (kulcs: p_bukrs + lv_compno)
    IF save_run( lv_compno ) = abap_false.
      RETURN.
    ENDIF.



*   " 6. Smartform függvénymodul neve
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = 'YYFI_DOUBLE_COMP'
      IMPORTING
        fm_name  = lv_fm
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE 'A YY_COMP_TESZT Smartform nem található / nincs aktiválva' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
*
*   " 7. Nyomtatás
    ls_ctrl-no_dialog = abap_false.
    ls_ctrl-preview   = abap_false.
    ls_ctrl-langu     = VALUE #( lt_fields[ fieldname = 'SPRAS' ]-value DEFAULT sy-langu ).
    IF ls_ctrl-langu IS INITIAL.
      ls_ctrl-langu = sy-langu.
    ENDIF.
    ls_out-tddest     = 'LOCL'.
    ls_out-tdnewid    = abap_true.

    CALL FUNCTION lv_fm
      EXPORTING
        control_parameters = ls_ctrl
        output_options     = ls_out
        user_settings      = abap_true
        iv_bukrs           = p_bukrs
        iv_compno          = lv_compno
        iv_date            = p_date
      TABLES
        it_bsid            = lt_bsid_p
        it_bsik            = lt_bsik_p
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0 AND sy-subrc <> 4.
      MESSAGE 'Hiba a nyomtatványgenerálásnál' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
     MESSAGE |Proposal { lv_compno } saved; PDF: { pdf_path( lv_compno ) }| TYPE 'S'.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Overview: P_COMPNO kitöltve = mentett futás betöltése a Z táblából,
*&           üres = az aktuális párosítás (zöld sorok)
*&---------------------------------------------------------------*
  METHOD show_overview.

    CLEAR: mv_compno, mv_dir, mv_cust, mv_vend, mv_from_log,
           mv_mail_sent, mv_posted.

    " 500-as popup: itt adható meg a P_COMPNO
    CALL SELECTION-SCREEN 500.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF p_compno IS NOT INITIAL.
      IF load_run( p_compno ) = abap_false.
        RETURN.
      ENDIF.
    ELSE.
      DATA(lt_d) = mt_bsid.
      DATA(lt_k) = mt_bsik.

      IF mv_matched = abap_true.
        DELETE lt_d WHERE cmp_stat <> 'G'.
        DELETE lt_k WHERE cmp_stat <> 'G'.
      ENDIF.

      build_proposal_data( it_bsid   = lt_d
                           it_bsik   = lt_k
                           iv_compno = '' ).
    ENDIF.

    CALL SCREEN 9100.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD build_proposal_data.

    DATA: ls_item  TYPE ty_prop_item,
          lv_cnt   TYPE i,
          lv_sum_c TYPE p LENGTH 13 DECIMALS 2,
          lv_sum_v TYPE p LENGTH 13 DECIMALS 2,
          lv_waers TYPE waers.

    CLEAR: mt_prop_hdr, mt_prop_items.

    " Helyi pénznem a cégkódból
    SELECT SINGLE waers FROM t001
      WHERE bukrs = @p_bukrs
      INTO @lv_waers.

    " Előjel: H = pozitív, S = negatív
    LOOP AT it_bsid INTO DATA(ls_d).
      lv_cnt = lv_cnt + 1.
      CLEAR ls_item.
      ls_item-bukrs  = ls_d-bukrs.
      ls_item-compno = iv_compno.
      ls_item-cnt    = lv_cnt.
      ls_item-kunnr  = ls_d-kunnr.
      ls_item-zuonr  = ls_d-zuonr.
      ls_item-amount = COND #( WHEN ls_d-shkzg = 'H' THEN ls_d-dmbtr ELSE ls_d-dmbtr * -1 ).
      ls_item-gjahr  = ls_d-gjahr.
      ls_item-belnr  = ls_d-belnr.
      ls_item-buzei  = ls_d-buzei.
      ls_item-augdt  = ls_d-augdt.
      ls_item-augbl  = ls_d-augbl.
      ls_item-zfbdt  = ls_d-zfbdt.
      ls_item-shkzg  = ls_d-shkzg.
      ls_item-hkont  = ls_d-hkont.
      ls_item-xref3  = ls_d-xref3.
      APPEND ls_item TO mt_prop_items.
      lv_sum_c = lv_sum_c + ls_item-amount.
    ENDLOOP.

    LOOP AT it_bsik INTO DATA(ls_k).
      lv_cnt = lv_cnt + 1.
      CLEAR ls_item.
      ls_item-bukrs  = ls_k-bukrs.
      ls_item-compno = iv_compno.
      ls_item-cnt    = lv_cnt.
      ls_item-lifnr  = ls_k-lifnr.
      ls_item-zuonr  = ls_k-zuonr.
      ls_item-amount = COND #( WHEN ls_k-shkzg = 'H' THEN ls_k-dmbtr ELSE ls_k-dmbtr * -1 ).
      ls_item-gjahr  = ls_k-gjahr.
      ls_item-belnr  = ls_k-belnr.
      ls_item-buzei  = ls_k-buzei.
      ls_item-augdt  = ls_k-augdt.
      ls_item-augbl  = ls_k-augbl.
      ls_item-zfbdt  = ls_k-zfbdt.
      ls_item-shkzg  = ls_k-shkzg.
      ls_item-hkont  = ls_k-hkont.
      ls_item-xref3  = ls_k-xref3.
      APPEND ls_item TO mt_prop_items.
      lv_sum_v = lv_sum_v + ls_item-amount.
    ENDLOOP.

    APPEND VALUE #( bukrs  = p_bukrs
                    compno = iv_compno
                    lifnr  = p_lifnr
                    kunnr  = p_kunnr
                    datum  = p_date
                    amount = nmin( val1 = abs( lv_sum_c ) val2 = abs( lv_sum_v ) )
                    waers  = lv_waers
                    text   = p_text ) TO mt_prop_hdr.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD pbo_9100.

    SET PF-STATUS 'STATUS_9100'.
    SET TITLEBAR 'TITLE_9100'.

    IF mo_cc_prop IS NOT BOUND.
      build_alv_prop( ).
    ENDIF.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD build_alv_prop.

    CREATE OBJECT mo_cc_prop
      EXPORTING
        container_name = 'CC_PROP'.

    CREATE OBJECT mo_split_prop
      EXPORTING
        parent  = mo_cc_prop
        rows    = 2
        columns = 1.

    mo_split_prop->set_row_height( id = 1 height = 25 ).

    DATA(lo_top) = mo_split_prop->get_container( row = 1 column = 1 ).
    DATA(lo_bot) = mo_split_prop->get_container( row = 2 column = 1 ).

    TRY.
        " --- Felső ALV: javaslat fejsor ---
        cl_salv_table=>factory(
          EXPORTING r_container  = lo_top
          IMPORTING r_salv_table = mo_alv_hdr
          CHANGING  t_table      = mt_prop_hdr ).

        DATA(lo_cols_h) = mo_alv_hdr->get_columns( ).
        lo_cols_h->set_optimize( abap_true ).
        lo_cols_h->set_color_column( 'COLOR' ).
        lo_cols_h->get_column( 'BUKRS'  )->set_short_text( 'CoCd' ).
        lo_cols_h->get_column( 'COMPNO' )->set_short_text( 'CompNo' ).
        lo_cols_h->get_column( 'LIFNR'  )->set_short_text( 'Supplier' ).
        lo_cols_h->get_column( 'KUNNR'  )->set_short_text( 'Customer' ).
        lo_cols_h->get_column( 'DATUM'  )->set_short_text( 'Comp Date' ).
        lo_cols_h->get_column( 'AMOUNT' )->set_medium_text( 'CompAmount' ).
        lo_cols_h->get_column( 'WAERS'  )->set_short_text( 'Crcy' ).
        lo_cols_h->get_column( 'TEXT'   )->set_medium_text( 'Compensation text' ).
        mo_alv_hdr->get_functions( )->set_default( abap_true ).
        mo_alv_hdr->display( ).

        " --- Alsó ALV: tételek ---
        cl_salv_table=>factory(
          EXPORTING r_container  = lo_bot
          IMPORTING r_salv_table = mo_alv_items
          CHANGING  t_table      = mt_prop_items ).

        DATA(lo_cols_i) = mo_alv_items->get_columns( ).
        lo_cols_i->set_optimize( abap_true ).
        lo_cols_i->set_color_column( 'COLOR' ).     " row color (yellow = mail sent, green = posted)
        lo_cols_i->get_column( 'BUKRS'  )->set_short_text( 'CoCode' ).
        lo_cols_i->get_column( 'COMPNO' )->set_visible( abap_false ).
        lo_cols_i->get_column( 'CNT'    )->set_short_text( '1/No' ).
        lo_cols_i->get_column( 'LIFNR'  )->set_short_text( 'Supplier' ).
        lo_cols_i->get_column( 'KUNNR'  )->set_short_text( 'Customer' ).
        lo_cols_i->get_column( 'ZUONR'  )->set_short_text( 'Assignm.' ).
        lo_cols_i->get_column( 'AMOUNT' )->set_medium_text( 'Loc.curr.amount' ).
        lo_cols_i->get_column( 'GJAHR'  )->set_short_text( 'Year' ).
        lo_cols_i->get_column( 'BELNR'  )->set_medium_text( 'DocumentNo' ).
        lo_cols_i->get_column( 'BUZEI'  )->set_short_text( 'Itm' ).
        lo_cols_i->get_column( 'AUGDT'  )->set_medium_text( 'Clearing Date' ).
        lo_cols_i->get_column( 'AUGBL'  )->set_medium_text( 'Clrng doc.' ).
        lo_cols_i->get_column( 'ZFBDT'  )->set_medium_text( 'Baseline Date' ).
        lo_cols_i->get_column( 'SHKZG'  )->set_short_text( 'D/C' ).
        lo_cols_i->get_column( 'HKONT'  )->set_medium_text( 'G/L Account' ).
        lo_cols_i->get_column( 'XREF3'  )->set_medium_text( 'Reference Key 3' ).
        mo_alv_items->get_functions( )->set_default( abap_true ).
        mo_alv_items->get_display_settings( )->set_striped_pattern( abap_true ).
        mo_alv_items->display( ).

      CATCH cx_salv_error INTO DATA(lx_err).
        MESSAGE lx_err TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD free_prop.

    IF mo_cc_prop IS BOUND.
      mo_cc_prop->free( ).
    ENDIF.
    FREE: mo_cc_prop, mo_split_prop, mo_alv_hdr, mo_alv_items.

  ENDMETHOD.

*&---------------------------------------------------------------*
  METHOD pai_9100.

    save_ok = ok_code.
    CLEAR ok_code.

    CASE save_ok.
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
        free_prop( ).
        LEAVE TO SCREEN 0.

      WHEN 'MAIL_SEND'.
        send_mail( ).                 " proposal mail (asks before resending)

      WHEN 'POST'.
        post_from_overview( ).        " confirmation, posting, popup, completion mail

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------*
*& Globális objektumreferencia
*&---------------------------------------------------------------*
DATA: go_app TYPE REF TO lcl_open_items.

*&---------------------------------------------------------------*
*& Authorization check of the company code on the selection screen
*&---------------------------------------------------------------*
AT SELECTION-SCREEN ON p_bukrs.
  DATA gv_auth_msg TYPE string.
  gv_auth_msg = lcl_auth=>check_posting( p_bukrs ).
  IF gv_auth_msg IS NOT INITIAL.
    MESSAGE gv_auth_msg TYPE 'E'.
  ENDIF.

*&---------------------------------------------------------------*
*& Vezérlés
*&---------------------------------------------------------------*
START-OF-SELECTION.

  CREATE OBJECT go_app.

  go_app->select_data(
    iv_bukrs = p_bukrs
    iv_c2v   = p_c2v
    iv_v2c   = p_v2c
    iv_kunnr = p_kunnr
    iv_lifnr = p_lifnr
    iv_date  = p_date
    iv_text  = p_text ).

  go_app->show_screen( ).

*&---------------------------------------------------------------*
*& Dynpro modulok
*&---------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  go_app->pbo( ).
ENDMODULE.

MODULE user_command_9000 INPUT.
  go_app->pai( ).
ENDMODULE.

MODULE status_9100 OUTPUT.
  go_app->pbo_9100( ).
ENDMODULE.

MODULE user_command_9100 INPUT.
  go_app->pai_9100( ).
ENDMODULE.
