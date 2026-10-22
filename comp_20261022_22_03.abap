*&---------------------------------------------------------------------*
*& Report YY_TESZT_O  -  Vevő-szállító kompenzáció (C2V / V2C)
*&---------------------------------------------------------------------*
REPORT yy_teszt_o.

*&---------------------------------------------------------------------*
*& Konstansok (ide kell beírni a T008 szerinti zárolási kulcsokat)
*&---------------------------------------------------------------------*
CONSTANTS: gc_blart   TYPE blart  VALUE 'AB',
           gc_zlspr_d TYPE dzlspr VALUE 'K',          " új fizetési zárolás a VEVŐTÉTELEKEN
           gc_zlspr_k TYPE dzlspr VALUE IS INITIAL.   " szállítói tételeken: üres = nem nyúlunk hozzá

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

TYPES: BEGIN OF ty_amt,
         idx   TYPE i,
         cents TYPE int8,
       END OF ty_amt,
       tt_amt TYPE STANDARD TABLE OF ty_amt WITH DEFAULT KEY.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

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
           END OF ty_result,
           " zárolás-módosítás nyilvántartása (visszaállításhoz)
           BEGIN OF ty_chg,
             kind  TYPE c LENGTH 1,
             belnr TYPE belnr_d,
             gjahr TYPE gjahr,
             buzei TYPE buzei,
             old   TYPE dzlspr,
           END OF ty_chg,
           tt_chg TYPE STANDARD TABLE OF ty_chg WITH EMPTY KEY.

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
      IMPORTING iv_kind    TYPE char1          " D = vevő, K = szállító
                iv_bukrs   TYPE bukrs
                iv_belnr   TYPE belnr_d
                iv_gjahr   TYPE gjahr
                iv_buzei   TYPE buzei
                iv_new     TYPE dzlspr
      EXPORTING ev_msg     TYPE string         " üres = rendben
                ev_old     TYPE dzlspr
                ev_changed TYPE abap_bool.

    CLASS-METHODS restore_blocks
      IMPORTING iv_bukrs TYPE bukrs
                it_chg   TYPE tt_chg.
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

    DATA: mv_runid   TYPE sysuuid_x16,
          mv_matched TYPE abap_bool.     " volt-e összehasonlítás

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
      save_run,
      apply_colors,
      delete_yellow,
      save_log,
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
          lv_old    TYPE dzlspr,
          lv_subrc  TYPE sy-subrc.

    CLEAR: ev_msg, ev_old, ev_changed.

    " Aktuális érték a nyitott tételből, a tétel fajtája szerint
    CASE iv_kind.
      WHEN 'D'.
        SELECT SINGLE zlspr FROM bsid
          WHERE bukrs = @iv_bukrs AND belnr = @iv_belnr
            AND gjahr = @iv_gjahr AND buzei = @iv_buzei
          INTO @lv_old.
        lv_subrc = sy-subrc.
      WHEN 'K'.
        SELECT SINGLE zlspr FROM bsik
          WHERE bukrs = @iv_bukrs AND belnr = @iv_belnr
            AND gjahr = @iv_gjahr AND buzei = @iv_buzei
          INTO @lv_old.
        lv_subrc = sy-subrc.
      WHEN OTHERS.
        ev_msg = |Ismeretlen tételfajta: { iv_kind }|.
        RETURN.
    ENDCASE.

    IF lv_subrc <> 0.
      ev_msg = |{ iv_belnr }/{ iv_buzei }: nem nyitott tétel ({ iv_kind })|.
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
        ev_msg = |{ iv_belnr }: fizetési zárolás módosítása sikertelen (subrc { lv_rc })|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD restore_blocks.
    LOOP AT it_chg INTO DATA(ls_chg).
      set_payment_block( EXPORTING iv_kind  = ls_chg-kind
                                   iv_bukrs = iv_bukrs
                                   iv_belnr = ls_chg-belnr
                                   iv_gjahr = ls_chg-gjahr
                                   iv_buzei = ls_chg-buzei
                                   iv_new   = ls_chg-old
                         IMPORTING ev_msg   = DATA(lv_dummy) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD clear_items.
    DATA: lt_ftclear TYPE STANDARD TABLE OF ftclear,
          lt_ftpost  TYPE STANDARD TABLE OF ftpost,
          lt_fttax   TYPE STANDARD TABLE OF fttax,
          lt_blntab  TYPE STANDARD TABLE OF blntab,
          lt_chg     TYPE tt_chg,
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
    " 1/b) Fizetési zárolás átírása KIPONTOZÁS ELŐTT (nyitott tételeken)
    "------------------------------------------------------------------
    LOOP AT it_items INTO DATA(ls_blk).
      DATA(lv_new) = COND dzlspr( WHEN ls_blk-kind = 'D' THEN iv_zlspr_d
                                  ELSE iv_zlspr_k ).
      CHECK lv_new IS NOT INITIAL.        " nincs megadva -> nem nyúlunk hozzá

      set_payment_block( EXPORTING iv_kind    = ls_blk-kind
                                   iv_bukrs   = iv_bukrs
                                   iv_belnr   = ls_blk-belnr
                                   iv_gjahr   = ls_blk-gjahr
                                   iv_buzei   = ls_blk-buzei
                                   iv_new     = lv_new
                         IMPORTING ev_msg     = DATA(lv_blk_err)
                                   ev_old     = DATA(lv_blk_old)
                                   ev_changed = DATA(lv_blk_chg) ).

      IF lv_blk_chg = abap_true.
        APPEND VALUE #( kind  = ls_blk-kind
                        belnr = ls_blk-belnr
                        gjahr = ls_blk-gjahr
                        buzei = ls_blk-buzei
                        old   = lv_blk_old ) TO lt_chg.
      ENDIF.

      IF lv_blk_err IS NOT INITIAL.
        restore_blocks( iv_bukrs = iv_bukrs it_chg = lt_chg ).   " eddigi módosítások vissza
        rs_result-msg = lv_blk_err.
        RETURN.
      ENDIF.
    ENDLOOP.

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
    " 4) Könyvelés
    "------------------------------------------------------------------
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_function = 'C'
        i_mode     = iv_mode
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      restore_blocks( iv_bukrs = iv_bukrs it_chg = lt_chg ).
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
    ELSE.
      " sikertelen kipontozás -> a zárolásokat visszaállítjuk
      restore_blocks( iv_bukrs = iv_bukrs it_chg = lt_chg ).
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
    SORT mt_amt BY cents DESCENDING.

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

    mo_cont_cust = mo_splitter->get_container( row = 1 column = 1 ).
    mo_cont_vend = mo_splitter->get_container( row = 1 column = 2 ).

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
      DATA(lv_cc) = CONV int8( abs( <d>-wrbtr ) * 100 ).
      APPEND VALUE #( idx = lv_c cents = lv_cc ) TO lt_amt_c.
      lv_sum_c = lv_sum_c + lv_cc.
    ENDLOOP.

    LOOP AT mt_sel_v INTO DATA(lv_v).
      ASSIGN mt_bsik[ lv_v ] TO FIELD-SYMBOL(<k>).
      CHECK sy-subrc = 0.
      INSERT <k>-waers INTO TABLE lt_cur.
      DATA(lv_vc) = CONV int8( abs( <k>-wrbtr ) * 100 ).
      APPEND VALUE #( idx = lv_v cents = lv_vc ) TO lt_amt_v.
      lv_sum_v = lv_sum_v + lv_vc.
    ENDLOOP.

    " Egy párosításban csak egy pénznem lehet
    IF lines( lt_cur ) > 1.
      MESSAGE 'The selected items have different currencies; select items of one currency only' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

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

    TRY.
        mv_runid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        MESSAGE 'Futásazonosító generálása sikertelen' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    SELECT SINGLE waers FROM t001
      WHERE bukrs = @p_bukrs
      INTO @DATA(lv_hwaer).

    DATA(ls_head) = VALUE yyfi_comp_log(
      runid     = mv_runid
      direction = COND #( WHEN mv_c2v = abap_true THEN 'C' ELSE 'V' )
      comp_date = p_date
      comp_text = p_text
      ernam     = sy-uname
      erdat     = sy-datum
      erzet     = sy-uzeit
      hwaer     = lv_hwaer ).

    LOOP AT mt_bsid ASSIGNING FIELD-SYMBOL(<d>).
      lv_cnt = lv_cnt + 1.
      APPEND VALUE #( BASE ls_head
        side = 'C' lineno_comp = lv_cnt
        bukrs = <d>-bukrs kunnr = <d>-kunnr
        gjahr = <d>-gjahr belnr = <d>-belnr buzei = <d>-buzei
        zuonr = <d>-zuonr shkzg = <d>-shkzg budat = <d>-budat
        wrbtr = <d>-wrbtr waers = <d>-waers dmbtr = <d>-dmbtr
        cmp_stat = <d>-cmp_stat ) TO lt_log.
    ENDLOOP.

    CLEAR lv_cnt.
    LOOP AT mt_bsik ASSIGNING FIELD-SYMBOL(<k>).
      lv_cnt = lv_cnt + 1.
      APPEND VALUE #( BASE ls_head
        side = 'V' lineno_comp = lv_cnt
        bukrs = <k>-bukrs lifnr = <k>-lifnr
        gjahr = <k>-gjahr belnr = <k>-belnr buzei = <k>-buzei
        zuonr = <k>-zuonr shkzg = <k>-shkzg budat = <k>-budat
        wrbtr = <k>-wrbtr waers = <k>-waers dmbtr = <k>-dmbtr
        cmp_stat = <k>-cmp_stat ) TO lt_log.
    ENDLOOP.

    IF lt_log IS NOT INITIAL.
      INSERT yyfi_comp_log FROM TABLE @lt_log.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
        MESSAGE 'Mentés a Z táblába sikertelen' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD save_run.

    IF mv_matched = abap_false.
      MESSAGE 'Előbb jelöld ki és hasonlítsd össze a vevői és szállítói tételeket' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    IF line_exists( mt_bsid[ cmp_stat = 'Y' ] ) OR
       line_exists( mt_bsik[ cmp_stat = 'Y' ] ).
      MESSAGE 'Maradt sárga sor, töröld őket mentés előtt' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF NOT line_exists( mt_bsid[ cmp_stat = 'G' ] ) OR
       NOT line_exists( mt_bsik[ cmp_stat = 'G' ] ).
      MESSAGE 'Nincs egyező vevői és szállítói tétel a mentéshez' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lv_sum_c) = REDUCE wrbtr( INIT s = CONV wrbtr( 0 )
                                   FOR wa IN mt_bsid WHERE ( cmp_stat = 'G' )
                                   NEXT s = s + abs( wa-wrbtr ) ).
    DATA(lv_sum_v) = REDUCE wrbtr( INIT s = CONV wrbtr( 0 )
                                   FOR wb IN mt_bsik WHERE ( cmp_stat = 'G' )
                                   NEXT s = s + abs( wb-wrbtr ) ).

    IF lv_sum_c <> lv_sum_v.
      MESSAGE 'A vevői és szállítói összegek nem egyeznek' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    save_log( ).

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
*& Kiegyenlítés a zöld (G) sorokkal, pénznemenként
*&   iv_test = X : szimuláció, nem könyvel
*&   Irányfüggetlen: a C2V/V2C csak a párosításnál számít,
*&   a kiegyenlítés az előjelek alapján dolgozik.
*&---------------------------------------------------------------*
  METHOD run_clearing.

    DATA lt_cur TYPE SORTED TABLE OF waers WITH UNIQUE KEY table_line.

    rv_ok = abap_false.

    " Kompenzáció dátuma: 500-as ablak mezője, ha üres, a főképernyő dátuma
    DATA(lv_budat) = COND budat( WHEN p_datum IS NOT INITIAL THEN p_datum ELSE p_date ).

    " A zöld sorok átadása, a megjelenített táblák érintetlenek
    DATA(lt_all) = VALUE lcl_kd_comp=>tt_item(
      FOR ls_d IN mt_bsid WHERE ( cmp_stat = 'G' )
      ( kind = 'D' belnr = ls_d-belnr gjahr = ls_d-gjahr buzei = ls_d-buzei
        shkzg = ls_d-shkzg wrbtr = ls_d-wrbtr waers = ls_d-waers ) ).

    lt_all = VALUE #( BASE lt_all
      FOR ls_k IN mt_bsik WHERE ( cmp_stat = 'G' )
      ( kind = 'K' belnr = ls_k-belnr gjahr = ls_k-gjahr buzei = ls_k-buzei
        shkzg = ls_k-shkzg wrbtr = ls_k-wrbtr waers = ls_k-waers ) ).

    IF NOT line_exists( lt_all[ kind = 'D' ] ) OR NOT line_exists( lt_all[ kind = 'K' ] ).
      MESSAGE 'There are no matched (green) customer and vendor items' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_all INTO DATA(ls_all).
      INSERT ls_all-waers INTO TABLE lt_cur.
    ENDLOOP.

    rv_ok = abap_true.

    LOOP AT lt_cur INTO DATA(lv_cur).

      DATA(lt_cur_items) = VALUE lcl_kd_comp=>tt_item(
        FOR ls_x IN lt_all WHERE ( waers = lv_cur ) ( ls_x ) ).

      DATA(ls_res) = lcl_kd_comp=>clear_items(
                       iv_bukrs   = p_bukrs
                       iv_kunnr   = p_kunnr
                       iv_lifnr   = p_lifnr
                       iv_budat   = lv_budat
                       iv_blart   = gc_blart
                       iv_mode    = 'N'
                       iv_test    = iv_test
                       iv_zlspr_d = gc_zlspr_d
                       iv_zlspr_k = gc_zlspr_k
                       it_items   = lt_cur_items ).

      IF ls_res-success = abap_false.
        rv_ok = abap_false.
        MESSAGE |{ lv_cur }: { ls_res-msg }| TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.      " a következő pénznemre nem lépünk tovább
      ENDIF.

      IF iv_test = abap_true.
        MESSAGE |{ ls_res-msg }| TYPE 'S'.
      ELSE.
        MESSAGE |{ lv_cur }: clearing document { ls_res-belnr } / { ls_res-gjahr }| TYPE 'S'.
        " a sikeresen kiegyenlített pénznem sorai kikerülnek a listából
        DELETE mt_bsid WHERE cmp_stat = 'G' AND waers = lv_cur.
        DELETE mt_bsik WHERE cmp_stat = 'G' AND waers = lv_cur.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Create Proposal: szimuláció + Smartform nyomtatás (csak zöld sorok)
*&---------------------------------------------------------------*
  METHOD create_proposal.

    DATA: lt_fields  TYPE STANDARD TABLE OF sval,
          lv_rc      TYPE c LENGTH 1,
          lv_fm      TYPE rs38l_fnam,
          ls_ctrl    TYPE ssfctrlop,
          ls_out     TYPE ssfcompop,
          lv_compno  TYPE c LENGTH 10,
          lt_bsid_p  TYPE tt_bsid_p,
          lt_bsik_p  TYPE tt_bsik_p.

    " 1. Popup (Print compensation)
    lt_fields = VALUE #(
      ( tabname = 'T001' fieldname = 'BUKRS' value = p_bukrs )
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

    " 2. Csak párosított (zöld) tételekből lehet javaslat
    IF mv_matched = abap_false.
      MESSAGE 'Előbb jelöld ki és hasonlítsd össze a vevői és szállítói tételeket' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " 3. Szimuláció (nem könyvel)
    IF run_clearing( iv_test = abap_true ) = abap_false.
      RETURN.
    ENDIF.

    " 4. Smartformnak sima BSID/BSIK szerkezet kell, csak a zöld sorok
    LOOP AT mt_bsid INTO DATA(ls_d) WHERE cmp_stat = 'G'.
      APPEND CORRESPONDING #( ls_d ) TO lt_bsid_p.
    ENDLOOP.
    LOOP AT mt_bsik INTO DATA(ls_k) WHERE cmp_stat = 'G'.
      APPEND CORRESPONDING #( ls_k ) TO lt_bsik_p.
    ENDLOOP.

    " 5. Teszt javaslatszám (később számkörből)
    lv_compno = |{ sy-datum+4(4) }{ sy-uzeit(2) }{ sy-uzeit+2(2) }|.

    " 6. Smartform függvénymodul neve
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = 'YY_COMP_TESZT'
      IMPORTING
        fm_name  = lv_fm
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE 'A YY_COMP_TESZT Smartform nem található / nincs aktiválva' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " 7. Nyomtatás
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

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Overview: megtekintő nézet (párosítás után csak a zöld sorok)
*&---------------------------------------------------------------*
  METHOD show_overview.

    DATA(lt_d) = mt_bsid.
    DATA(lt_k) = mt_bsik.

    IF mv_matched = abap_true.
      DELETE lt_d WHERE cmp_stat <> 'G'.
      DELETE lt_k WHERE cmp_stat <> 'G'.
    ENDIF.

    build_proposal_data( it_bsid   = lt_d
                         it_bsik   = lt_k
                         iv_compno = '' ).

    CALL SELECTION-SCREEN 500.

    IF sy-subrc = 0.
      CALL SCREEN 9100.
    ENDIF.

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
        lo_cols_h->get_column( 'BUKRS'  )->set_short_text( 'CoCd' ).
        lo_cols_h->get_column( 'COMPNO' )->set_visible( abap_false ).
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

      WHEN 'POST'.
        " Éles könyvelés a zöld sorokkal, pénznemenként
        DATA(lv_posted) = run_clearing( iv_test = abap_false ).

        " A sikeresen kiegyenlített sorok már kikerültek a táblákból
        do_refresh( ).

        IF lv_posted = abap_true.
          free_prop( ).
          LEAVE TO SCREEN 0.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------*
*& Globális objektumreferencia
*&---------------------------------------------------------------*
DATA: go_app TYPE REF TO lcl_open_items.

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
