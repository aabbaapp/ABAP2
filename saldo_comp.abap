*&---------------------------------------------------------------------*
*& Report YY_TESZT_CLEARING
*& Vevő-szállító kompenzáció: kiegyenlítés + különbözet könyvelése
*&---------------------------------------------------------------------*
REPORT yy_teszt_clearing.

PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY,
            p_kunnr TYPE kna1-kunnr OBLIGATORY,
            p_lifnr TYPE lfa1-lifnr OBLIGATORY,
            p_date  TYPE sy-datum OBLIGATORY DEFAULT sy-datum,
            p_blart TYPE blart OBLIGATORY DEFAULT 'AB',
            p_mode  TYPE c LENGTH 1 DEFAULT 'N',   " N = háttér, A = minden képernyő, E = csak hiba
            p_test  AS CHECKBOX DEFAULT 'X'.       " X = nem könyvel, csak szimulál

START-OF-SELECTION.

  DATA: lt_ftpost  TYPE STANDARD TABLE OF ftpost,
        lt_ftclear TYPE STANDARD TABLE OF ftclear,
        lt_fttax   TYPE STANDARD TABLE OF fttax,
        lt_blntab  TYPE STANDARD TABLE OF blntab,
        lv_datum   TYPE c LENGTH 10,
        lv_monat   TYPE c LENGTH 2,
        lv_waers   TYPE waers,
        lv_sum_c   TYPE p LENGTH 13 DECIMALS 2,
        lv_sum_v   TYPE p LENGTH 13 DECIMALS 2,
        lv_saldo   TYPE p LENGTH 13 DECIMALS 2,
        lv_resid   TYPE p LENGTH 13 DECIMALS 2,
        lv_amt_c   TYPE c LENGTH 20,
        lv_newbs   TYPE c LENGTH 2,
        lv_newko   TYPE c LENGTH 17,
        lv_msgid   TYPE sy-msgid,
        lv_msgty   TYPE sy-msgty,
        lv_msgno   TYPE sy-msgno,
        lv_msgv1   TYPE sy-msgv1,
        lv_msgv2   TYPE sy-msgv2,
        lv_msgv3   TYPE sy-msgv3,
        lv_msgv4   TYPE sy-msgv4,
        lv_subrc   TYPE sy-subrc.

*----------------------------------------------------------------------*
* 1. Nyitott tételek beolvasása
*----------------------------------------------------------------------*
  SELECT * FROM bsid
    INTO TABLE @DATA(lt_bsid)
    WHERE bukrs = @p_bukrs
      AND kunnr = @p_kunnr
      AND budat <= @p_date.

  SELECT * FROM bsik
    INTO TABLE @DATA(lt_bsik)
    WHERE bukrs = @p_bukrs
      AND lifnr = @p_lifnr
      AND budat <= @p_date.

  IF lt_bsid IS INITIAL OR lt_bsik IS INITIAL.
    MESSAGE 'Mindkét oldalon kell nyitott tétel' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* 2. Saldó helyi pénznemben (S = tartozik +, H = követel -)
*----------------------------------------------------------------------*
  LOOP AT lt_bsid INTO DATA(ls_d).
    lv_sum_c = lv_sum_c + COND #( WHEN ls_d-shkzg = 'S' THEN ls_d-dmbtr ELSE ls_d-dmbtr * -1 ).
  ENDLOOP.

  LOOP AT lt_bsik INTO DATA(ls_k).
    lv_sum_v = lv_sum_v + COND #( WHEN ls_k-shkzg = 'S' THEN ls_k-dmbtr ELSE ls_k-dmbtr * -1 ).
  ENDLOOP.

  lv_saldo = lv_sum_c + lv_sum_v.

  SELECT SINGLE waers FROM t001
    WHERE bukrs = @p_bukrs
    INTO @lv_waers.

*----------------------------------------------------------------------*
* 3. Maradványtétel meghatározása (ha a saldó nem nulla)
*    A nagyobb abszolút összegű oldalra könyvelünk, a saldóval
*    azonos előjellel:  vevő: S=01, H=11   szállító: S=21, H=31
*----------------------------------------------------------------------*
  IF abs( lv_saldo ) >= '0.005'.

    lv_resid = abs( lv_saldo ).

    IF abs( lv_sum_c ) >= abs( lv_sum_v ).
      lv_newbs = COND #( WHEN lv_saldo > 0 THEN '01' ELSE '11' ).
      lv_newko = p_kunnr.
    ELSE.
      lv_newbs = COND #( WHEN lv_saldo > 0 THEN '21' ELSE '31' ).
      lv_newko = p_lifnr.
    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
* Tesztmód: csak kiírjuk, mit csinálna
*----------------------------------------------------------------------*
  IF p_test = abap_true.
    WRITE: / 'TESZTMÓD - nem történik könyvelés'.
    WRITE: / 'Vevői saldó     :', lv_sum_c LEFT-JUSTIFIED, lv_waers.
    WRITE: / 'Szállítói saldó :', lv_sum_v LEFT-JUSTIFIED, lv_waers.
    WRITE: / 'Különbözet      :', lv_saldo LEFT-JUSTIFIED, lv_waers.
    IF lv_resid IS NOT INITIAL.
      WRITE: / 'Maradványtétel  : kulcs', lv_newbs, ', számla', lv_newko,
               ', összeg', lv_resid LEFT-JUSTIFIED, lv_waers.
    ELSE.
      WRITE: / 'Nincs különbözet, tiszta kiegyenlítés.'.
    ENDIF.
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* 4. Fejadatok (FTPOST, STYPE = 'K')
*----------------------------------------------------------------------*
  WRITE p_date TO lv_datum.
  lv_monat = p_date+4(2).

  lt_ftpost = VALUE #(
    ( stype = 'K' count = 1 fnam = 'BKPF-BUKRS' fval = p_bukrs )
    ( stype = 'K' count = 1 fnam = 'BKPF-BLDAT' fval = lv_datum )
    ( stype = 'K' count = 1 fnam = 'BKPF-BUDAT' fval = lv_datum )
    ( stype = 'K' count = 1 fnam = 'BKPF-MONAT' fval = lv_monat )
    ( stype = 'K' count = 1 fnam = 'BKPF-BLART' fval = p_blart )
    ( stype = 'K' count = 1 fnam = 'BKPF-WAERS' fval = lv_waers )
    ( stype = 'K' count = 1 fnam = 'BKPF-BKTXT' fval = 'Kompenzacio' ) ).

*----------------------------------------------------------------------*
* 5. Maradványtétel könyvelési sora (FTPOST, STYPE = 'P')
*----------------------------------------------------------------------*
  IF lv_resid IS NOT INITIAL.

    WRITE lv_resid TO lv_amt_c CURRENCY lv_waers.   " felhasználói számformátum
    CONDENSE lv_amt_c.

    APPEND VALUE #( stype = 'P' count = 1 fnam = 'RF05A-NEWBS' fval = lv_newbs ) TO lt_ftpost.
    APPEND VALUE #( stype = 'P' count = 1 fnam = 'RF05A-NEWKO' fval = lv_newko ) TO lt_ftpost.
    APPEND VALUE #( stype = 'P' count = 1 fnam = 'BSEG-WRBTR'  fval = lv_amt_c ) TO lt_ftpost.
    APPEND VALUE #( stype = 'P' count = 1 fnam = 'BSEG-SGTXT'  fval = 'Kompenzacio maradvany' ) TO lt_ftpost.

  ENDIF.

*----------------------------------------------------------------------*
* 6. Kiegyenlítendő tételek (FTCLEAR)
*----------------------------------------------------------------------*
  LOOP AT lt_bsid INTO ls_d.
    APPEND VALUE #( agkoa  = 'D'
                    agkon  = ls_d-kunnr
                    agbuk  = ls_d-bukrs
                    xnops  = 'X'
                    selfd  = 'BELNR'
                    selvon = |{ ls_d-belnr }{ ls_d-gjahr }{ ls_d-buzei }| ) TO lt_ftclear.
  ENDLOOP.

  LOOP AT lt_bsik INTO ls_k.
    APPEND VALUE #( agkoa  = 'K'
                    agkon  = ls_k-lifnr
                    agbuk  = ls_k-bukrs
                    xnops  = 'X'
                    selfd  = 'BELNR'
                    selvon = |{ ls_k-belnr }{ ls_k-gjahr }{ ls_k-buzei }| ) TO lt_ftclear.
  ENDLOOP.

*----------------------------------------------------------------------*
* 7. Könyvelés: START -> CLEARING -> END
*----------------------------------------------------------------------*
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_function = 'C'
      i_mode     = p_mode
      i_update   = 'S'
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Hiba a POSTING_INTERFACE_START hívásnál' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv   = 'UMBUCHNG'
      i_tcode   = 'FB05'
    IMPORTING
      e_msgid   = lv_msgid
      e_msgty   = lv_msgty
      e_msgno   = lv_msgno
      e_msgv1   = lv_msgv1
      e_msgv2   = lv_msgv2
      e_msgv3   = lv_msgv3
      e_msgv4   = lv_msgv4
      e_subrc   = lv_subrc
    TABLES
      t_blntab  = lt_blntab
      t_ftclear = lt_ftclear
      t_ftpost  = lt_ftpost
      t_fttax   = lt_fttax
    EXCEPTIONS
      OTHERS    = 1.

  DATA(lv_exc_subrc) = sy-subrc.

  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXCEPTIONS
      OTHERS = 1.

*----------------------------------------------------------------------*
* 8. Eredmény
*----------------------------------------------------------------------*
  IF lv_exc_subrc = 0 AND lv_subrc = 0 AND lt_blntab IS NOT INITIAL.
    READ TABLE lt_blntab INTO DATA(ls_bln) INDEX 1.
    MESSAGE |Kiegyenlítés és könyvelés kész. Bizonylat: { ls_bln-belnr } / { ls_bln-gjahr }| TYPE 'S'.
  ELSEIF lv_msgid IS NOT INITIAL.
    MESSAGE ID lv_msgid TYPE 'S' NUMBER lv_msgno
      WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4
      DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE 'A kiegyenlítés sikertelen' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
