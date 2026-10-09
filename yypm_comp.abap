
REPORT z_demo_bsik_salv.

DATA: lt_bsik TYPE STANDARD TABLE OF bsik,
      lo_alv  TYPE REF TO cl_salv_table.

DATA: lt_rows TYPE salv_t_row,
      lv_row  TYPE salv_de_row.

DATA: ls_bsik TYPE bsik,
      lt_selected TYPE STANDARD TABLE OF bsik.

"1. Nyitott szállítói tételek olvasása
SELECT bukrs,
       lifnr,
       umsks,
       umskz,
       augdt,
       augbl,
       zuonr,
       gjahr,
       belnr,
       buzei,
       budat,
       bldat,
       waers,
       wrbtr,
       shkzg,
       sgtxt
  FROM bsik
  INTO CORRESPONDING FIELDS OF TABLE @lt_bsik
  UP TO 100 ROWS.

"2. Ellenőrzés
IF lt_bsik IS INITIAL.
  MESSAGE 'Nincs megjeleníthető adat a BSIK táblában'
    TYPE 'I'.
  RETURN.
ENDIF.

"3. SALV létrehozása
cl_salv_table=>factory(
  IMPORTING
    r_salv_table = lo_alv
  CHANGING
    t_table      = lt_bsik
).

"4. Standard funkciók bekapcsolása
lo_alv->get_functions( )->set_all(
  abap_true
).

"5. Sor- és cellakijelölés engedélyezése
lo_alv->get_selections( )->set_selection_mode(
  if_salv_c_selection_mode=>row_column
).

"6. ALV megjelenítése
lo_alv->display( ).

"7. Kijelölt sorok indexeinek kiolvasása
lt_rows = lo_alv->get_selections( )->get_selected_rows( ).

"8. Kijelölt rekordok összegyűjtése
LOOP AT lt_rows INTO lv_row.

  READ TABLE lt_bsik
    INDEX lv_row
    INTO ls_bsik.

  IF sy-subrc = 0.
    APPEND ls_bsik TO lt_selected.
  ENDIF.

ENDLOOP.

"9. Eredmény megjelenítése
IF lt_selected IS INITIAL.

  MESSAGE 'Nem lett sor kijelölve' TYPE 'I'.

ELSE.

  cl_demo_output=>display(
    lt_selected
  ).

ENDIF.
