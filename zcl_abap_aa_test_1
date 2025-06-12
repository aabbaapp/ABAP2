REPORT z_fi_aa_value_example.

" Lokális típus definíciók (fentebb már látható)
TYPES: BEGIN OF ty_s_depreciation_entry,
         bukrs     TYPE bukrs,
         anln1     TYPE anln1,
         anln2     TYPE anln2,
         gjahr     TYPE gjahr,
         afabe     TYPE afabe,
         bwmod     TYPE bwmod,
         x_amount  TYPE wrbtr,
         waers     TYPE waers,
         budat     TYPE budat,
       END OF ty_s_depreciation_entry.
TYPES ty_t_depreciation_entry TYPE STANDARD TABLE OF ty_s_depreciation_entry WITH EMPTY KEY.

START-OF-SELECTION.

  " Belső tábla feltöltése az új VALUE #() operátorral
  DATA(lt_depreciation_data) = VALUE ty_t_depreciation_entry(
    ( bukrs = '1000' anln1 = '000000000001' anln2 = '0000' gjahr = '2024' afabe = '01'
      bwmod = 'Z001' x_amount = '1000.00' waers = 'HUF' budat = sy-datum )
    ( bukrs = '1000' anln1 = '000000000001' anln2 = '0000' gjahr = '2024' afabe = '10'
      bwmod = 'Z002' x_amount = '800.00'  waers = 'HUF' budat = sy-datum )
    ( bukrs = '1000' anln1 = '000000000002' anln2 = '0000' gjahr = '2024' afabe = '01'
      bwmod = 'Z001' x_amount = '500.00'  waers = 'HUF' budat = sy-datum )
    ( bukrs = '2000' anln1 = '000000000003' anln2 = '0000' gjahr = '2023' afabe = '01'
      bwmod = 'Z001' x_amount = '2000.00' waers = 'USD' budat = sy-datum - 30 )
  ).

  " Az inicializált adatok kiírása
  WRITE: / '--- Értékcsökkenési tételek ---'.
  LOOP AT lt_depreciation_data INTO DATA(ls_entry).
    WRITE: / |Cégkód: { ls_entry-bukrs }, Eszköz: { ls_entry-anln1 }/{ ls_entry-anln2 }, Év: { ls_entry-gjahr }, | &
             |Terület: { ls_entry-afabe }, Összeg: { ls_entry-x_amount } { ls_entry-waers }, Dátum: { ls_entry-budat }|.
  ENDLOOP.

  " Példa egyetlen struktúra inicializálására is
  DATA(ls_single_entry) = VALUE ty_s_depreciation_entry(
    bukrs    = '1000'
    anln1    = '000000000004'
    anln2    = '0000'
    gjahr    = '2024'
    afabe    = '01'
    bwmod    = 'Z001'
    x_amount = '300.00'
    waers    = 'EUR'
    budat    = sy-datum
  ).

  WRITE: / '--- Egyedi tétel ---'.
  WRITE: / |Cégkód: { ls_single_entry-bukrs }, Eszköz: { ls_single_entry-anln1 }/{ ls_single_entry-anln2 }, Összeg: { ls_single_entry-x_amount } { ls_single_entry-waers }|.
