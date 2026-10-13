*&---------------------------------------------------------------------*
*& Report YY_TESZT_O
*&---------------------------------------------------------------------*
REPORT yy_teszt_o.

****&---------------------------------------------------------------------*
****& Report YY_TESZT_O
****&---------------------------------------------------------------------*
***
***TABLES sscrfields.
***
****&---------------------------------------------------------------*
****& 1000-es szelekciós képernyő
****&---------------------------------------------------------------*
***
***
***SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
***PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
***SELECTION-SCREEN END OF BLOCK b1.
***
***SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
***PARAMETERS: p_c2v RADIOBUTTON GROUP g1 DEFAULT 'X',
***            p_v2c RADIOBUTTON GROUP g1.
***SELECTION-SCREEN END OF BLOCK b2.
***
***SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
***PARAMETERS: p_kunnr TYPE kna1-kunnr OBLIGATORY,
***            p_lifnr TYPE lfa1-lifnr OBLIGATORY.
***SELECTION-SCREEN END OF BLOCK b3.
***
***SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
***PARAMETERS: p_date TYPE sy-datum OBLIGATORY DEFAULT sy-datum.
***PARAMETERS: p_text TYPE char50.
***SELECTION-SCREEN END OF BLOCK b4.
***
***SELECTION-SCREEN BEGIN OF SCREEN 500 AS WINDOW.
***
***SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-b05.
***PARAMETERS: p_bukrs2 TYPE bukrs OBLIGATORY.
***
***SELECTION-SCREEN END OF BLOCK b5.
***SELECTION-SCREEN END OF SCREEN 500.
***
***
***
****&---------------------------------------------------------------*
****& Típusok (globális szinten, az osztály előtt)
****&---------------------------------------------------------------*
***TYPES: BEGIN OF ty_out,
***         text   TYPE char40,
***         amount TYPE p LENGTH 13 DECIMALS 2,
***         waers  TYPE waers,
***         color  TYPE lvc_t_scol,
***       END OF ty_out.
***
***DATA: ok_code TYPE sy-ucomm,
***      save_ok TYPE sy-ucomm.
***
***DATA: dynnr_code TYPE sy-dynnr,
***      dynnr_ok   TYPE sy-dynnr.
***
***" Elemi típus helyett struktúraként definiáljuk a sort:
***TYPES: BEGIN OF ty_txt_line,
***         line TYPE char255,
***       END OF ty_txt_line.
***
****&---------------------------------------------------------------*
****& Event handler osztály - a lcl_open_items ELŐTT kell, mert
****& a lcl_open_items fogja használni (NEW lcl_handler)
****&---------------------------------------------------------------*
***CLASS lcl_open_items DEFINITION DEFERRED.   " előre jelezzük, hogy lesz ilyen osztály
***
***CLASS lcl_handler DEFINITION.
***  PUBLIC SECTION.
***    DATA: mo_app TYPE REF TO lcl_open_items.
***    METHODS on_func FOR EVENT added_function OF cl_salv_events_table
***      IMPORTING e_salv_function.
***ENDCLASS.
***
****&---------------------------------------------------------------*
****& Lokális osztály: az adatlekérdezés és megjelenítés logikája
****&---------------------------------------------------------------*
***CLASS lcl_open_items DEFINITION.
***
***  PUBLIC SECTION.
***    METHODS:
***      select_data
***        IMPORTING iv_bukrs TYPE bukrs
***                  iv_c2v   TYPE abap_bool
***                  iv_v2c   TYPE abap_bool
***                  iv_kunnr TYPE kna1-kunnr
***                  iv_lifnr TYPE lfa1-lifnr
***                  iv_date  TYPE sy-datum
***                  iv_text  TYPE char50,
***      show_screen,
***      pbo,
***      pai,
***      create_proposal,
***      do_refresh.
***
***    DATA: mv_proposal_status TYPE abap_bool.
***
***  PRIVATE SECTION.
***    DATA: mt_bsid TYPE STANDARD TABLE OF bsid,
***          mt_bsik TYPE STANDARD TABLE OF bsik,
***          mt_out  TYPE STANDARD TABLE OF ty_out.
***
***    " 1. Custom Container (Screen Painteren felvett neve: 'CC_TOP')
***    DATA: mo_cc_top    TYPE REF TO cl_gui_custom_container,
***          mo_splitter  TYPE REF TO cl_gui_splitter_container,
***          mo_cont_cust TYPE REF TO cl_gui_container,
***          mo_cont_vend TYPE REF TO cl_gui_container,
***          mo_alv_cust  TYPE REF TO cl_salv_table,
***          mo_alv_vend  TYPE REF TO cl_salv_table.
***
***    " 2. Custom Container (Screen Painteren felvett neve: 'CC_SUMMARY')
***    DATA: mo_cc_sum  TYPE REF TO cl_gui_custom_container,
***          mo_alv_sum TYPE REF TO cl_salv_table.
***
***    DATA: mo_handler TYPE REF TO lcl_handler.
***
***         DATA: mv_c2v TYPE abap_bool,
***          mv_v2c TYPE abap_bool.
***
***    METHODS: build_alv_top,
***      build_alv_summary,
***      fill_summary.
***
***ENDCLASS.
***
***CLASS lcl_handler IMPLEMENTATION.
***  METHOD on_func.
***    CASE e_salv_function.
***      WHEN 'REFRESH'.
***        mo_app->do_refresh( ).
***      WHEN 'CREATE_PROP'.
***        " ide jön a javaslat létrehozása
***        mo_app->create_proposal( ).
***    ENDCASE.
***  ENDMETHOD.
***ENDCLASS.
***
***CLASS lcl_open_items IMPLEMENTATION.
***
***  METHOD select_data.
***
***        " Irány eltárolása a fill_summary számára
***    mv_c2v = iv_c2v.
***    mv_v2c = iv_v2c.
***
***    SELECT * FROM bsid
***      INTO TABLE @mt_bsid
***      WHERE bukrs = @iv_bukrs
***        AND kunnr = @iv_kunnr
***        AND budat <= @iv_date.
***
****    IF iv_text IS NOT INITIAL.
****      DELETE mt_bsid WHERE sgtxt NP iv_text.
****    ENDIF.
***
***    SELECT * FROM bsik
***      INTO TABLE @mt_bsik
***      WHERE bukrs = @iv_bukrs
***        AND lifnr = @iv_lifnr
***        AND budat <= @iv_date.
***
****    IF iv_text IS NOT INITIAL.
****      DELETE mt_bsik WHERE sgtxt NP iv_text.
****    ENDIF.
***
***
***
***    fill_summary( ).
***
***  ENDMETHOD.
***
***  METHOD fill_summary.
***
***    DATA: lt_col     TYPE lvc_t_scol,
***          lv_sum_c   TYPE p LENGTH 13 DECIMALS 2,
***          lv_sum_v   TYPE p LENGTH 13 DECIMALS 2,
***          lv_waers_c TYPE waers,
***          lv_waers_v TYPE waers,
***
***    lv_base    TYPE p LENGTH 13 DECIMALS 2,
***          lv_other   TYPE p LENGTH 13 DECIMALS 2,
***          lv_waers_b TYPE waers,
***          lv_diff_tx TYPE char40.
***
****    CLEAR mt_out.
****
****    IF mt_bsid IS NOT INITIAL.
****      lv_sum_c   = REDUCE #( INIT sum = 0 FOR wa IN mt_bsid NEXT sum = sum + wa-wrbtr ).
****      lv_waers_c = mt_bsid[ 1 ]-waers.
****    ENDIF.
****
****    IF mt_bsik IS NOT INITIAL.
****      lv_sum_v   = REDUCE #( INIT sum = 0 FOR wa2 IN mt_bsik NEXT sum = sum + wa2-wrbtr ).
****      lv_waers_v = mt_bsik[ 1 ]-waers.
****    ENDIF.
****
****    lt_col = VALUE #( ( color-col = 7 color-int = 0 color-inv = 0 ) ).
****    APPEND VALUE #( text = 'Amount' amount = lv_sum_c waers = lv_waers_c color = lt_col ) TO mt_out.
****
****    lt_col = VALUE #( ( color-col = 5 color-int = 0 color-inv = 0 ) ).
****    APPEND VALUE #( text = 'Customer' amount = lv_sum_c waers = lv_waers_c color = lt_col ) TO mt_out.
****    APPEND VALUE #( text = 'Vendor'   amount = lv_sum_v waers = lv_waers_v color = lt_col ) TO mt_out.
****
****    lt_col = VALUE #( ( color-col = 3 color-int = 0 color-inv = 0 ) ).
****    APPEND VALUE #( text = 'Difference: Vendor to amount'
****                     amount = ( abs( lv_sum_v ) - lv_sum_c ) * -1
****                     waers  = lv_waers_c
****                     color  = lt_col ) TO mt_out.
***
***CLEAR mt_out.
***
***    IF mt_bsid IS NOT INITIAL.
***      lv_sum_c   = REDUCE #( INIT sum = 0 FOR wa IN mt_bsid NEXT sum = sum + wa-wrbtr ).
***      lv_waers_c = mt_bsid[ 1 ]-waers.
***    ENDIF.
***
***    IF mt_bsik IS NOT INITIAL.
***      lv_sum_v   = REDUCE #( INIT sum = 0 FOR wa2 IN mt_bsik NEXT sum = sum + wa2-wrbtr ).
***      lv_waers_v = mt_bsik[ 1 ]-waers.
***    ENDIF.
***
***    " Irány: Customer -> Vendor vagy Vendor -> Customer
***    IF mv_c2v = abap_true.
***      lv_base    = abs( lv_sum_c ).
***      lv_other   = abs( lv_sum_v ).
***      lv_waers_b = lv_waers_c.
***      lv_diff_tx = 'Difference: Vendor to amount'.
***    ELSE.
***      lv_base    = abs( lv_sum_v ).
***      lv_other   = abs( lv_sum_c ).
***      lv_waers_b = lv_waers_v.
***      lv_diff_tx = 'Difference: Customer to amount'.
***    ENDIF.
***
***    lt_col = VALUE #( ( color-col = 7 color-int = 0 color-inv = 0 ) ).
***    APPEND VALUE #( text = 'Amount' amount = lv_base waers = lv_waers_b color = lt_col ) TO mt_out.
***
***    lt_col = VALUE #( ( color-col = 5 color-int = 0 color-inv = 0 ) ).
***    APPEND VALUE #( text = 'Customer' amount = lv_sum_c waers = lv_waers_c color = lt_col ) TO mt_out.
***    APPEND VALUE #( text = 'Vendor'   amount = lv_sum_v waers = lv_waers_v color = lt_col ) TO mt_out.
***
***    lt_col = VALUE #( ( color-col = 3 color-int = 0 color-inv = 0 ) ).
***    APPEND VALUE #( text   = lv_diff_tx
***                    amount = lv_base - lv_other
***                    waers  = lv_waers_b
***                    color  = lt_col ) TO mt_out.
***
***  ENDMETHOD.
***
***  METHOD show_screen.
***
***    IF mt_bsid IS INITIAL AND mt_bsik IS INITIAL.
***      MESSAGE 'Nincs találat a megadott szűrésre' TYPE 'S' DISPLAY LIKE 'E'.
***      RETURN.
***    ENDIF.
***
***    CALL SCREEN 9000.
***
***  ENDMETHOD.
***
***  METHOD pbo.
***
***        SET PF-STATUS 'STATUS_9000'.
***        SET TITLEBAR  'TITLE_9000'.   " opcionális
***
***        IF mo_cc_top IS NOT BOUND.
***          build_alv_top( ).
***        ENDIF.
***
***        IF mo_cc_sum IS NOT BOUND.
***          build_alv_summary( ).
***        ENDIF.
***
***  ENDMETHOD.
***
***
***
***  METHOD build_alv_top.
***
***    CREATE OBJECT mo_cc_top
***      EXPORTING
***        container_name = 'CC_TOP'.   " Screen Painterben felvett Custom Container neve
***
***    CREATE OBJECT mo_splitter
***      EXPORTING
***        parent  = mo_cc_top
***        rows    = 1
***        columns = 2.
***
***    mo_cont_cust = mo_splitter->get_container( row = 1 column = 1 ).
***    mo_cont_vend = mo_splitter->get_container( row = 1 column = 2 ).
***
***    TRY.
***        cl_salv_table=>factory(
***          EXPORTING r_container  = mo_cont_cust
***          IMPORTING r_salv_table = mo_alv_cust
***          CHANGING  t_table      = mt_bsid ).
***
***        mo_alv_cust->get_functions( )->set_all( abap_false ).
***        mo_alv_cust->get_columns( )->set_optimize( abap_true ).
***        mo_alv_cust->get_display_settings( )->set_list_header( |Customer open items: { p_kunnr }| ).
***        mo_alv_cust->get_display_settings( )->set_striped_pattern( abap_true ).
***        mo_alv_cust->get_selections( )->set_selection_mode(
***  if_salv_c_selection_mode=>row_column ).
***        mo_alv_cust->display( ).
***
***      CATCH cx_salv_error.
***    ENDTRY.
***
***    TRY.
***        cl_salv_table=>factory(
***          EXPORTING r_container  = mo_cont_vend
***          IMPORTING r_salv_table = mo_alv_vend
***          CHANGING  t_table      = mt_bsik ).
***
***        mo_alv_vend->get_functions( )->set_all( abap_false ).
***        mo_alv_vend->get_columns( )->set_optimize( abap_true ).
***        mo_alv_vend->get_display_settings( )->set_list_header( |Vendor open items: { p_lifnr }| ).
***        mo_alv_vend->get_display_settings( )->set_striped_pattern( abap_true ).
***        mo_alv_vend->get_selections( )->set_selection_mode(
***  if_salv_c_selection_mode=>row_column ).
***        mo_alv_vend->display( ).
***
***      CATCH cx_salv_error.
***    ENDTRY.
***
***  ENDMETHOD.
***
***  METHOD create_proposal.
***
***    DATA: lt_bsid_sel TYPE STANDARD TABLE OF bsid,
***          lt_bsik_sel TYPE STANDARD TABLE OF bsik,
***          lt_fields   TYPE STANDARD TABLE OF sval,
***          lv_rc       TYPE c LENGTH 1,
***          lv_fm       TYPE rs38l_fnam,
***          ls_ctrl     TYPE ssfctrlop,
***          ls_out      TYPE ssfcompop,
***          lv_compno   TYPE c LENGTH 10.
***
***
***    " Munkatáblák a TXT állomány generálásához
***DATA: lt_txt_data TYPE STANDARD TABLE OF ty_txt_line,
***      lv_filename TYPE string,
***      lv_path     TYPE string,
***      lv_fullpath TYPE string.
***
****DATA: lt_bsik TYPE STANDARD TABLE OF bsik,
****      mo_alv_cust  TYPE REF TO cl_salv_table.
****
****DATA: lt_rows TYPE salv_t_row,
****      lv_row  TYPE salv_de_row.
****
****DATA: ls_bsik TYPE bsik,
****      lt_selected TYPE STANDARD TABLE OF bsik.
****
****lt_rows = mo_alv_cust->get_selections( )->get_selected_rows( ).
****
****LOOP AT lt_rows INTO lv_row.
****
****  READ TABLE mt_bsid INTO DATA(ls_bsid) INDEX lv_row.
****
****  IF sy-subrc = 0.
****    APPEND ls_bsid TO lt_bsid_sel.
****  ENDIF.
****
****ENDLOOP.
***
****  " 1. Kijelölt sorok összegyűjtése
****  LOOP AT mo_alv_cust->get_selections( )->get_selected_rows( ) INTO DATA(lv_row_c).
****    READ TABLE mt_bsid INTO DATA(ls_bsid) INDEX lv_row_c.
****    IF sy-subrc = 0.
****      APPEND ls_bsid TO lt_bsid_sel.
****    ENDIF.
****  ENDLOOP.
****
****  LOOP AT mo_alv_vend->get_selections( )->get_selected_rows( ) INTO DATA(lv_row_v).
****    READ TABLE mt_bsik INTO DATA(ls_bsik) INDEX lv_row_v.
****    IF sy-subrc = 0.
****      APPEND ls_bsik TO lt_bsik_sel.
****    ENDIF.
****  ENDLOOP.
***
***    lt_bsid_sel =  mt_bsid.
***    lt_bsik_sel =  mt_bsik.
***
***    IF lt_bsid_sel IS INITIAL AND lt_bsik_sel IS INITIAL.
***      MESSAGE 'Jelölj ki legalább egy sort' TYPE 'S' DISPLAY LIKE 'E'.
***      RETURN.
***    ENDIF.
***
***    " 2. Popup (Print compensation)
***    lt_fields = VALUE #(
****    ( tabname = 'T001' fieldname = 'BUKRS' value = p_bukrs )
***      ( tabname = 'T002' fieldname = 'SPRAS' value = sy-langu )
***       ).
***
***    mv_proposal_status = abap_true.
***
***    CALL FUNCTION 'POPUP_GET_VALUES'
***      EXPORTING
***        popup_title = 'Print compensation'
***      IMPORTING
***        returncode  = lv_rc
***      TABLES
***        fields      = lt_fields.
***
***    IF lv_rc IS NOT INITIAL.
***      RETURN.   " Mégse
***    ENDIF.
***
***    " 3. Teszt javaslatszám (később számkörből)
***    lv_compno = |{ sy-datum+4(4) }{ sy-uzeit(2) }{ sy-uzeit+2(2) }|.
***
***    " 4. Smartform függvénymodul neve
*******    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*******      EXPORTING
*******        formname = 'YY_KOMPENZACIO'
*******      IMPORTING
*******        fm_name  = lv_fm
*******      EXCEPTIONS
*******        OTHERS   = 1.
*******    IF sy-subrc <> 0.
*******      MESSAGE 'A YY_KOMPENZACIO Smartform nem található / nincs aktiválva' TYPE 'S' DISPLAY LIKE 'E'.
*******      RETURN.
*******    ENDIF.
*******
*******    " 5. Nyomtatás: a standard nyomtatási ablak (Output Device, Print Preview) magától jön
*******    ls_ctrl-no_dialog = abap_false.
*******    ls_ctrl-preview   = abap_false.
*******    ls_ctrl-langu     = lt_fields[ fieldname = 'SPRAS' ]-value.
*******    ls_out-tddest     = 'LOCL'.
*******    ls_out-tdnewid    = abap_true.
*******
*******    CALL FUNCTION lv_fm
*******      EXPORTING
*******        control_parameters = ls_ctrl
*******        output_options     = ls_out
*******        user_settings      = abap_true
*******        iv_bukrs           = p_bukrs
*******        iv_compno          = lv_compno
*******        iv_date            = p_date
*******      TABLES
*******        it_bsid            = lt_bsid_sel
*******        it_bsik            = lt_bsik_sel
*******      EXCEPTIONS
*******        formatting_error   = 1
*******        internal_error     = 2
*******        send_error         = 3
*******        user_canceled      = 4
*******        OTHERS             = 5.
*******
*******    IF sy-subrc <> 0 AND sy-subrc <> 4.
*******      MESSAGE 'Hiba a nyomtatványgenerálásnál' TYPE 'S' DISPLAY LIKE 'E'.
*******    ENDIF.
***
***APPEND VALUE #( line = |VEVŐI TÉTELEK ({ p_kunnr }):| ) TO lt_txt_data.
***LOOP AT lt_bsid_sel INTO DATA(ls_bsid).
***  APPEND VALUE #( line = |Dok.szám: { ls_bsid-belnr } | &&
***                         |Összeg: { ls_bsid-wrbtr } { ls_bsid-waers } | &&
***                         |Szöveg: { ls_bsid-sgtxt }| ) TO lt_txt_data.
***ENDLOOP.
***
***APPEND VALUE #( line = '--------------------------------------------------' ) TO lt_txt_data.
***APPEND VALUE #( line = |SZÁLLÍTÓI TÉTELEK ({ p_lifnr }):| ) TO lt_txt_data.
***LOOP AT lt_bsik_sel INTO DATA(ls_bsik).
***  APPEND VALUE #( line = |Dok.szám: { ls_bsik-belnr } | &&
***                         |Összeg: { ls_bsik-wrbtr } { ls_bsik-waers } | &&
***                         |Szöveg: { ls_bsik-sgtxt }| ) TO lt_txt_data.
***ENDLOOP.
***
***" Fájlmentés párbeszédablak megnyitása
***cl_gui_frontend_services=>file_save_dialog(
***  EXPORTING
***    default_extension   = 'txt'
***    default_file_name   = |kompenzacio_{ lv_compno }.txt|
***  CHANGING
***    filename            = lv_filename
***    path                = lv_path
***    fullpath            = lv_fullpath
***  EXCEPTIONS
***    OTHERS              = 1 ).
***
***IF sy-subrc = 0 AND lv_fullpath IS NOT INITIAL.
***  " Mentés a helyi gépre
***  cl_gui_frontend_services=>gui_download(
***    EXPORTING
***      filename                = lv_fullpath
***      filetype                = 'DAT'
***    CHANGING
***      data_tab                = lt_txt_data
***    EXCEPTIONS
***      OTHERS                  = 1 ).
***
***  IF sy-subrc = 0.
***    MESSAGE 'A TXT fájl sikeresen elmentve!' TYPE 'S'.
***  ELSE.
***    MESSAGE 'Hiba történt a fájl mentése során' TYPE 'S' DISPLAY LIKE 'E'.
***  ENDIF.
***ENDIF.
***
***
***  ENDMETHOD.
***
***  METHOD build_alv_summary.
***
***    CREATE OBJECT mo_cc_sum
***      EXPORTING
***        container_name = 'CC_SUMMARY'.   " Screen Painterben felvett 2. Custom Container neve
***
***    TRY.
***        cl_salv_table=>factory(
***          EXPORTING r_container  = mo_cc_sum
***          IMPORTING r_salv_table = mo_alv_sum
***          CHANGING  t_table      = mt_out ).
***
***        DATA(lo_cols) = mo_alv_sum->get_columns( ).
***        lo_cols->set_color_column( 'COLOR' ).
***        lo_cols->set_optimize( abap_true ).
***        lo_cols->get_column( 'TEXT'   )->set_short_text( ' ' ).
***        lo_cols->get_column( 'AMOUNT' )->set_medium_text( 'Loc.curr.amount' ).
***        lo_cols->get_column( 'WAERS'  )->set_short_text( 'Crcy' ).
***
***        DATA(lo_func) = mo_alv_sum->get_functions( ).
***        lo_func->set_default( abap_false ).
***        lo_func->add_function(
***          name     = 'REFRESH'
***          icon     = CONV string( icon_refresh )
***          text     = 'Refresh'
***          tooltip  = 'Refresh'
***          position = if_salv_c_function_position=>left_of_salv_functions ).
***        lo_func->add_function(
***          name     = 'CREATE_PROP'
***          icon     = CONV string( icon_okay )
***          text     = 'Create Proposal'
***          tooltip  = 'Create Proposal'
***          position = if_salv_c_function_position=>left_of_salv_functions ).
***
***        CREATE OBJECT mo_handler.
***        mo_handler->mo_app = me.
***        SET HANDLER mo_handler->on_func FOR mo_alv_sum->get_event( ).
***
***        mo_alv_sum->display( ).
***
***      CATCH cx_salv_error INTO DATA(lx_err).
***        MESSAGE lx_err TYPE 'E'.
***    ENDTRY.
***
***  ENDMETHOD.
***
***  METHOD do_refresh.
***
***    fill_summary( ).
***
***    mo_alv_sum->refresh( refresh_mode = if_salv_c_refresh=>soft ).
***    mo_alv_cust->refresh( refresh_mode = if_salv_c_refresh=>soft ).
***    mo_alv_vend->refresh( refresh_mode = if_salv_c_refresh=>soft ).
***
***  ENDMETHOD.
***
***  METHOD pai.
***
***    save_ok = ok_code.
***    CLEAR ok_code.
***
***    CASE save_ok.
***      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
***        LEAVE PROGRAM.
***
***      WHEN 'CREATE_PRO'.
***        create_proposal( ).
***
***      WHEN 'OVERVIEW'.
****        tit2 = 'Airports'.
***        CALL SELECTION-SCREEN 500.
***
***
***    ENDCASE.
***
***  ENDMETHOD.
***
***
***
***ENDCLASS.
***
****&---------------------------------------------------------------*
****& Vezérlés
****&---------------------------------------------------------------*
***
****&---------------------------------------------------------------*
****& Globális objektumreferencia
****&---------------------------------------------------------------*
***DATA: go_app TYPE REF TO lcl_open_items.
***
****SELECTION-SCREEN PUSHBUTTON /1(20) gv_button USER-COMMAND btn1.
***
***
***START-OF-SELECTION.
***
***
***
***  CREATE OBJECT go_app.
***
***  go_app->select_data(
***    iv_bukrs = p_bukrs
***    iv_c2v   = p_c2v
***    iv_v2c   = p_v2c
***    iv_kunnr = p_kunnr
***    iv_lifnr = p_lifnr
***    iv_date  = p_date
***    iv_text  = p_text ).
***
***  go_app->show_screen( ).
***
****&---------------------------------------------------------------*
****& Dynpro modulok (9000-es képernyő)
****&---------------------------------------------------------------*
***MODULE status_9000 OUTPUT.
***  go_app->pbo( ).
***ENDMODULE.
***
***MODULE user_command_9000 INPUT.
***  go_app->pai(  ).
***ENDMODULE.
****&---------------------------------------------------------------------*
****&      Module  STATUS_9100  OUTPUT
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
***MODULE status_9100 OUTPUT.
***
***  go_app->pbo( ).
***
***
***ENDMODULE.
****&---------------------------------------------------------------------*
****&      Module  USER_COMMAND_9100  INPUT
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
***MODULE user_command_9100 INPUT.
***  go_app->pai(  ).
***ENDMODULE.

*&---------------------------------------------------------------------*
*& Report YY_TESZT_O
*&---------------------------------------------------------------------*


*sdfdsfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

*TYPE-POOLS: icon.
*
**&---------------------------------------------------------------*
**& 1000-es szelekciós képernyő
**&---------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
*PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK b1.
*
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
*PARAMETERS: p_c2v RADIOBUTTON GROUP g1 DEFAULT 'X',
*            p_v2c RADIOBUTTON GROUP g1.
*SELECTION-SCREEN END OF BLOCK b2.
*
*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
*PARAMETERS: p_kunnr TYPE kna1-kunnr OBLIGATORY,
*            p_lifnr TYPE lfa1-lifnr OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK b3.
*
*SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
*PARAMETERS: p_date TYPE sy-datum OBLIGATORY DEFAULT sy-datum.
*PARAMETERS: p_text TYPE char50.
*SELECTION-SCREEN END OF BLOCK b4.
*
**&---------------------------------------------------------------*
**& Típusok (globális szinten, az osztályok előtt)
**&---------------------------------------------------------------*
*TYPES: BEGIN OF ty_out,
*         text   TYPE char40,
*         amount TYPE p LENGTH 13 DECIMALS 2,
*         waers  TYPE waers,
*         color  TYPE lvc_t_scol,
*       END OF ty_out.
*
*" Overview képernyő: fejsor
*TYPES: BEGIN OF ty_prop_hdr,
*         bukrs  TYPE bukrs,
*         compno TYPE c LENGTH 10,
*         lifnr  TYPE lifnr,
*         kunnr  TYPE kunnr,
*         datum  TYPE datum,
*         amount TYPE p LENGTH 13 DECIMALS 2,
*         waers  TYPE waers,
*         text   TYPE char50,
*       END OF ty_prop_hdr.
*
*" Overview képernyő: tételek (vevői + szállítói egy közös táblában)
*TYPES: BEGIN OF ty_prop_item,
*         bukrs  TYPE bukrs,
*         compno TYPE c LENGTH 10,
*         cnt    TYPE i,
*         lifnr  TYPE lifnr,
*         kunnr  TYPE kunnr,
*         zuonr  TYPE dzuonr,
*         amount TYPE p LENGTH 13 DECIMALS 2,
*         gjahr  TYPE gjahr,
*         belnr  TYPE belnr_d,
*         buzei  TYPE buzei,
*         augdt  TYPE augdt,
*         augbl  TYPE augbl,
*         zfbdt  TYPE dzfbdt,
*         shkzg  TYPE shkzg,
*         hkont  TYPE hkont,
*         xref3  TYPE xref3,
*       END OF ty_prop_item.
*
*TYPES: tt_bsid TYPE STANDARD TABLE OF bsid WITH DEFAULT KEY,
*       tt_bsik TYPE STANDARD TABLE OF bsik WITH DEFAULT KEY.
*
*DATA: ok_code TYPE sy-ucomm,
*      save_ok TYPE sy-ucomm.
*
**&---------------------------------------------------------------*
**& Event handler osztály - előre jelezzük az lcl_open_items-t
**&---------------------------------------------------------------*
*CLASS lcl_open_items DEFINITION DEFERRED.
*
*CLASS lcl_handler DEFINITION.
*  PUBLIC SECTION.
*    DATA: mo_app TYPE REF TO lcl_open_items.
*    METHODS on_func FOR EVENT added_function OF cl_salv_events_table
*      IMPORTING e_salv_function.
*ENDCLASS.
*
**&---------------------------------------------------------------*
**& Lokális osztály: az adatlekérdezés és megjelenítés logikája
**&---------------------------------------------------------------*
*CLASS lcl_open_items DEFINITION.
*
*  PUBLIC SECTION.
*    METHODS:
*      select_data
*        IMPORTING iv_bukrs TYPE bukrs
*                  iv_c2v   TYPE abap_bool
*                  iv_v2c   TYPE abap_bool
*                  iv_kunnr TYPE kna1-kunnr
*                  iv_lifnr TYPE lfa1-lifnr
*                  iv_date  TYPE sy-datum
*                  iv_text  TYPE char50,
*      show_screen,
*      pbo,
*      pai,
*      pbo_9100,
*      pai_9100,
*      create_proposal,
*      show_overview,
*      do_refresh.
*
*  PRIVATE SECTION.
*    DATA: mt_bsid TYPE tt_bsid,
*          mt_bsik TYPE tt_bsik,
*          mt_out  TYPE STANDARD TABLE OF ty_out.
*
*    " Elszámolás iránya (a rádiógombokból)
*    DATA: mv_c2v TYPE abap_bool,
*          mv_v2c TYPE abap_bool.
*
*    " 9000-es képernyő, 1. Custom Container ('CC_TOP')
*    DATA: mo_cc_top     TYPE REF TO cl_gui_custom_container,
*          mo_splitter   TYPE REF TO cl_gui_splitter_container,
*          mo_cont_cust  TYPE REF TO cl_gui_container,
*          mo_cont_vend  TYPE REF TO cl_gui_container,
*          mo_alv_cust   TYPE REF TO cl_salv_table,
*          mo_alv_vend   TYPE REF TO cl_salv_table.
*
*    " 9000-es képernyő, 2. Custom Container ('CC_SUMMARY')
*    DATA: mo_cc_sum     TYPE REF TO cl_gui_custom_container,
*          mo_alv_sum    TYPE REF TO cl_salv_table.
*
*    " 9100-as képernyő (Overview), Custom Container ('CC_PROP')
*    DATA: mt_prop_hdr   TYPE STANDARD TABLE OF ty_prop_hdr,
*          mt_prop_items TYPE STANDARD TABLE OF ty_prop_item,
*          mo_cc_prop    TYPE REF TO cl_gui_custom_container,
*          mo_split_prop TYPE REF TO cl_gui_splitter_container,
*          mo_alv_hdr    TYPE REF TO cl_salv_table,
*          mo_alv_items  TYPE REF TO cl_salv_table.
*
*    DATA: mo_handler TYPE REF TO lcl_handler.
*
*    METHODS: build_alv_top,
*             build_alv_summary,
*             build_alv_prop,
*             fill_summary,
*             get_selected
*               EXPORTING et_bsid  TYPE tt_bsid
*                         et_bsik  TYPE tt_bsik
*                         ev_found TYPE abap_bool,
*             build_proposal_data
*               IMPORTING it_bsid   TYPE tt_bsid
*                         it_bsik   TYPE tt_bsik
*                         iv_compno TYPE clike.
*
*ENDCLASS.
*
*CLASS lcl_handler IMPLEMENTATION.
*  METHOD on_func.
*    CASE e_salv_function.
*      WHEN 'REFRESH'.
*        mo_app->do_refresh( ).
*      WHEN 'CREATE_PROP'.
*        mo_app->create_proposal( ).
*      WHEN 'OVERVIEW'.
*        mo_app->show_overview( ).
*    ENDCASE.
*  ENDMETHOD.
*ENDCLASS.
*
*CLASS lcl_open_items IMPLEMENTATION.
*
**&---------------------------------------------------------------*
*  METHOD select_data.
*
*    " Irány eltárolása a fill_summary számára
*    mv_c2v = iv_c2v.
*    mv_v2c = iv_v2c.
*
*    SELECT * FROM bsid
*      INTO TABLE @mt_bsid
*      WHERE bukrs = @iv_bukrs
*        AND kunnr = @iv_kunnr
*        AND budat <= @iv_date.
*
*    IF iv_text IS NOT INITIAL.
*      DELETE mt_bsid WHERE sgtxt NP iv_text.
*    ENDIF.
*
*    SELECT * FROM bsik
*      INTO TABLE @mt_bsik
*      WHERE bukrs = @iv_bukrs
*        AND lifnr = @iv_lifnr
*        AND budat <= @iv_date.
*
*    IF iv_text IS NOT INITIAL.
*      DELETE mt_bsik WHERE sgtxt NP iv_text.
*    ENDIF.
*
*    fill_summary( ).
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD fill_summary.
*
*    DATA: lt_col     TYPE lvc_t_scol,
*          lv_sum_c   TYPE p LENGTH 13 DECIMALS 2,
*          lv_sum_v   TYPE p LENGTH 13 DECIMALS 2,
*          lv_waers_c TYPE waers,
*          lv_waers_v TYPE waers,
*          lv_base    TYPE p LENGTH 13 DECIMALS 2,
*          lv_other   TYPE p LENGTH 13 DECIMALS 2,
*          lv_waers_b TYPE waers,
*          lv_diff_tx TYPE char40.
*
*    CLEAR mt_out.
*
*    IF mt_bsid IS NOT INITIAL.
*      lv_sum_c   = REDUCE #( INIT sum = 0 FOR wa IN mt_bsid NEXT sum = sum + wa-wrbtr ).
*      lv_waers_c = mt_bsid[ 1 ]-waers.
*    ENDIF.
*
*    IF mt_bsik IS NOT INITIAL.
*      lv_sum_v   = REDUCE #( INIT sum = 0 FOR wa2 IN mt_bsik NEXT sum = sum + wa2-wrbtr ).
*      lv_waers_v = mt_bsik[ 1 ]-waers.
*    ENDIF.
*
*    " Irány: Customer -> Vendor vagy Vendor -> Customer
*    IF mv_c2v = abap_true.
*      lv_base    = abs( lv_sum_c ).
*      lv_other   = abs( lv_sum_v ).
*      lv_waers_b = lv_waers_c.
*      lv_diff_tx = 'Difference: Vendor to amount'.
*    ELSE.
*      lv_base    = abs( lv_sum_v ).
*      lv_other   = abs( lv_sum_c ).
*      lv_waers_b = lv_waers_v.
*      lv_diff_tx = 'Difference: Customer to amount'.
*    ENDIF.
*
*    lt_col = VALUE #( ( color-col = 7 color-int = 0 color-inv = 0 ) ).
*    APPEND VALUE #( text = 'Amount' amount = lv_base waers = lv_waers_b color = lt_col ) TO mt_out.
*
*    lt_col = VALUE #( ( color-col = 5 color-int = 0 color-inv = 0 ) ).
*    APPEND VALUE #( text = 'Customer' amount = lv_sum_c waers = lv_waers_c color = lt_col ) TO mt_out.
*    APPEND VALUE #( text = 'Vendor'   amount = lv_sum_v waers = lv_waers_v color = lt_col ) TO mt_out.
*
*    lt_col = VALUE #( ( color-col = 3 color-int = 0 color-inv = 0 ) ).
*    APPEND VALUE #( text   = lv_diff_tx
*                    amount = lv_base - lv_other
*                    waers  = lv_waers_b
*                    color  = lt_col ) TO mt_out.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD show_screen.
*
*    IF mt_bsid IS INITIAL AND mt_bsik IS INITIAL.
*      MESSAGE 'Nincs találat a megadott szűrésre' TYPE 'S' DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.
*
*    CALL SCREEN 9000.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD pbo.
*
*    SET PF-STATUS 'STATUS_9000'.
*    SET TITLEBAR  'TITLE_9000'.
*
*    IF mo_cc_top IS NOT BOUND.
*      build_alv_top( ).
*    ENDIF.
*
*    IF mo_cc_sum IS NOT BOUND.
*      build_alv_summary( ).
*    ENDIF.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD pai.
*
*    save_ok = ok_code.
*    CLEAR ok_code.
*
*    CASE save_ok.
*      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
*        LEAVE PROGRAM.
*
*
*    ENDCASE.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD build_alv_top.
*
*    CREATE OBJECT mo_cc_top
*      EXPORTING container_name = 'CC_TOP'.
*
*    CREATE OBJECT mo_splitter
*      EXPORTING
*        parent  = mo_cc_top
*        rows    = 1
*        columns = 2.
*
*    mo_cont_cust = mo_splitter->get_container( row = 1 column = 1 ).
*    mo_cont_vend = mo_splitter->get_container( row = 1 column = 2 ).
*
*    TRY.
*        cl_salv_table=>factory(
*          EXPORTING r_container  = mo_cont_cust
*          IMPORTING r_salv_table = mo_alv_cust
*          CHANGING  t_table      = mt_bsid ).
*
*        mo_alv_cust->get_functions( )->set_all( abap_false ).
*        mo_alv_cust->get_columns( )->set_optimize( abap_true ).
*        mo_alv_cust->get_display_settings( )->set_list_header( |Customer open items: { p_kunnr }| ).
*        mo_alv_cust->get_display_settings( )->set_striped_pattern( abap_true ).
*        mo_alv_cust->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>multiple ).
*        mo_alv_cust->display( ).
*
*      CATCH cx_salv_error.
*    ENDTRY.
*
*    TRY.
*        cl_salv_table=>factory(
*          EXPORTING r_container  = mo_cont_vend
*          IMPORTING r_salv_table = mo_alv_vend
*          CHANGING  t_table      = mt_bsik ).
*
*        mo_alv_vend->get_functions( )->set_all( abap_false ).
*        mo_alv_vend->get_columns( )->set_optimize( abap_true ).
*        mo_alv_vend->get_display_settings( )->set_list_header( |Vendor open items: { p_lifnr }| ).
*        mo_alv_vend->get_display_settings( )->set_striped_pattern( abap_true ).
*        mo_alv_vend->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>multiple ).
*        mo_alv_vend->display( ).
*
*      CATCH cx_salv_error.
*    ENDTRY.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD build_alv_summary.
*
*    CREATE OBJECT mo_cc_sum
*      EXPORTING container_name = 'CC_SUMMARY'.
*
*    TRY.
*        cl_salv_table=>factory(
*          EXPORTING r_container  = mo_cc_sum
*          IMPORTING r_salv_table = mo_alv_sum
*          CHANGING  t_table      = mt_out ).
*
*        DATA(lo_cols) = mo_alv_sum->get_columns( ).
*        lo_cols->set_color_column( 'COLOR' ).
*        lo_cols->set_optimize( abap_true ).
*        lo_cols->get_column( 'TEXT'   )->set_short_text( ' ' ).
*        lo_cols->get_column( 'AMOUNT' )->set_medium_text( 'Loc.curr.amount' ).
*        lo_cols->get_column( 'WAERS'  )->set_short_text( 'Crcy' ).
*
*        DATA(lo_func) = mo_alv_sum->get_functions( ).
*        lo_func->set_default( abap_false ).
*        lo_func->add_function(
*          name     = 'REFRESH'
*          icon     = CONV string( icon_refresh )
*          text     = 'Refresh'
*          tooltip  = 'Refresh'
*          position = if_salv_c_function_position=>left_of_salv_functions ).
*        lo_func->add_function(
*          name     = 'CREATE_PROP'
*          icon     = CONV string( icon_okay )
*          text     = 'Create Proposal'
*          tooltip  = 'Create Proposal'
*          position = if_salv_c_function_position=>left_of_salv_functions ).
*        lo_func->add_function(
*          name     = 'OVERVIEW'
*          icon     = CONV string( icon_display_more )
*          text     = 'Overview'
*          tooltip  = 'Overview of selected items'
*          position = if_salv_c_function_position=>left_of_salv_functions ).
*
*        CREATE OBJECT mo_handler.
*        mo_handler->mo_app = me.
*        SET HANDLER mo_handler->on_func FOR mo_alv_sum->get_event( ).
*
*        mo_alv_sum->display( ).
*
*      CATCH cx_salv_error INTO DATA(lx_err).
*        MESSAGE lx_err TYPE 'E'.
*    ENDTRY.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
**& Kijelölt sorok összegyűjtése a két ALV-ból
**&---------------------------------------------------------------*
*  METHOD get_selected.
*
*    CLEAR: et_bsid, et_bsik, ev_found.
*
*    cl_gui_cfw=>flush( ).
*
*    DATA(lt_rows_c) = mo_alv_cust->get_selections( )->get_selected_rows( ).
*    DATA(lt_rows_v) = mo_alv_vend->get_selections( )->get_selected_rows( ).
*
*    LOOP AT lt_rows_c INTO DATA(lv_row_c).
*      READ TABLE mt_bsid INTO DATA(ls_bsid) INDEX lv_row_c.
*      IF sy-subrc = 0.
*        APPEND ls_bsid TO et_bsid.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT lt_rows_v INTO DATA(lv_row_v).
*      READ TABLE mt_bsik INTO DATA(ls_bsik) INDEX lv_row_v.
*      IF sy-subrc = 0.
*        APPEND ls_bsik TO et_bsik.
*      ENDIF.
*    ENDLOOP.
*
*    IF et_bsid IS NOT INITIAL OR et_bsik IS NOT INITIAL.
*      ev_found = abap_true.
*    ENDIF.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
**& Create Proposal: kijelölés -> popup -> Smartform nyomtatás
**&---------------------------------------------------------------*
*  METHOD create_proposal.
*
*    DATA: lt_bsid_sel TYPE tt_bsid,
*          lt_bsik_sel TYPE tt_bsik,
*          lt_fields   TYPE STANDARD TABLE OF sval,
*          lv_rc       TYPE c LENGTH 1,
*          lv_fm       TYPE rs38l_fnam,
*          ls_ctrl     TYPE ssfctrlop,
*          ls_out      TYPE ssfcompop,
*          lv_compno   TYPE c LENGTH 10.
*
*    " 1. Kijelölt sorok összegyűjtése
*    get_selected( IMPORTING et_bsid  = lt_bsid_sel
*                            et_bsik  = lt_bsik_sel
*                            ev_found = DATA(lv_found) ).
*
*    IF lv_found = abap_false.
*      MESSAGE 'Jelölj ki legalább egy sort' TYPE 'S' DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.
*
*    " 2. Popup (Print compensation)
*    lt_fields = VALUE #(
*      ( tabname = 'T001' fieldname = 'BUKRS' value = p_bukrs )
*      ( tabname = 'T002' fieldname = 'SPRAS' value = sy-langu ) ).
*
*    CALL FUNCTION 'POPUP_GET_VALUES'
*      EXPORTING
*        popup_title = 'Print compensation'
*      IMPORTING
*        returncode  = lv_rc
*      TABLES
*        fields      = lt_fields.
*
*    IF lv_rc IS NOT INITIAL.
*      RETURN.   " Mégse
*    ENDIF.
*
*    " 3. Teszt javaslatszám (később számkörből)
*    lv_compno = |{ sy-datum+4(4) }{ sy-uzeit(2) }{ sy-uzeit+2(2) }|.
*
*    " 4. Smartform függvénymodul neve
*    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*      EXPORTING
*        formname = 'YY_KOMPENZACIO'
*      IMPORTING
*        fm_name  = lv_fm
*      EXCEPTIONS
*        OTHERS   = 1.
*    IF sy-subrc <> 0.
*      MESSAGE 'A YY_KOMPENZACIO Smartform nem található / nincs aktiválva' TYPE 'S' DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.
*
*    " 5. Nyomtatás
*    ls_ctrl-no_dialog = abap_false.
*    ls_ctrl-preview   = abap_false.
*    ls_ctrl-langu     = VALUE #( lt_fields[ fieldname = 'SPRAS' ]-value DEFAULT sy-langu ).
*    IF ls_ctrl-langu IS INITIAL.
*      ls_ctrl-langu = sy-langu.
*    ENDIF.
*    ls_out-tddest     = 'LOCL'.
*    ls_out-tdnewid    = abap_true.
*
*    CALL FUNCTION lv_fm
*      EXPORTING
*        control_parameters = ls_ctrl
*        output_options     = ls_out
*        user_settings      = abap_true
*        iv_bukrs           = p_bukrs
*        iv_compno          = lv_compno
*        iv_date            = p_date
*      TABLES
*        it_bsid            = lt_bsid_sel
*        it_bsik            = lt_bsik_sel
*      EXCEPTIONS
*        formatting_error   = 1
*        internal_error     = 2
*        send_error         = 3
*        user_canceled      = 4
*        OTHERS             = 5.
*
*    IF sy-subrc <> 0 AND sy-subrc <> 4.
*      MESSAGE 'Hiba a nyomtatványgenerálásnál' TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
**& Overview: csak megtekintő nézet a kijelölt tételekről
**&---------------------------------------------------------------*
*  METHOD show_overview.
*
*    DATA: lt_bsid_sel TYPE tt_bsid,
*          lt_bsik_sel TYPE tt_bsik.
*
*    get_selected( IMPORTING et_bsid  = lt_bsid_sel
*                            et_bsik  = lt_bsik_sel
*                            ev_found = DATA(lv_found) ).
*
*    IF lv_found = abap_false.
*      MESSAGE 'Jelölj ki legalább egy sort' TYPE 'S' DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.
*
*    " Nincs még javaslatszám, ez csak áttekintés
*    build_proposal_data( it_bsid   = lt_bsid_sel
*                         it_bsik   = lt_bsik_sel
*                         iv_compno = '' ).
*
*    CALL SCREEN 9100.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD build_proposal_data.
*
*    DATA: ls_item  TYPE ty_prop_item,
*          lv_cnt   TYPE i,
*          lv_sum_c TYPE p LENGTH 13 DECIMALS 2,
*          lv_sum_v TYPE p LENGTH 13 DECIMALS 2,
*          lv_waers TYPE waers.
*
*    CLEAR: mt_prop_hdr, mt_prop_items.
*
*    " Helyi pénznem a cégkódból
*    SELECT SINGLE waers FROM t001
*      WHERE bukrs = @p_bukrs
*      INTO @lv_waers.
*
*    " Előjel: H = pozitív, S = negatív
*    LOOP AT it_bsid INTO DATA(ls_d).
*      lv_cnt = lv_cnt + 1.
*      CLEAR ls_item.
*      ls_item-bukrs  = ls_d-bukrs.
*      ls_item-compno = iv_compno.
*      ls_item-cnt    = lv_cnt.
*      ls_item-kunnr  = ls_d-kunnr.
*      ls_item-zuonr  = ls_d-zuonr.
*      ls_item-amount = COND #( WHEN ls_d-shkzg = 'H' THEN ls_d-dmbtr ELSE ls_d-dmbtr * -1 ).
*      ls_item-gjahr  = ls_d-gjahr.
*      ls_item-belnr  = ls_d-belnr.
*      ls_item-buzei  = ls_d-buzei.
*      ls_item-augdt  = ls_d-augdt.
*      ls_item-augbl  = ls_d-augbl.
*      ls_item-zfbdt  = ls_d-zfbdt.
*      ls_item-shkzg  = ls_d-shkzg.
*      ls_item-hkont  = ls_d-hkont.
*      ls_item-xref3  = ls_d-xref3.
*      APPEND ls_item TO mt_prop_items.
*      lv_sum_c = lv_sum_c + ls_item-amount.
*    ENDLOOP.
*
*    LOOP AT it_bsik INTO DATA(ls_k).
*      lv_cnt = lv_cnt + 1.
*      CLEAR ls_item.
*      ls_item-bukrs  = ls_k-bukrs.
*      ls_item-compno = iv_compno.
*      ls_item-cnt    = lv_cnt.
*      ls_item-lifnr  = ls_k-lifnr.
*      ls_item-zuonr  = ls_k-zuonr.
*      ls_item-amount = COND #( WHEN ls_k-shkzg = 'H' THEN ls_k-dmbtr ELSE ls_k-dmbtr * -1 ).
*      ls_item-gjahr  = ls_k-gjahr.
*      ls_item-belnr  = ls_k-belnr.
*      ls_item-buzei  = ls_k-buzei.
*      ls_item-augdt  = ls_k-augdt.
*      ls_item-augbl  = ls_k-augbl.
*      ls_item-zfbdt  = ls_k-zfbdt.
*      ls_item-shkzg  = ls_k-shkzg.
*      ls_item-hkont  = ls_k-hkont.
*      ls_item-xref3  = ls_k-xref3.
*      APPEND ls_item TO mt_prop_items.
*      lv_sum_v = lv_sum_v + ls_item-amount.
*    ENDLOOP.
*
*    APPEND VALUE #( bukrs  = p_bukrs
*                    compno = iv_compno
*                    lifnr  = p_lifnr
*                    kunnr  = p_kunnr
*                    datum  = p_date
*                    amount = nmin( val1 = abs( lv_sum_c ) val2 = abs( lv_sum_v ) )
*                    waers  = lv_waers
*                    text   = p_text ) TO mt_prop_hdr.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD pbo_9100.
*
*    SET PF-STATUS 'STATUS_9100'.
*    SET TITLEBAR 'TITLE_9100'.
*
*    IF mo_cc_prop IS NOT BOUND.
*      build_alv_prop( ).
*    ENDIF.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD build_alv_prop.
*
*    CREATE OBJECT mo_cc_prop
*      EXPORTING container_name = 'CC_PROP'.
*
*    CREATE OBJECT mo_split_prop
*      EXPORTING
*        parent  = mo_cc_prop
*        rows    = 2
*        columns = 1.
*
*    mo_split_prop->set_row_height( id = 1 height = 25 ).
*
*    DATA(lo_top) = mo_split_prop->get_container( row = 1 column = 1 ).
*    DATA(lo_bot) = mo_split_prop->get_container( row = 2 column = 1 ).
*
*    TRY.
*        " --- Felső ALV: javaslat fejsor ---
*        cl_salv_table=>factory(
*          EXPORTING r_container  = lo_top
*          IMPORTING r_salv_table = mo_alv_hdr
*          CHANGING  t_table      = mt_prop_hdr ).
*
*        DATA(lo_cols_h) = mo_alv_hdr->get_columns( ).
*        lo_cols_h->set_optimize( abap_true ).
*        lo_cols_h->get_column( 'BUKRS'  )->set_short_text( 'CoCd' ).
*        lo_cols_h->get_column( 'COMPNO' )->set_visible( abap_false ).
*        lo_cols_h->get_column( 'LIFNR'  )->set_short_text( 'Supplier' ).
*        lo_cols_h->get_column( 'KUNNR'  )->set_short_text( 'Customer' ).
*        lo_cols_h->get_column( 'DATUM'  )->set_short_text( 'Comp Date' ).
*        lo_cols_h->get_column( 'AMOUNT' )->set_medium_text( 'CompAmount' ).
*        lo_cols_h->get_column( 'WAERS'  )->set_short_text( 'Crcy' ).
*        lo_cols_h->get_column( 'TEXT'   )->set_medium_text( 'Compensation text' ).
*        mo_alv_hdr->get_functions( )->set_default( abap_true ).
*        mo_alv_hdr->display( ).
*
*        " --- Alsó ALV: tételek ---
*        cl_salv_table=>factory(
*          EXPORTING r_container  = lo_bot
*          IMPORTING r_salv_table = mo_alv_items
*          CHANGING  t_table      = mt_prop_items ).
*
*        DATA(lo_cols_i) = mo_alv_items->get_columns( ).
*        lo_cols_i->set_optimize( abap_true ).
*        lo_cols_i->get_column( 'BUKRS'  )->set_short_text( 'CoCode' ).
*        lo_cols_i->get_column( 'COMPNO' )->set_visible( abap_false ).
*        lo_cols_i->get_column( 'CNT'    )->set_short_text( '1/No' ).
*        lo_cols_i->get_column( 'LIFNR'  )->set_short_text( 'Supplier' ).
*        lo_cols_i->get_column( 'KUNNR'  )->set_short_text( 'Customer' ).
*        lo_cols_i->get_column( 'ZUONR'  )->set_short_text( 'Assignm.' ).
*        lo_cols_i->get_column( 'AMOUNT' )->set_medium_text( 'Loc.curr.amount' ).
*        lo_cols_i->get_column( 'GJAHR'  )->set_short_text( 'Year' ).
*        lo_cols_i->get_column( 'BELNR'  )->set_medium_text( 'DocumentNo' ).
*        lo_cols_i->get_column( 'BUZEI'  )->set_short_text( 'Itm' ).
*        lo_cols_i->get_column( 'AUGDT'  )->set_medium_text( 'Clearing Date' ).
*        lo_cols_i->get_column( 'AUGBL'  )->set_medium_text( 'Clrng doc.' ).
*        lo_cols_i->get_column( 'ZFBDT'  )->set_medium_text( 'Baseline Date' ).
*        lo_cols_i->get_column( 'SHKZG'  )->set_short_text( 'D/C' ).
*        lo_cols_i->get_column( 'HKONT'  )->set_medium_text( 'G/L Account' ).
*        lo_cols_i->get_column( 'XREF3'  )->set_medium_text( 'Reference Key 3' ).
*        mo_alv_items->get_functions( )->set_default( abap_true ).
*        mo_alv_items->get_display_settings( )->set_striped_pattern( abap_true ).
*        mo_alv_items->display( ).
*
*      CATCH cx_salv_error INTO DATA(lx_err).
*        MESSAGE lx_err TYPE 'E'.
*    ENDTRY.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD pai_9100.
*
*    save_ok = ok_code.
*    CLEAR ok_code.
*
*    CASE save_ok.
*      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
*        IF mo_cc_prop IS BOUND.
*          mo_cc_prop->free( ).
*        ENDIF.
*        FREE: mo_cc_prop, mo_split_prop, mo_alv_hdr, mo_alv_items.
*        LEAVE TO SCREEN 0.
*    ENDCASE.
*
*  ENDMETHOD.
*
**&---------------------------------------------------------------*
*  METHOD do_refresh.
*
*    fill_summary( ).
*
*    mo_alv_sum->refresh( refresh_mode = if_salv_c_refresh=>soft ).
*    mo_alv_cust->refresh( refresh_mode = if_salv_c_refresh=>soft ).
*    mo_alv_vend->refresh( refresh_mode = if_salv_c_refresh=>soft ).
*
*  ENDMETHOD.
*
*ENDCLASS.
*
**&---------------------------------------------------------------*
**& Globális objektumreferencia
**&---------------------------------------------------------------*
*DATA: go_app TYPE REF TO lcl_open_items.
*
**&---------------------------------------------------------------*
**& Vezérlés
**&---------------------------------------------------------------*
*START-OF-SELECTION.
*
*  CREATE OBJECT go_app.
*
*  go_app->select_data(
*    iv_bukrs = p_bukrs
*    iv_c2v   = p_c2v
*    iv_v2c   = p_v2c
*    iv_kunnr = p_kunnr
*    iv_lifnr = p_lifnr
*    iv_date  = p_date
*    iv_text  = p_text ).
*
*  go_app->show_screen( ).
*
**&---------------------------------------------------------------*
**& Dynpro modulok
**&---------------------------------------------------------------*
*MODULE status_9000 OUTPUT.
*  go_app->pbo( ).
*ENDMODULE.
*
*MODULE user_command_9000 INPUT.
*  go_app->pai( ).
*ENDMODULE.
*
*MODULE status_9100 OUTPUT.
*  go_app->pbo_9100( ).
*ENDMODULE.
*
*MODULE user_command_9100 INPUT.
*  go_app->pai_9100( ).
*ENDMODULE.



*************************************************-

*&---------------------------------------------------------------------*
*& Report YY_TESZT_O
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------*
*& 1000-es szelekciós képernyő
*&---------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report YY_TESZT_O
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------*
*& 1000-es szelekciós képernyő
*&---------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_c2v RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_v2c RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
PARAMETERS: p_kunnr TYPE kna1-kunnr OBLIGATORY,
            p_lifnr TYPE lfa1-lifnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
PARAMETERS: p_date TYPE sy-datum OBLIGATORY DEFAULT sy-datum.
PARAMETERS: p_text TYPE char50.
SELECTION-SCREEN END OF BLOCK b4.

*&---------------------------------------------------------------*
*& Típusok (globális szinten, az osztály előtt)
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

TYPES: tt_bsid TYPE STANDARD TABLE OF bsid WITH DEFAULT KEY,
       tt_bsik TYPE STANDARD TABLE OF bsik WITH DEFAULT KEY.

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.

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

    " 9000-es képernyő, 1. Custom Container ('CC_TOP')
    DATA: mo_cc_top     TYPE REF TO cl_gui_custom_container,
          mo_splitter   TYPE REF TO cl_gui_splitter_container,
          mo_cont_cust  TYPE REF TO cl_gui_container,
          mo_cont_vend  TYPE REF TO cl_gui_container,
          mo_alv_cust   TYPE REF TO cl_salv_table,
          mo_alv_vend   TYPE REF TO cl_salv_table.

    " 9000-es képernyő, 2. Custom Container ('CC_SUMMARY')
    DATA: mo_cc_sum     TYPE REF TO cl_gui_custom_container,
          mo_alv_sum    TYPE REF TO cl_salv_table.

    " 9100-as képernyő (Overview), Custom Container ('CC_PROP')
    DATA: mt_prop_hdr   TYPE STANDARD TABLE OF ty_prop_hdr,
          mt_prop_items TYPE STANDARD TABLE OF ty_prop_item,
          mo_cc_prop    TYPE REF TO cl_gui_custom_container,
          mo_split_prop TYPE REF TO cl_gui_splitter_container,
          mo_alv_hdr    TYPE REF TO cl_salv_table,
          mo_alv_items  TYPE REF TO cl_salv_table.

    METHODS: build_alv_top,
             build_alv_summary,
             build_alv_prop,
             fill_summary,
             do_refresh,
             create_proposal,
             show_overview,
             build_proposal_data
               IMPORTING it_bsid   TYPE tt_bsid
                         it_bsik   TYPE tt_bsik
                         iv_compno TYPE clike.

ENDCLASS.

CLASS lcl_open_items IMPLEMENTATION.

*&---------------------------------------------------------------*
  METHOD select_data.

    " Irány eltárolása a fill_summary számára
    mv_c2v = iv_c2v.
    mv_v2c = iv_v2c.

    SELECT * FROM bsid
      INTO TABLE @mt_bsid
      WHERE bukrs = @iv_bukrs
        AND kunnr = @iv_kunnr
        AND budat <= @iv_date.

    IF iv_text IS NOT INITIAL.
      DELETE mt_bsid WHERE sgtxt NP iv_text.
    ENDIF.

    SELECT * FROM bsik
      INTO TABLE @mt_bsik
      WHERE bukrs = @iv_bukrs
        AND lifnr = @iv_lifnr
        AND budat <= @iv_date.

    IF iv_text IS NOT INITIAL.
      DELETE mt_bsik WHERE sgtxt NP iv_text.
    ENDIF.

    fill_summary( ).

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
      lv_sum_c   = REDUCE #( INIT sum = 0 FOR wa IN mt_bsid NEXT sum = sum + wa-wrbtr ).
      lv_waers_c = mt_bsid[ 1 ]-waers.
    ENDIF.

    IF mt_bsik IS NOT INITIAL.
      lv_sum_v   = REDUCE #( INIT sum = 0 FOR wa2 IN mt_bsik NEXT sum = sum + wa2-wrbtr ).
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
      MESSAGE 'Nincs találat a megadott szűrésre' TYPE 'S' DISPLAY LIKE 'E'.
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
*& 9000-es képernyő PAI: a gombok a GUI státuszról jönnek
*&---------------------------------------------------------------*
  METHOD pai.

    save_ok = ok_code.
    CLEAR ok_code.

    CASE save_ok.
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
        LEAVE PROGRAM.

      WHEN 'REFRESH'.
        do_refresh( ).

      WHEN 'CREATE_PROP' OR 'CREATE_PRO'.
        create_proposal( ).

      WHEN 'OVERVIEW'.
        show_overview( ).
    ENDCASE.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Vevői és szállítói nyitott tételek (csak megjelenítés)
*&---------------------------------------------------------------*
  METHOD build_alv_top.

    CREATE OBJECT mo_cc_top
      EXPORTING container_name = 'CC_TOP'.

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
        mo_alv_cust->get_columns( )->set_optimize( abap_true ).
        mo_alv_cust->get_display_settings( )->set_list_header( |Customer open items: { p_kunnr }| ).
        mo_alv_cust->get_display_settings( )->set_striped_pattern( abap_true ).
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
        mo_alv_vend->display( ).

      CATCH cx_salv_error.
    ENDTRY.

  ENDMETHOD.

*&---------------------------------------------------------------*
*& Summary ALV: nincs saját gomb, az eszköztár ki van kapcsolva
*&---------------------------------------------------------------*
  METHOD build_alv_summary.

    CREATE OBJECT mo_cc_sum
      EXPORTING container_name = 'CC_SUMMARY'.

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
*& Create Proposal: popup -> Smartform nyomtatás (összes tétel)
*&---------------------------------------------------------------*
  METHOD create_proposal.

    DATA: lt_fields TYPE STANDARD TABLE OF sval,
          lv_rc     TYPE c LENGTH 1,
          lv_fm     TYPE rs38l_fnam,
          ls_ctrl   TYPE ssfctrlop,
          ls_out    TYPE ssfcompop,
          lv_compno TYPE c LENGTH 10.

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

    " 2. Teszt javaslatszám (később számkörből)
    lv_compno = |{ sy-datum+4(4) }{ sy-uzeit(2) }{ sy-uzeit+2(2) }|.

    " 3. Smartform függvénymodul neve
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = 'YY_KOMPENZACIO'
      IMPORTING
        fm_name  = lv_fm
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE 'A YY_KOMPENZACIO Smartform nem található / nincs aktiválva' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " 4. Nyomtatás
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
        it_bsid            = mt_bsid
        it_bsik            = mt_bsik
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
*& Overview: csak megtekintő nézet az összes tételről
*&---------------------------------------------------------------*
  METHOD show_overview.

    " Nincs még javaslatszám, ez csak áttekintés
    build_proposal_data( it_bsid   = mt_bsid
                         it_bsik   = mt_bsik
                         iv_compno = '' ).

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
      EXPORTING container_name = 'CC_PROP'.

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
  METHOD pai_9100.

    save_ok = ok_code.
    CLEAR ok_code.

    CASE save_ok.
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
        IF mo_cc_prop IS BOUND.
          mo_cc_prop->free( ).
        ENDIF.
        FREE: mo_cc_prop, mo_split_prop, mo_alv_hdr, mo_alv_items.
        LEAVE TO SCREEN 0.
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
