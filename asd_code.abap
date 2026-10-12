
REPORT yypm_comp.

TABLES: bsid, bsik.

*---------------------------------------------------------------------*
* Selection screen
*---------------------------------------------------------------------*
PARAMETERS:
  p_bukrs TYPE bukrs OBLIGATORY,
  p_kunnr TYPE kunnr,
  p_lifnr TYPE lifnr.

*---------------------------------------------------------------------*
* Data
*---------------------------------------------------------------------*
DATA:
  gv_kunnr TYPE kunnr,
  gv_lifnr TYPE lifnr.

DATA:
  gt_bsid TYPE zpm_comp_bsid_tt,
  gt_bsik TYPE zpm_comp_bsik_tt.

DATA:
  gt_sel_bsid TYPE zpm_comp_bsid_tt,
  gt_sel_bsik TYPE zpm_comp_bsik_tt,
  gt_summary  TYPE zpm_comp_sum_tt.

DATA:
  gt_cust_rows TYPE salv_t_row,
  gt_vend_rows TYPE salv_t_row.

DATA:
  go_cust_container TYPE REF TO cl_gui_custom_container,
  go_vend_container TYPE REF TO cl_gui_custom_container,
  go_sum_container  TYPE REF TO cl_gui_custom_container.

DATA:
  go_cust_alv TYPE REF TO cl_salv_table,
  go_vend_alv TYPE REF TO cl_salv_table,
  go_sum_alv  TYPE REF TO cl_salv_table.

DATA:
  gv_alv_created TYPE abap_bool VALUE abap_false.

*---------------------------------------------------------------------*
* Selection validation
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF p_kunnr IS INITIAL AND p_lifnr IS INITIAL.
    MESSAGE 'Adj meg vevőt vagy szállítót!' TYPE 'E'.
  ENDIF.

  IF p_kunnr IS NOT INITIAL AND p_lifnr IS NOT INITIAL.
    MESSAGE 'Egyszerre csak vevőt vagy szállítót adj meg!'
      TYPE 'E'.
  ENDIF.

*---------------------------------------------------------------------*
* Start
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM resolve_partners.
  PERFORM load_open_items.

  CALL SCREEN 0200.

*---------------------------------------------------------------------*
* Customer -> Vendor OR Vendor -> Customer
*---------------------------------------------------------------------*
FORM resolve_partners.

  CLEAR: gv_kunnr, gv_lifnr.

  IF p_kunnr IS NOT INITIAL.

    gv_kunnr = p_kunnr.

    SELECT SINGLE lifnr
      FROM kna1
      INTO gv_lifnr
      WHERE kunnr = p_kunnr.

    IF sy-subrc <> 0.
      MESSAGE 'A vevő nem található a KNA1 táblában!'
        TYPE 'E'.
    ENDIF.

    IF gv_lifnr IS INITIAL.
      MESSAGE 'A vevőhöz nincs hozzárendelt szállító!'
        TYPE 'E'.
    ENDIF.

  ELSEIF p_lifnr IS NOT INITIAL.

    gv_lifnr = p_lifnr.

    SELECT SINGLE kunnr
      FROM lfa1
      INTO gv_kunnr
      WHERE lifnr = p_lifnr.

    IF sy-subrc <> 0.
      MESSAGE 'A szállító nem található az LFA1 táblában!'
        TYPE 'E'.
    ENDIF.

    IF gv_kunnr IS INITIAL.
      MESSAGE 'A szállítóhoz nincs hozzárendelt vevő!'
        TYPE 'E'.
    ENDIF.

  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Read open items
*---------------------------------------------------------------------*
FORM load_open_items.

  CLEAR: gt_bsid, gt_bsik.

  SELECT *
    FROM bsid
    INTO TABLE gt_bsid
    UP TO 50 ROWS
    WHERE bukrs = p_bukrs
      AND kunnr = gv_kunnr.

  SELECT *
    FROM bsik
    INTO TABLE gt_bsik
    UP TO 50 ROWS
    WHERE bukrs = p_bukrs
      AND lifnr = gv_lifnr.

  IF gt_bsid IS INITIAL AND gt_bsik IS INITIAL.
    MESSAGE 'Nincs nyitott vevői vagy szállítói tétel.'
      TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Dynpro 0200 - PBO
*---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'ZCOMP_200'.
  SET TITLEBAR 'ZCOMP_TITLE'.

ENDMODULE.

MODULE display_0200 OUTPUT.

  IF gv_alv_created = abap_false.

    PERFORM create_alv.

    gv_alv_created = abap_true.

  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
* Dynpro 0200 - PAI
*---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.

    WHEN 'REFRESH'.

      PERFORM build_summary.

      IF go_sum_alv IS BOUND.
        go_sum_alv->refresh( ).
      ENDIF.

    WHEN 'PDF'.

      PERFORM build_selected_items.
      PERFORM build_summary.
      PERFORM create_pdf.

      IF go_sum_alv IS BOUND.
        go_sum_alv->refresh( ).
      ENDIF.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.

      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*---------------------------------------------------------------------*
* Create the three SALV lists
*---------------------------------------------------------------------*
FORM create_alv.

  CREATE OBJECT go_cust_container
    EXPORTING
      container_name = 'CC_CUST'.

  CREATE OBJECT go_vend_container
    EXPORTING
      container_name = 'CC_VEND'.

  CREATE OBJECT go_sum_container
    EXPORTING
      container_name = 'CC_SUM'.

  TRY.

      cl_salv_table=>factory(
        EXPORTING
          r_container  = go_cust_container
        IMPORTING
          r_salv_table = go_cust_alv
        CHANGING
          t_table      = gt_bsid ).

      go_cust_alv->get_functions( )->set_all( abap_true ).

      go_cust_alv->get_selections( )->set_selection_mode(
        if_salv_c_selection_mode=>multiple ).

      go_cust_alv->get_display_settings( )->set_list_header(
        'Vevői nyitott tételek - BSID' ).

      go_cust_alv->display( ).

      cl_salv_table=>factory(
        EXPORTING
          r_container  = go_vend_container
        IMPORTING
          r_salv_table = go_vend_alv
        CHANGING
          t_table      = gt_bsik ).

      go_vend_alv->get_functions( )->set_all( abap_true ).

      go_vend_alv->get_selections( )->set_selection_mode(
        if_salv_c_selection_mode=>multiple ).

      go_vend_alv->get_display_settings( )->set_list_header(
        'Szállítói nyitott tételek - BSIK' ).

      go_vend_alv->display( ).

      cl_salv_table=>factory(
        EXPORTING
          r_container  = go_sum_container
        IMPORTING
          r_salv_table = go_sum_alv
        CHANGING
          t_table      = gt_summary ).

      go_sum_alv->get_functions( )->set_all( abap_true ).

      go_sum_alv->get_display_settings( )->set_list_header(
        'Kiválasztott tételek összesítése' ).

      go_sum_alv->display( ).

    CATCH cx_salv_msg.

      MESSAGE 'Hiba a SALV listák létrehozásakor!' TYPE 'I'.

  ENDTRY.

ENDFORM.

*---------------------------------------------------------------------*
* Collect selected rows from both SALV lists
*---------------------------------------------------------------------*
FORM build_selected_items.

  DATA:
    lv_row TYPE salv_de_row,
    ls_bsid TYPE bsid,
    ls_bsik TYPE bsik.

  CLEAR:
    gt_sel_bsid,
    gt_sel_bsik,
    gt_cust_rows,
    gt_vend_rows.

  IF go_cust_alv IS BOUND.

    gt_cust_rows =
      go_cust_alv->get_selections( )->get_selected_rows( ).

    LOOP AT gt_cust_rows INTO lv_row.

      READ TABLE gt_bsid INTO ls_bsid INDEX lv_row.

      IF sy-subrc = 0.
        APPEND ls_bsid TO gt_sel_bsid.
      ENDIF.

    ENDLOOP.

  ENDIF.

  IF go_vend_alv IS BOUND.

    gt_vend_rows =
      go_vend_alv->get_selections( )->get_selected_rows( ).

    LOOP AT gt_vend_rows INTO lv_row.

      READ TABLE gt_bsik INTO ls_bsik INDEX lv_row.

      IF sy-subrc = 0.
        APPEND ls_bsik TO gt_sel_bsik.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Build summary from selected rows
* Amounts are grouped by document currency (WAERS).
*---------------------------------------------------------------------*
FORM build_summary.

  DATA:
    ls_bsid TYPE bsid,
    ls_bsik TYPE bsik,
    ls_sum  TYPE zpm_comp_sum,
    lv_amount TYPE wrbtr.

  PERFORM build_selected_items.

  CLEAR gt_summary.

  LOOP AT gt_sel_bsid INTO ls_bsid.

    CLEAR ls_sum.

    READ TABLE gt_summary INTO ls_sum
      WITH KEY waers = ls_bsid-waers.

    IF sy-subrc <> 0.
      CLEAR ls_sum.
      ls_sum-waers = ls_bsid-waers.
    ENDIF.

    lv_amount = ls_bsid-wrbtr.

    IF ls_bsid-shkzg = 'H'.
      lv_amount = 0 - lv_amount.
    ENDIF.

    ls_sum-cust_total = ls_sum-cust_total + lv_amount.
    ls_sum-net_total  = ls_sum-net_total  + lv_amount.

    DELETE gt_summary WHERE waers = ls_sum-waers.
    APPEND ls_sum TO gt_summary.

  ENDLOOP.

  LOOP AT gt_sel_bsik INTO ls_bsik.

    CLEAR ls_sum.

    READ TABLE gt_summary INTO ls_sum
      WITH KEY waers = ls_bsik-waers.

    IF sy-subrc <> 0.
      CLEAR ls_sum.
      ls_sum-waers = ls_bsik-waers.
    ENDIF.

    lv_amount = ls_bsik-wrbtr.

    IF ls_bsik-shkzg = 'H'.
      lv_amount = 0 - lv_amount.
    ENDIF.

    ls_sum-vend_total = ls_sum-vend_total + lv_amount.
    ls_sum-net_total  = ls_sum-net_total  + lv_amount.

    DELETE gt_summary WHERE waers = ls_sum-waers.
    APPEND ls_sum TO gt_summary.

  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
* Create PDF from Smart Form YYPM_COMP_SF
*---------------------------------------------------------------------*
FORM create_pdf.

  DATA:
    lv_fm_name   TYPE rs38l_fnam,
    lv_pdf       TYPE xstring,
    lv_pdf_size  TYPE i,
    lv_filename  TYPE string,
    lv_path      TYPE string,
    lv_fullpath  TYPE string,
    lv_user_action TYPE i,
    lt_binary    TYPE solix_tab,
    ls_control   TYPE ssfctrlop,
    ls_output    TYPE ssfcrescl.

  IF gt_sel_bsid IS INITIAL AND gt_sel_bsik IS INITIAL.
    MESSAGE 'Először jelölj ki legalább egy sort!' TYPE 'S'
      DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'YYPM_COMP_SF'
    IMPORTING
      fm_name            = lv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE 'A YYPM_COMP_SF Smart Form nem található!'
      TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CLEAR ls_control.
  ls_control-no_dialog = abap_true.
  ls_control-getotf    = abap_true.

  CALL FUNCTION lv_fm_name
    EXPORTING
      control_parameters = ls_control
      user_settings      = space
      iv_bukrs           = p_bukrs
      iv_kunnr           = gv_kunnr
      iv_lifnr           = gv_lifnr
    IMPORTING
      job_output_info    = ls_output
    TABLES
      it_customer        = gt_sel_bsid
      it_vendor          = gt_sel_bsik
      it_summary         = gt_summary
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE 'Hiba a Smart Form generálásakor!'
      TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format       = 'PDF'
    IMPORTING
      bin_file     = lv_pdf
      bin_filesize = lv_pdf_size
    TABLES
      otf          = ls_output-otfdata
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      OTHERS                = 4.

  IF sy-subrc <> 0 OR lv_pdf IS INITIAL.
    MESSAGE 'A PDF konvertálása nem sikerült!'
      TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_pdf
    TABLES
      binary_tab    = lt_binary.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension = 'pdf'
      default_file_name = 'Kompenzacio.pdf'
      file_filter       = 'PDF fájl (*.pdf)|*.pdf|'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath
      user_action       = lv_user_action
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      not_supported_by_gui = 3
      OTHERS            = 4.

  IF sy-subrc <> 0 OR lv_user_action <> 0
     OR lv_fullpath IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename     = lv_fullpath
      filetype     = 'BIN'
      bin_filesize = lv_pdf_size
    TABLES
      data_tab     = lt_binary
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed  = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error                = 11
      disk_full               = 12
      dp_timeout              = 13
      OTHERS                  = 14.

  IF sy-subrc = 0.
    MESSAGE 'A PDF sikeresen elkészült.' TYPE 'S'.
  ELSE.
    MESSAGE 'A PDF mentése nem sikerült.' TYPE 'S'
      DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
