REPORT ZCDS_VIEW.
***Begin of Selection Screen
PARAMETERS: p_table TYPE vibastab.
***End of Selection Screen

***Begin of Data declaration
TYPES: BEGIN OF ty_cdsview,
                table       TYPE vibastab,
                cdsview(40) TYPE c,
       END OF   ty_cdsview,
                lt_cdsview  TYPE STANDARD TABLE OF ty_cdsview
                            WITH DEFAULT KEY.
***End of Data declaration

***To get the View name based on the table
SELECT viewname,as4local,objpos,as4vers,tabname FROM dd27s
                                                INTO TABLE @DATA(lt_dd27s)
                                                WHERE tabname = @p_table.

IF lt_dd27s[] IS NOT INITIAL.
  SELECT * FROM acm_ddlstbviw_1r INTO TABLE @DATA(lt_cds)
                                 FOR ALL ENTRIES IN @lt_dd27s
                                 WHERE  view_name = @lt_dd27s-viewname.

  DATA(lt_cdsview1) = VALUE lt_cdsview( FOR ls_dd27s  IN  lt_dd27s
                      FOR ls_cds IN lt_cds FROM line_index( lt_cds[ view_name = ls_dd27s-viewname ] )
                      WHERE ( view_name = ls_dd27s-viewname )
                      LET ls_final      = VALUE ty_cdsview(
                          table         = ls_dd27s-tabname
                          cdsview       = ls_cds-ddls_name )
                      IN ( CORRESPONDING #( BASE ( ls_final ) ls_dd27s ) ) ).
ENDIF.


***Unique records based table and View
data(list_of_cdsviews) = value lt_cdsview( FOR GROUPS gr1 of ls_cdsview1  in lt_cdsview1
                                   GROUP BY ( table   = ls_cdsview1-table
                                              cdsview = ls_cdsview1-cdsview
                                              count  = group size ) ASCENDING
                                              ( table = gr1-table
                                                cdsview = gr1-cdsview ) ).

DATA(lv_count) = lines( list_of_cdsviews ).
WRITE:/ 'no.of records:', lv_count.
***Display the data
cl_demo_output=>display_data( list_of_cdsviews ).

