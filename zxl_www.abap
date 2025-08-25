*&———————————————————————*
*& ABAP OOP Asset Management - Interface Implementation Example
*&———————————————————————*

*–––––––––––––––––––––––––––––––––––*

- Interface Definitions
  *–––––––––––––––––––––––––––––––––––*

“ Main Asset Interface
INTERFACE if_asset.
TYPES: BEGIN OF ty_asset_data,
bukrs TYPE bukrs,           “ Company Code
anln1 TYPE anln1,           “ Asset Number
anln2 TYPE anln2,           “ Sub Number
txt50 TYPE txt50,           “ Description
anlkl TYPE anlkl,           “ Asset Class
werks TYPE werks_d,         “ Plant
END OF ty_asset_data.

TYPES: BEGIN OF ty_depreciation,
gjahr TYPE gjahr,           “ Fiscal Year
afabe TYPE afabe,           “ Depreciation Area
nafap TYPE nafap,           “ Posted Depreciation
answt TYPE answt,           “ Book Value
END OF ty_depreciation.

TYPES: tt_asset_data TYPE TABLE OF ty_asset_data,
tt_depreciation TYPE TABLE OF ty_depreciation.

METHODS: get_asset_data
IMPORTING iv_bukrs TYPE bukrs
iv_anln1 TYPE anln1
RETURNING VALUE(rs_asset) TYPE ty_asset_data
EXCEPTIONS asset_not_found,

```
       calculate_depreciation
         IMPORTING iv_bukrs TYPE bukrs
                  iv_anln1 TYPE anln1
                  iv_gjahr TYPE gjahr
         RETURNING VALUE(rt_depreciation) TYPE tt_depreciation
         EXCEPTIONS calculation_error,

       validate_asset
         IMPORTING is_asset TYPE ty_asset_data
         RETURNING VALUE(rv_valid) TYPE abap_bool.
```

ENDINTERFACE.

“ Depreciation Calculation Interface
INTERFACE if_depreciation_calculator.
METHODS: calculate_linear
IMPORTING iv_acquisition_value TYPE p
iv_useful_life TYPE i
iv_salvage_value TYPE p DEFAULT 0
RETURNING VALUE(rv_annual_depreciation) TYPE p,

```
       calculate_declining_balance
         IMPORTING iv_book_value TYPE p
                  iv_rate TYPE p
         RETURNING VALUE(rv_depreciation) TYPE p,

       get_depreciation_method
         IMPORTING iv_asset_class TYPE anlkl
         RETURNING VALUE(rv_method) TYPE string.
```

ENDINTERFACE.

“ Asset Validation Interface
INTERFACE if_asset_validator.
METHODS: validate_company_code
IMPORTING iv_bukrs TYPE bukrs
RETURNING VALUE(rv_valid) TYPE abap_bool,

```
       validate_asset_class
         IMPORTING iv_anlkl TYPE anlkl
         RETURNING VALUE(rv_valid) TYPE abap_bool,

       validate_useful_life
         IMPORTING iv_ndjar TYPE ndjar
                  iv_anlkl TYPE anlkl
         RETURNING VALUE(rv_valid) TYPE abap_bool.
```

ENDINTERFACE.

“ Posting Interface
INTERFACE if_asset_posting.
TYPES: BEGIN OF ty_posting_data,
bukrs TYPE bukrs,
anln1 TYPE anln1,
anln2 TYPE anln2,
afabe TYPE afabe,
anbtr TYPE anbtr,           “ Amount
bzdat TYPE bzdat,           “ Reference Date
bldat TYPE bldat,           “ Document Date
bktxt TYPE bktxt,           “ Document Header Text
END OF ty_posting_data.

METHODS: post_acquisition
IMPORTING is_posting TYPE ty_posting_data
RETURNING VALUE(rv_document_number) TYPE belnr
EXCEPTIONS posting_error,

```
       post_depreciation
         IMPORTING is_posting TYPE ty_posting_data
         RETURNING VALUE(rv_document_number) TYPE belnr
         EXCEPTIONS posting_error,

       reverse_posting
         IMPORTING iv_belnr TYPE belnr
                  iv_gjahr TYPE gjahr
                  iv_bukrs TYPE bukrs
         RETURNING VALUE(rv_success) TYPE abap_bool.
```

ENDINTERFACE.

*–––––––––––––––––––––––––––––––––––*

- Base Asset Class
  *–––––––––––––––––––––––––––––––––––*
  CLASS cl_asset_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
  INTERFACES: if_asset.
  
  METHODS: constructor
  IMPORTING iv_bukrs TYPE bukrs
  iv_anln1 TYPE anln1.
  
  PROTECTED SECTION.
  DATA: mv_bukrs TYPE bukrs,
  mv_anln1 TYPE anln1,
  ms_asset_data TYPE if_asset=>ty_asset_data.
  
  METHODS: load_asset_data
  EXCEPTIONS asset_not_found,
  
  ```
         get_asset_class_config
           IMPORTING iv_anlkl TYPE anlkl
           RETURNING VALUE(rs_config) TYPE t093c.
  ```
  
  PRIVATE SECTION.
  METHODS: validate_input_parameters
  EXCEPTIONS invalid_parameters.
  ENDCLASS.

CLASS cl_asset_base IMPLEMENTATION.
METHOD constructor.
mv_bukrs = iv_bukrs.
mv_anln1 = iv_anln1.

```
TRY.
    validate_input_parameters( ).
    load_asset_data( ).
  CATCH cx_sy_exception.
    MESSAGE e001(zasset) WITH 'Asset initialization failed'.
ENDTRY.
```

ENDMETHOD.

METHOD if_asset~get_asset_data.
IF ms_asset_data IS INITIAL.
load_asset_data( ).
ENDIF.
rs_asset = ms_asset_data.
ENDMETHOD.

METHOD if_asset~validate_asset.
rv_valid = abap_true.

```
" Basic validation
IF is_asset-bukrs IS INITIAL OR
   is_asset-anln1 IS INITIAL OR
   is_asset-anlkl IS INITIAL.
  rv_valid = abap_false.
  RETURN.
ENDIF.

" Check if company code exists
SELECT SINGLE bukrs FROM t001
  INTO @DATA(lv_bukrs)
  WHERE bukrs = @is_asset-bukrs.

IF sy-subrc <> 0.
  rv_valid = abap_false.
  RETURN.
ENDIF.

" Check if asset class exists
SELECT SINGLE anlkl FROM t093c
  INTO @DATA(lv_anlkl)
  WHERE anlkl = @is_asset-anlkl.

IF sy-subrc <> 0.
  rv_valid = abap_false.
ENDIF.
```

ENDMETHOD.

METHOD load_asset_data.
SELECT SINGLE bukrs, anln1, anln2, txt50, anlkl, werks
FROM anla
INTO CORRESPONDING FIELDS OF @ms_asset_data
WHERE bukrs = @mv_bukrs
AND anln1 = @mv_anln1.

```
IF sy-subrc <> 0.
  RAISE asset_not_found.
ENDIF.
```

ENDMETHOD.

METHOD get_asset_class_config.
SELECT SINGLE *
FROM t093c
INTO @rs_config
WHERE anlkl = @iv_anlkl.
ENDMETHOD.

METHOD validate_input_parameters.
IF mv_bukrs IS INITIAL OR mv_anln1 IS INITIAL.
RAISE invalid_parameters.
ENDIF.
ENDMETHOD.
ENDCLASS.

*–––––––––––––––––––––––––––––––––––*

- Tangible Asset Class (Tárgyi Eszközök)
  *–––––––––––––––––––––––––––––––––––*
  CLASS cl_tangible_asset DEFINITION INHERITING FROM cl_asset_base.
  PUBLIC SECTION.
  INTERFACES: if_depreciation_calculator,
  if_asset_validator.
  
  METHODS: if_asset~calculate_depreciation REDEFINITION.
  
  PRIVATE SECTION.
  DATA: mv_acquisition_value TYPE p,
  mv_useful_life TYPE i,
  mv_salvage_value TYPE p.
  
  METHODS: get_acquisition_value
  RETURNING VALUE(rv_value) TYPE p,
  
  ```
         get_useful_life
           RETURNING VALUE(rv_life) TYPE i.
  ```

ENDCLASS.

CLASS cl_tangible_asset IMPLEMENTATION.
METHOD if_asset~calculate_depreciation.
DATA: ls_depreciation TYPE if_asset=>ty_depreciation,
lv_annual_depr TYPE p.

```
" Get asset values
mv_acquisition_value = get_acquisition_value( ).
mv_useful_life = get_useful_life( ).

" Calculate linear depreciation
lv_annual_depr = if_depreciation_calculator~calculate_linear(
  iv_acquisition_value = mv_acquisition_value
  iv_useful_life = mv_useful_life
  iv_salvage_value = mv_salvage_value
).

" Prepare result
ls_depreciation-gjahr = iv_gjahr.
ls_depreciation-afabe = '01'.  " Book depreciation
ls_depreciation-nafap = lv_annual_depr.
ls_depreciation-answt = mv_acquisition_value - lv_annual_depr.

APPEND ls_depreciation TO rt_depreciation.
```

ENDMETHOD.

METHOD if_depreciation_calculator~calculate_linear.
IF iv_useful_life > 0.
rv_annual_depreciation = ( iv_acquisition_value - iv_salvage_value ) / iv_useful_life.
ELSE.
RAISE calculation_error.
ENDIF.
ENDMETHOD.

METHOD if_depreciation_calculator~calculate_declining_balance.
rv_depreciation = iv_book_value * ( iv_rate / 100 ).
ENDMETHOD.

METHOD if_depreciation_calculator~get_depreciation_method.
DATA: ls_config TYPE t093c.

```
ls_config = get_asset_class_config( iv_asset_class ).

CASE ls_config-afasl.
  WHEN '0001'.
    rv_method = 'LINEAR'.
  WHEN '0002'.
    rv_method = 'DECLINING_BALANCE'.
  WHEN '0003'.
    rv_method = 'SUM_OF_YEARS'.
  WHEN OTHERS.
    rv_method = 'UNKNOWN'.
ENDCASE.
```

ENDMETHOD.

METHOD if_asset_validator~validate_company_code.
SELECT SINGLE bukrs FROM t001
INTO @DATA(lv_bukrs)
WHERE bukrs = @iv_bukrs.

```
rv_valid = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
```

ENDMETHOD.

METHOD if_asset_validator~validate_asset_class.
SELECT SINGLE anlkl FROM t093c
INTO @DATA(lv_anlkl)
WHERE anlkl = @iv_anlkl
AND ktogr = ‘1000’.  “ Tangible assets

```
rv_valid = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
```

ENDMETHOD.

METHOD if_asset_validator~validate_useful_life.
DATA: ls_config TYPE t093c.

```
ls_config = get_asset_class_config( iv_anlkl ).

" Check if useful life is within allowed range
IF iv_ndjar BETWEEN ls_config-ndmin AND ls_config-ndmax.
  rv_valid = abap_true.
ELSE.
  rv_valid = abap_false.
ENDIF.
```

ENDMETHOD.

METHOD get_acquisition_value.
SELECT SINGLE answt FROM anlc
INTO @rv_value
WHERE bukrs = @mv_bukrs
AND anln1 = @mv_anln1
AND afabe = ‘01’
AND gjahr = @sy-datum(4).
ENDMETHOD.

METHOD get_useful_life.
SELECT SINGLE ndjar FROM anla
INTO @rv_life
WHERE bukrs = @mv_bukrs
AND anln1 = @mv_anln1.
ENDMETHOD.
ENDCLASS.

*–––––––––––––––––––––––––––––––––––*

- Intangible Asset Class (Immateriális Javak)
  *–––––––––––––––––––––––––––––––––––*
  CLASS cl_intangible_asset DEFINITION INHERITING FROM cl_asset_base.
  PUBLIC SECTION.
  INTERFACES: if_depreciation_calculator.
  
  METHODS: if_asset~calculate_depreciation REDEFINITION,
  
  ```
         check_impairment
           RETURNING VALUE(rv_impairment_needed) TYPE abap_bool.
  ```
  
  PRIVATE SECTION.
  METHODS: calculate_amortization
  IMPORTING iv_book_value TYPE p
  iv_remaining_life TYPE i
  RETURNING VALUE(rv_amortization) TYPE p.
  ENDCLASS.

CLASS cl_intangible_asset IMPLEMENTATION.
METHOD if_asset~calculate_depreciation.
DATA: ls_depreciation TYPE if_asset=>ty_depreciation,
lv_book_value TYPE p,
lv_remaining_life TYPE i,
lv_amortization TYPE p.

```
" Get current book value
SELECT SINGLE answt FROM anlc
  INTO @lv_book_value
  WHERE bukrs = @mv_bukrs
    AND anln1 = @mv_anln1
    AND afabe = '01'
    AND gjahr = @iv_gjahr.

" Calculate remaining useful life
SELECT SINGLE ndjar FROM anla
  INTO @DATA(lv_total_life)
  WHERE bukrs = @mv_bukrs
    AND anln1 = @mv_anln1.

" Simple calculation for remaining life
lv_remaining_life = lv_total_life - ( sy-datum(4) - sy-datum(4) ). " Simplified

" Calculate amortization
lv_amortization = calculate_amortization(
  iv_book_value = lv_book_value
  iv_remaining_life = lv_remaining_life
).

" Check for impairment
IF check_impairment( ) = abap_true.
  lv_amortization = lv_amortization * '1.2'. " Additional impairment
ENDIF.

" Prepare result
ls_depreciation-gjahr = iv_gjahr.
ls_depreciation-afabe = '01'.
ls_depreciation-nafap = lv_amortization.
ls_depreciation-answt = lv_book_value - lv_amortization.

APPEND ls_depreciation TO rt_depreciation.
```

ENDMETHOD.

METHOD if_depreciation_calculator~calculate_linear.
rv_annual_depreciation = iv_acquisition_value / iv_useful_life.
ENDMETHOD.

METHOD if_depreciation_calculator~calculate_declining_balance.
“ Not applicable for intangible assets
rv_depreciation = 0.
ENDMETHOD.

METHOD if_depreciation_calculator~get_depreciation_method.
rv_method = ‘STRAIGHT_LINE_AMORTIZATION’.
ENDMETHOD.

METHOD check_impairment.
“ Simplified impairment test
“ In reality, this would involve complex fair value calculations
DATA: lv_market_value TYPE p,
lv_book_value TYPE p.

```
" Get book value
SELECT SINGLE answt FROM anlc
  INTO @lv_book_value
  WHERE bukrs = @mv_bukrs
    AND anln1 = @mv_anln1
    AND afabe = '01'
    AND gjahr = @sy-datum(4).

" Simulate market value (in real scenario, this would be external data)
lv_market_value = lv_book_value * '0.8'. " Assume 20% decline

IF lv_market_value < lv_book_value.
  rv_impairment_needed = abap_true.
ELSE.
  rv_impairment_needed = abap_false.
ENDIF.
```

ENDMETHOD.

METHOD calculate_amortization.
IF iv_remaining_life > 0.
rv_amortization = iv_book_value / iv_remaining_life.
ELSE.
rv_amortization = iv_book_value. “ Full amortization
ENDIF.
ENDMETHOD.
ENDCLASS.

*–––––––––––––––––––––––––––––––––––*

- Asset Posting Class
  *–––––––––––––––––––––––––––––––––––*
  CLASS cl_asset_posting DEFINITION.
  PUBLIC SECTION.
  INTERFACES: if_asset_posting.
  
  METHODS: constructor.
  
  PRIVATE SECTION.
  DATA: mo_bapi TYPE REF TO object.
  
  METHODS: prepare_bapi_data
  IMPORTING is_posting TYPE if_asset_posting=>ty_posting_data
  EXPORTING es_header TYPE bapiache09
  et_items TYPE bapiacgl09_tab,
  
  ```
         call_bapi_commit
           RETURNING VALUE(rv_success) TYPE abap_bool.
  ```

ENDCLASS.

CLASS cl_asset_posting IMPLEMENTATION.
METHOD constructor.
“ Initialize BAPI object if needed
ENDMETHOD.

METHOD if_asset_posting~post_acquisition.
DATA: ls_header TYPE bapiache09,
lt_items TYPE bapiacgl09_tab,
lt_return TYPE bapiret2_tab.

```
" Prepare BAPI data
prepare_bapi_data(
  EXPORTING is_posting = is_posting
  IMPORTING es_header = ls_header
            et_items = lt_items
).

" Call BAPI for asset acquisition posting
CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
  EXPORTING
    documentheader = ls_header
  TABLES
    accountgl      = lt_items
    return         = lt_return.

" Check for errors
READ TABLE lt_return TRANSPORTING NO FIELDS
  WITH KEY type = 'E'.

IF sy-subrc = 0.
  RAISE posting_error.
ELSE.
  " Commit transaction
  IF call_bapi_commit( ) = abap_true.
    rv_document_number = ls_header-doc_no.
  ELSE.
    RAISE posting_error.
  ENDIF.
ENDIF.
```

ENDMETHOD.

METHOD if_asset_posting~post_depreciation.
“ Similar implementation as post_acquisition
“ but with different account determination
rv_document_number = ‘1234567890’. “ Simplified
ENDMETHOD.

METHOD if_asset_posting~reverse_posting.
DATA: lt_return TYPE bapiret2_tab.

```
" Call BAPI for document reversal
CALL FUNCTION 'BAPI_ACC_DOCUMENT_REV_POST'
  EXPORTING
    obj_type = 'BKPFF'
    obj_key  = |{ iv_bukrs }{ iv_belnr }{ iv_gjahr }|
    obj_sys  = sy-sysid
  TABLES
    return   = lt_return.

" Check results
READ TABLE lt_return TRANSPORTING NO FIELDS
  WITH KEY type = 'E'.

IF sy-subrc = 0.
  rv_success = abap_false.
ELSE.
  rv_success = call_bapi_commit( ).
ENDIF.
```

ENDMETHOD.

METHOD prepare_bapi_data.
“ Prepare document header
es_header-doc_date = is_posting-bldat.
es_header-pstng_date = is_posting-bzdat.
es_header-comp_code = is_posting-bukrs.
es_header-doc_type = ‘AA’.  “ Asset document
es_header-header_txt = is_posting-bktxt.

```
" Prepare line items
DATA: ls_item TYPE bapiacgl09.

" Asset line
ls_item-itemno_acc = '001'.
ls_item-gl_account = '1500000'.  " Asset account
ls_item-asset_no = is_posting-anln1.
ls_item-sub_number = is_posting-anln2.
ls_item-debit_crdt = 'H'.
ls_item-currency = 'EUR'.
ls_item-amount = is_posting-anbtr.
APPEND ls_item TO et_items.

" Clearing account line
CLEAR ls_item.
ls_item-itemno_acc = '002'.
ls_item-gl_account = '1600000'.  " Clearing account
ls_item-debit_crdt = 'S'.
ls_item-currency = 'EUR'.
ls_item-amount = is_posting-anbtr.
APPEND ls_item TO et_items.
```

ENDMETHOD.

METHOD call_bapi_commit.
CALL FUNCTION ‘BAPI_TRANSACTION_COMMIT’
EXPORTING
wait = ‘X’.

```
rv_success = abap_true.
```

ENDMETHOD.
ENDCLASS.

*–––––––––––––––––––––––––––––––––––*

- Asset Factory Class (Factory Pattern)
  *–––––––––––––––––––––––––––––––––––*
  CLASS cl_asset_factory DEFINITION.
  PUBLIC SECTION.
  CLASS-METHODS: create_asset
  IMPORTING iv_bukrs TYPE bukrs
  iv_anln1 TYPE anln1
  iv_asset_type TYPE string
  RETURNING VALUE(ro_asset) TYPE REF TO cl_asset_base
  EXCEPTIONS invalid_asset_type.
  
  PRIVATE SECTION.
  CLASS-METHODS: determine_asset_type
  IMPORTING iv_bukrs TYPE bukrs
  iv_anln1 TYPE anln1
  RETURNING VALUE(rv_type) TYPE string.
  ENDCLASS.

CLASS cl_asset_factory IMPLEMENTATION.
METHOD create_asset.
DATA: lv_type TYPE string.

```
" Determine asset type if not provided
IF iv_asset_type IS INITIAL.
  lv_type = determine_asset_type(
    iv_bukrs = iv_bukrs
    iv_anln1 = iv_anln1
  ).
ELSE.
  lv_type = iv_asset_type.
ENDIF.

" Create appropriate asset object
CASE lv_type.
  WHEN 'TANGIBLE'.
    CREATE OBJECT ro_asset TYPE cl_tangible_asset
      EXPORTING
        iv_bukrs = iv_bukrs
        iv_anln1 = iv_anln1.

  WHEN 'INTANGIBLE'.
    CREATE OBJECT ro_asset TYPE cl_intangible_asset
      EXPORTING
        iv_bukrs = iv_bukrs
        iv_anln1 = iv_anln1.

  WHEN OTHERS.
    RAISE invalid_asset_type.
ENDCASE.
```

ENDMETHOD.

METHOD determine_asset_type.
DATA: lv_ktogr TYPE ktogr.

```
" Get asset class group
SELECT SINGLE t093c~ktogr
  FROM anla
  INNER JOIN t093c ON anla~anlkl = t093c~anlkl
  INTO @lv_ktogr
  WHERE anla~bukrs = @iv_bukrs
    AND anla~anln1 = @iv_anln1.

CASE lv_ktogr.
  WHEN '1000'.
    rv_type = 'TANGIBLE'.
  WHEN '2000'.
    rv_type = 'INTANGIBLE'.
  WHEN OTHERS.
    rv_type = 'TANGIBLE'. " Default
ENDCASE.
```

ENDMETHOD.
ENDCLASS.

*–––––––––––––––––––––––––––––––––––*

- Main Program - Usage Example
  *–––––––––––––––––––––––––––––––––––*
  START-OF-SELECTION.
  DATA: lo_asset TYPE REF TO cl_asset_base,
  lo_posting TYPE REF TO cl_asset_posting,
  ls_asset_data TYPE if_asset=>ty_asset_data,
  lt_depreciation TYPE if_asset=>tt_depreciation,
  ls_posting TYPE if_asset_posting=>ty_posting_data.
  
  TRY.
  “ Create asset object using factory
  lo_asset = cl_asset_factory=>create_asset(
  iv_bukrs = ‘1000’
  iv_anln1 = ‘0000100001’
  iv_asset_type = ‘TANGIBLE’
  ).
  
  ```
  " Get asset data
  ls_asset_data = lo_asset->if_asset~get_asset_data( ).
  
  WRITE: / 'Asset Number:', ls_asset_data-anln1,
         / 'Description:', ls_asset_data-txt50,
         / 'Asset Class:', ls_asset_data-anlkl,
         / 'Plant:', ls_asset_data-werks.
  
  " Calculate depreciation
  lt_depreciation = lo_asset->if_asset~calculate_depreciation(
    iv_bukrs = '1000'
    iv_anln1 = '0000100001'
    iv_gjahr = sy-datum(4)
  ).
  
  LOOP AT lt_depreciation INTO DATA(ls_depreciation).
    WRITE: / 'Year:', ls_depreciation-gjahr,
           / 'Area:', ls_depreciation-afabe,
           / 'Depreciation:', ls_depreciation-nafap,
           / 'Book Value:', ls_depreciation-answt.
  ENDLOOP.
  
  " Create posting object and post acquisition
  CREATE OBJECT lo_posting TYPE cl_asset_posting.
  
  ls_posting-bukrs = '1000'.
  ls_posting-anln1 = '0000100001'.
  ls_posting-anln2 = '0'.
  ls_posting-afabe = '01'.
  ls_posting-anbtr = 50000.
  ls_posting-bzdat = sy-datum.
  ls_posting-bldat = sy-datum.
  ls_posting-bktxt = 'Asset Acquisition'.
  
  DATA(lv_document) = lo_posting->if_asset_posting~post_acquisition( ls_posting ).
  WRITE: / 'Posted Document:', lv_document.
  ```
  
  CATCH cx_sy_exception INTO DATA(lx_error).
  WRITE: / ‘Error:’, lx_error->get_text( ).
  ENDTRY.
