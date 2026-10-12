2. A szükséges DDIC-típusok létrehozása

A fenti program három saját DDIC-típust és egy struktúrát használ. Ezeket az SE11 tranzakcióban hozd létre.

Név	Típus	Beállítás
ZPM_COMP_BSID_TT	Table Type	Standard Table, line type BSID
ZPM_COMP_BSIK_TT	Table Type	Standard Table, line type BSIK
ZPM_COMP_SUM	Structure	Az alábbi mezők
ZPM_COMP_SUM_TT	Table Type	Standard Table, line type ZPM_COMP_SUM

A ZPM_COMP_SUM struktúra mezői:

Mező	Adattípus
WAERS	WAERS
CUST_TOTAL	WRBTR
VEND_TOTAL	WRBTR
NET_TOTAL	WRBTR

A három összegmezőnél a pénznemhivatkozást a WAERS mezőre állítsd be, ahol a DDIC ezt megköveteli.

3. Dynpro 0200 létrehozása

SE80-ban vagy SE51-ben hozd létre a 0200 képernyőt.

YYPM_COMP – Kompenzáció

Vevői tételek

Custom Control: CC_CUST

SALV – BSID

Szállítói tételek

Custom Control: CC_VEND

SALV – BSIK

Kiválasztott tételek összesítése

Custom Control: CC_SUM

REFRESHPDFBACK

A három Custom Control neve pontosan ez legyen: CC_CUST, CC_VEND, CC_SUM.

A képernyő Flow Logic része

PROCESS BEFORE OUTPUT.
  MODULE status_0200.
  MODULE display_0200.
PROCESS AFTER INPUT.
  MODULE user_command_0200.

PF-STATUS létrehozása

SE41-ben hozz létre egy ZCOMP_200 GUI-statuszt, a következő funkciókódokkal:

Gomb felirata	Function code
Frissítés	REFRESH
PDF generálása	PDF
Vissza	BACK

A SET PF-STATUS 'ZCOMP_200' sor ezt a státuszt használja.

4. Smart Form létrehozása a PDF-hez

Az SMARTFORMS tranzakcióban hozd létre a YYPM_COMP_SF formot.

A Form Interface részben állítsd be a következő paramétereket:

Importing paraméterek

* IV_BUKRS – BUKRS
* IV_KUNNR – KUNNR
* IV_LIFNR – LIFNR

Tables paraméterek

* IT_CUSTOMER – ZPM_COMP_BSID_TT
* IT_VENDOR – ZPM_COMP_BSIK_TT
* IT_SUMMARY – ZPM_COMP_SUM_TT

A formban készíts három táblázatrészt a vevői tételekhez, a szállítói tételekhez és az összesítéshez. A program ezek adatait adja át a formnak, majd PDF-fé konvertálja az eredményt.

5. Fontos ellenőrzések az első aktiválás előtt

* A vevő–szállító kapcsolatot a KNA1-LIFNR, a fordított kapcsolatot az LFA1-KUNNR mezőből oldja fel a program.
* A BSID és BSIK tételeket a megadott vállalatkódra (BUKRS) szűri, és mindkét lekérdezés legfeljebb 50 sort tölt be.
* A SALV-ban több sort is ki lehet jelölni; a PDF gomb csak a kijelölt sorokat adja át a Smart Formnak.
* A pénznemenkénti összesítés a kiválasztott tételek WRBTR értékeit használja, a SHKZG előjelét figyelembe véve.
* A PDF mentése SAP GUI-s frontendkapcsolatot igényel; háttérfeldolgozásban a GUI_DOWNLOAD nem használható így.

Még egy lényeges megjegyzés: ez a forrás a riport és a PDF-folyamat alapját adja, de ECC-verziótól függően lehetnek kisebb szintaktikai vagy interfészeltérések, amelyeket aktiváláskor ellenőrizni kell. A PDF előállítása önmagában nem könyveli és nem kompenzálja a FI-tételeket; a tényleges kiegyenlítést külön, megfelelő SAP FI-folyamatban kell elvégezni.