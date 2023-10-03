 DATA:zbukr  TYPE char30,
       zhbki  TYPE char30,
       zlaufd TYPE char30,
       zlaufi TYPE char30.

  IMPORT zbukr zhbki zlaufd zlaufi FROM MEMORY ID 'ZB'.


  TYPES: BEGIN OF st_accerror,
      flag type char1,
      text type char100,
      end of st_accerror.

      data: lt_accerror type table of st_accerror,
            ls_accerror type st_accerror.

DATA: lv_error1 type STRING,
      lv_error2 type char100,
      lv_error3 type char100,
      lv_error4 type char100.

CLEAR: ls_accerror, lv_error1, lv_error2, lv_error3, lv_error4.
refresh: lt_accerror.

TYPES: BEGIN OF zbnk_banco ,
    regis(1)       ,     "Tipo de Registro
    idcom(15),               "RNC de la empresa
    nomcom(35),              "Nombre de la Compañía
    sucu(7),                 "Secuencia asignada al archivo, no debe reperti
    tipser(2),               "Pago a Suplidores, valor fijo
    fechaefec(8)   TYPE n,    "fecha de pago YYYYMMDD,
    cntddeb(11),             "Cantidad de débitos. Solo aplica
    totam(13),               "Monto total débitos. Solo aplica completarse para los servicios 03 y 06
    cancred(11),             "Cantidad de créditos a realizar. Debe coincidir con la cantidad de beneficiario en los registros "N"
    montcred(13),            "Monto total Créditos. Debe coincidir con sumatoria de los montos en los registros "N"
    nummid(15),              "Afiliación a Azul o Cardnet solo si el tipo de servicio = 03, de lo contrario completar con ceros.
    fechaenvi(8)   TYPE n,    "YYYYMMDD de envío del archivo
    hora(4),                 "HHMM de envío del archivo (HORA DEL SISTEMA  CUANDO SE GENERA EL ARCHIVCO.
    correo(40),              "Información general del contacto
    estatus(1),              "Dejar en blanco
    fler(136),               "dejar en blanco
    tiporeg(1),              "valor fijo = N
    idcompania(15),          "RNC de la empresa.
    sucu2(7),                "Secuencia asignada al archivo, no debe repetirse
    secutran(7),             "Número que identifica la transacción. No debe repetirse por transacción en un mismo archivo. Inicia en 1 y se incrementa por cada registro encontrado para pago
    cuentades(20),           "Cuenta del proveedor alinear a la izquierda y llenar de espacios a la derecha
    tipcuenta(1),            "1 = Cuenta Corriente 2 = Cuenta de Ahorros
    moneda(3),               "moneda del pago a realizar
    codiban(8),              "banco donde tiene la cuenta el beneficiario del pago
    digvenban(1),            "Dijito de verificacion del banco
    codiope(2),              "Código de la operación de acuerdo al tipo de cuenta bancaria
    montransa(13),           "Valor del pago al proveedorNo utilizar ni comasni puntos, Toar el valor a pagar y multiplicar por 100 y solo tomar la parte entera (este valor es que se utiliza para sumar el total de créditos de la cabecera).
    tipidentifi(2),          "Tipo de identificación del tercero
    numeidentf(15),          "CHAR  Número de identificación del terceroLlenar con espacios el resto de este campo a la derecha
    nombenef(35),            "con espacios a la derecha para completar su tamaño
    numrefer(12),            "Usado para enviar en el estado de cuenta del beneficiario por parte del banco
    descri(40),               "Descripción del pago, se muestra en el estado de cuenta del proveedor
    fechavenci(4),            "solo para cobros y tarjetas de crédito dejar en blanco
    formconta(1),             "Para notificar al contacto establecido: Dejar en esta campo = "1"
    emailbene(40),            "Correo electrónico del proveedor, llenar con espacios a la derecha
    fax(12),                  "justificado a la derecha, si la forma de contacto es 2 o 3
    prospago(2),              "Proceso de crédito o débito  = 00
    numautor(15) ,            "dejar en blanco
    codretoremo(3),           "dejar en blanco
    codirazret(3),            "dejar en blanco
    codrazinte(3),            "dejar en blanco
    prostrans(1),             "dejar en blanco
    estatran(1),              "dejar en blanco
    filler(136),               "dejar en blanc

  END OF zbnk_banco.

DATA   banpro TYPE string.

TYPES: ty_line_arch TYPE string. "temp

DATA: t_estructura TYPE TABLE OF ty_line_arch,
      w_estructura TYPE          ty_line_arch,
      t_estrc_prv  TYPE TABLE OF zbnk_banco,
      w_estrc_prv  TYPE          zbnk_banco.


TYPES: BEGIN OF zbnk_banco2 ,
    tiporeg(1),              "valor fijo = N
    idcompania(15),          "RNC de la empresa.
    sucu2(7),                "Secuencia asignada al archivo, no debe repetirse
    secutran(7),             "Número que identifica la transacción. No debe repetirse por transacción en un mismo archivo. Inicia en 1 y se incrementa por cada registro encontrado para pago
    cuentades(20),           "Cuenta del proveedor alinear a la izquierda y llenar de espacios a la derecha
    tipcuenta(1),            "1 = Cuenta Corriente 2 = Cuenta de Ahorros
    moneda(3),               "moneda del pago a realizar
    codiban(8),              "banco donde tiene la cuenta el beneficiario del pago
    digvenban(1),            "Dijito de verificacion del banco
    codiope(2),              "Código de la operación de acuerdo al tipo de cuenta bancaria
    montransa(13),           "Valor del pago al proveedorNo utilizar ni comasni puntos, Toar el valor a pagar y multiplicar por 100 y solo tomar la parte entera (este valor es que se utiliza para sumar el total de créditos de la cabecera).
    tipidentifi(2),          "Tipo de identificación del tercero
    numeidentf(15),          "CHAR  Número de identificación del terceroLlenar con espacios el resto de este campo a la derecha
    nombenef(35),            "con espacios a la derecha para completar su tamaño
    numrefer(12),            "Usado para enviar en el estado de cuenta del beneficiario por parte del banco
    descri(40),               "Descripción del pago, se muestra en el estado de cuenta del proveedor
    fechavenci(4),            "solo para cobros y tarjetas de crédito dejar en blanco
    formconta(1),             "Para notificar al contacto establecido: Dejar en esta campo = "1"
    emailbene(40),            "Correo electrónico del proveedor, llenar con espacios a la derecha
    fax(12),                  "justificado a la derecha, si la forma de contacto es 2 o 3
    prospago(2),              "Proceso de crédito o débito  = 00
    numautor(15) ,            "dejar en blanco
    codretoremo(3),           "dejar en blanco
    codirazret(3),            "dejar en blanco
    codrazinte(3),            "dejar en blanco
    prostrans(1),             "dejar en blanco
    estatran(1),              "dejar en blanco
    filler(52),               "dejar en blanco

  END OF zbnk_banco2.


*
TYPES: ty_line_arch2 TYPE string. "temp
DATA   banpro2 TYPE string.

DATA: t_estructura2 TYPE TABLE OF ty_line_arch2,
      w_estructura2 TYPE          ty_line_arch2,
      t_estrc_prv2  TYPE TABLE OF  zbnk_banco2,
      w_estrc_prv2  TYPE          zbnk_banco2.



DATA: ti_tvarvc TYPE TABLE  OF tvarvc,
      wa_tvarvc LIKE LINE OF ti_tvarvc.
*/ESTRUCTURA DEL TXT.

DATA: legal      TYPE bu_legenty,
      zlifnr     TYPE lifnr,
      name       TYPE name1_gp,
      zvblnr     TYPE vblnr,
      zaddrnum   TYPE ad_addrnum,
      sumato(13)   ,
      zbanco_pag TYPE  rvari_vnam,
      zbanco_pag2 TYPE  rvari_vnam,
      zbanco_pag3 TYPE  rvari_vnam,
      ID_COM TYPE RVARI_VAL_255,
      descrp  type char50,
      ZADDRNUMBER  TYPE  ADR6-ADDRNUMBER ,

      zlow(150) ,
      ruta       TYPE rvari_val_255,
      lv_secuencia_reguh TYPE sy-tabix.

"PARA EL SALTO DE LINEA

DATA:c_cr(1) TYPE c VALUE cl_abap_char_utilities=>cr_lf.
DATA:c_nw(1) TYPE c VALUE cl_abap_char_utilities=>newline.

TYPES:
  BEGIN OF ty_reguh,
    zbkon LIKE reguh-zbkon,
    waers LIKE reguh-waers,
    zbnky LIKE reguh-zbnky,
    ZBNKN  LIKE reguh-zbnkn,
    rbetr LIKE reguh-rbetr,
    rwbtr LIKE reguh-rwbtr,
    lifnr LIKE reguh-lifnr,
    name1 LIKE reguh-name1,
    vblnr LIKE reguh-vblnr,
    laufi LIKE reguh-laufi,
    laufd LIKE reguh-laufd,
    HBKID LIKE reguh-HBKID,
    HKTID LIKE reguh-HKTID,
  END OF  ty_reguh.

DATA: ti_reguh    TYPE TABLE OF ty_reguh.
DATA: wa_reguh1    TYPE ty_reguh.  "PARA LOS MONTOS
DATA: wa_reguh    TYPE ty_reguh.

DATA: lv_HBKID type HBKID,
      lv_HKTID type HKTID.
DATA: lt_ZTR0002 type table of ZTR0002,
      ls_ZTR0002 type ZTR0002.



DATA: lt_ZTR00022 type table of ZTR0002,
      ls_ZTR00022 type ZTR0002.

"Limpieza de estructuras y tablas
REFRESH: t_estructura, t_estructura2, t_estrc_prv, t_estrc_prv2.
CLEAR: w_estructura, w_estructura2, w_estrc_prv, w_estrc_prv2,  sumato .


*******************************Cabecera
"Tipo-registro
w_estrc_prv-regis =  'H'.

"ID Compañía
SELECT SINGLE  paval
  INTO w_estrc_prv-idcom
 FROM  t001z
  WHERE bukrs  EQ BUKTAB-BUKRS
  AND party = 'CGIID'.

SHIFT w_estrc_prv-idcom LEFT DELETING LEADING space.


"Nombre Compañía
SELECT SINGLE  butxt
  INTO w_estrc_prv-nomcom
  FROM t001
  WHERE bukrs  EQ BUKTAB-BUKRS.

SHIFT  w_estrc_prv-nomcom LEFT DELETING LEADING space.

CONCATENATE 'ZID_' zhbki INTO zbanco_pag2.
SELECT SINGLE LOW
  INTO ID_COM  "Secuencia
  FROM   tvarvc
  WHERE type = 'P' AND
  name =  zbanco_pag2.


***Ruta para  descargar el archivo
CONCATENATE 'ZRUTA_' zhbki INTO zbanco_pag3.

SELECT SINGLE * INTO wa_tvarvc FROM tvarvc
  WHERE type EQ 'P'
    AND name EQ zbanco_pag3.

ruta =  wa_tvarvc-low.

CONCATENATE 'ZPAGO_' zhbki INTO zbanco_pag.

  CLEAR  wa_tvarvc.
"Secuencia / Incremetar Correlativo
SELECT SINGLE * INTO wa_tvarvc FROM tvarvc
  WHERE type EQ 'P'
    AND name EQ zbanco_pag.

IF wa_tvarvc-numb IS INITIAL.

  wa_tvarvc-numb = '1'.

ELSEIF wa_tvarvc-numb  IS NOT INITIAL.

  zlow  = wa_tvarvc-numb .
  wa_tvarvc-numb =  zlow  + 1 .
  CONDENSE wa_tvarvc-numb.

ENDIF.

w_estrc_prv-sucu = wa_tvarvc-numb.
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
  INPUT = w_estrc_prv-sucu
 IMPORTING
  OUTPUT = w_estrc_prv-sucu.




"Tipo Servicio

w_estrc_prv-tipser = '02'.

"  Fecha Efectiva

w_estrc_prv-fechaefec = zlaufd  .



  "Cantidad Débitos

  w_estrc_prv-cntddeb = '00000000000'.

  "Monto Total débitos

  w_estrc_prv-totam = '0000000000000'.


"Cantidad Créditos

SELECT   zbkon  waers  zbnky  rbetr rwbtr lifnr  name1  vblnr  laufi  laufd
  INTO CORRESPONDING FIELDS OF TABLE  ti_reguh
  FROM reguh
  WHERE  laufd = zlaufd  AND
  laufi = zlaufi  AND
  xvorl =  ' ' AND
  zbukr  = zbukr.

DESCRIBE TABLE ti_reguh LINES sy-tfill.
w_estrc_prv-cancred  = sy-tfill.


CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
  INPUT = w_estrc_prv-cancred
 IMPORTING
  OUTPUT = w_estrc_prv-cancred.

"Monto Total Créditos

LOOP AT  ti_reguh INTO wa_reguh1 .

  sumato =  sumato + WA_REGUH1-RWBTR * 100.

ENDLOOP.

REPLACE ALL OCCURRENCES OF '-' IN sumato WITH ''.
REPLACE ALL OCCURRENCES OF '.' IN sumato WITH ''.

w_estrc_prv-montcred = sumato.
  CONDENSE w_estrc_prv-montcred.

*  * Función que completa con ceros a la izquierda de una variable
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
  INPUT = w_estrc_prv-montcred
 IMPORTING
  OUTPUT = w_estrc_prv-montcred.
"Número de MID o afiliación.

w_estrc_prv-nummid = '000000000000000'.

"Fecha de Envío

w_estrc_prv-fechaenvi = zlaufd.

"Hora

w_estrc_prv-hora  = SY-TIMLO.

"E-mail

SELECT SINGLE low
INTO w_estrc_prv-correo
 FROM tvarvc
 WHERE name = 'ZUSR_MAIL' AND
  type = 'P' .


SHIFT w_estrc_prv-correo LEFT DELETING LEADING space.
"Estatus

w_estrc_prv-estatus = ' '.   "Estatus

SHIFT w_estrc_prv-estatus  LEFT DELETING LEADING space.
*Filler

SHIFT w_estrc_prv-fler  LEFT DELETING LEADING space.


*** Concatenar Cabecera ***
CONCATENATE w_estrc_prv-regis
            w_estrc_prv-idcom
            w_estrc_prv-nomcom
            w_estrc_prv-sucu
            w_estrc_prv-tipser
            w_estrc_prv-fechaefec
            w_estrc_prv-cntddeb
            w_estrc_prv-totam
            w_estrc_prv-cancred
            w_estrc_prv-montcred
            w_estrc_prv-nummid
            w_estrc_prv-fechaenvi
            w_estrc_prv-hora
            w_estrc_prv-correo
            w_estrc_prv-estatus
            w_estrc_prv-fler
            INTO w_estructura RESPECTING BLANKS.

APPEND w_estructura TO t_estructura    .
clear: w_estructura .
*******FIN DETALLE DE LA CABECERA



SELECT  zbkon  waers  zbnky ZBNKN rbetr rwbtr lifnr  name1  vblnr  laufi  laufd HBKID HKTID
  INTO CORRESPONDING FIELDS OF TABLE  ti_reguh
  FROM reguh
  WHERE  laufd = zlaufd  AND
  laufi = zlaufi  AND
  xvorl =  ' ' AND
  zbukr  = zbukr.


DATA: lt_regup TYPE TABLE OF regup,
      ls_regup TYPE regup.
CLEAR: ls_regup.

SELECT * INTO TABLE lt_regup FROM regup
  FOR ALL ENTRIES IN   ti_reguh
  WHERE laufd EQ  ti_reguh-laufd
    AND laufi EQ  ti_reguh-laufi
    AND xvorl EQ ' '
    AND vblnr EQ  ti_reguh-vblnr.


LOOP AT ti_reguh INTO wa_reguh.
  lv_HBKID = wa_reguh-HBKID.
  lv_HKTID = wa_reguh-HKTID.

  lv_secuencia_reguh = sy-tabix.

w_estrc_prv2-tiporeg  = 'N'. " TIPO DE REGISTRO

w_estrc_prv2-idcompania  = w_estrc_prv-idcom.  " IDE DE COMPALIA

w_estrc_prv2-idcompania  = w_estrc_prv-idcom.  " IDE DE COMPALIA
w_estrc_prv2-sucu2       = w_estrc_prv-sucu.   "SWCUENCIA


CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
  INPUT = w_estrc_prv2-sucu2
 IMPORTING
  OUTPUT = w_estrc_prv2-sucu2.


w_estrc_prv2-secutran    = lv_secuencia_reguh.  "Secuencia de la transacción
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input  = w_estrc_prv2-secutran
  IMPORTING
    output = w_estrc_prv2-secutran.


zlifnr =  wa_reguh-lifnr .

* Cuenta Destino
IF wa_reguh-zbnkn is not INITIAL.
  w_estrc_prv2-cuentades = wa_reguh-zbnkn.
ELSE.
  ls_accerror-flag = 'X'.
  CONCATENATE 'Error, Proveedor' wa_reguh-name1 'NO tiene cuenta bancaria' INTO ls_accerror-text SEPARATED BY space.
  APPEND ls_accerror to lt_accerror.
*

ENDIF.


SHIFT w_estrc_prv2-cuentades LEFT DELETING LEADING space.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
 EXPORTING
  INPUT = wa_reguh-zbkon
 IMPORTING
  OUTPUT = wa_reguh-zbkon.

*Tipo de Cuenta Destino
w_estrc_prv2-tipcuenta = wa_reguh-zbkon.


"Moneda Destino
CASE wa_reguh-waers.
  WHEN 'DOP'.
    w_estrc_prv2-moneda = '214'.
  WHEN 'USD'.
    w_estrc_prv2-moneda = '840'.
  WHEN 'EUR'.
    w_estrc_prv2-moneda = '978'.
ENDCASE.


"Código Banco Destino
IF wa_reguh-waers EQ 'USD'.
  CONCATENATE '8' wa_reguh-zbnky+1(7) INTO w_estrc_prv2-codiban.
ELSE.
  w_estrc_prv2-codiban = wa_reguh-zbnky(8).
ENDIF.

"Digito de verificación Banco destino
IF wa_reguh-waers EQ 'USD'.
  w_estrc_prv2-digvenban = 'L'.
ELSE.
  w_estrc_prv2-digvenban =  wa_reguh-zbnky+8(1).
ENDIF.

"Código de la operación
CASE  wa_reguh-zbkon.
  WHEN '1'.
    w_estrc_prv2-codiope = '22'.
  WHEN '2'.
    w_estrc_prv2-codiope = '32'.
  WHEN OTHERS.
ENDCASE.

"Monto de la transacción
w_estrc_prv2-montransa =  wa_reguh-rwbtr  * 100. "reguh-rbetr * 100



REPLACE ALL OCCURRENCES OF '-' IN w_estrc_prv2-montransa  WITH ''.
REPLACE ALL OCCURRENCES OF '.' IN w_estrc_prv2-montransa  WITH ''.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input  = w_estrc_prv2-montransa
  IMPORTING
    output = w_estrc_prv2-montransa.



"Tipo de identificación
SELECT SINGLE legal_enty INTO  w_estrc_prv2-tipidentifi FROM  but000
  WHERE partner EQ  wa_reguh-lifnr.

  "Número de Identificación
SELECT SINGLE zstc1 INTO w_estrc_prv2-numeidentf FROM  reguh
  WHERE lifnr EQ wa_reguh-lifnr.

  "Nombre del Beneficiario
 w_estrc_prv2-nombenef = wa_reguh-name1.

*  Numero de referencia
 w_estrc_prv2-numrefer = wa_reguh-vblnr.

SHIFT w_estrc_prv2-nombenef LEFT DELETING LEADING space.


CONCATENATE  'FI' w_estrc_prv2-numrefer INTO w_estrc_prv2-numrefer.

*** Consulta REGUP ***
LOOP AT lt_regup INTO ls_regup WHERE vblnr = wa_reguh-vblnr.

  IF ls_regup-xblnr IS NOT INITIAL.

    IF descrp IS INITIAL.
       descrp = ls_regup-xblnr.
    ELSE.
      CONCATENATE descrp Ls_regup-xblnr INTO descrp SEPARATED BY '/'.
    ENDIF.

    CONCATENATE 'REF:' descrp INTO w_estrc_prv2-descri.
    CONDENSE w_estrc_prv2-descri.
    SHIFT w_estrc_prv2-descri LEFT DELETING LEADING space.

ELSE .

  w_estrc_prv2-descri =  'REF:'.

    SHIFT w_estrc_prv2-descri LEFT DELETING LEADING space.
  ENDIF.

ENDLOOP.

CLEAR descrp.
"Fecha de Vencimiento
SHIFT w_estrc_prv2-fechavenci LEFT DELETING LEADING space.


"Forma de contacto
w_estrc_prv2-formconta = '1'.


"Email beneficiario

SELECT SINGLE ADDRNUMBER
  INTO ZADDRNUMBER
  FROM BUT020
  WHERE PARTNER  EQ wa_reguh-lifnr.

SELECT SINGLE smtp_addr INTO w_estrc_prv2-emailbene FROM adr6
  WHERE flgdefault EQ 'X' AND   ADDRNUMBER EQ   ZADDRNUMBER .

if  w_estrc_prv2-emailbene  is INITIAL .
  w_estrc_prv2-formconta = '3'.
  SELECT SINGLE TEL_NUMBER INTO w_estrc_prv2-fax FROM adrc
  WHERE  ADDRNUMBER EQ   ZADDRNUMBER .


ENDIF.
**

* << AND addrnumber EQ ( SELECT addrnumber FROM but020 WHERE partner EQ  wa_reguh-lifnr ).
**

  "Proceso de Pago
w_estrc_prv2-prospago = '00'.

"Número de autorización
*SHIFT w_estrc_prv2-numautor LEFT DELETING LEADING space.
SHIFT w_estrc_prv2-numautor RIGHT DELETING TRAILING SPACE.


"Código retorno remoto
SHIFT w_estrc_prv2-codretoremo LEFT DELETING LEADING space.

"Código razón retorno
SHIFT w_estrc_prv2-codirazret LEFT DELETING LEADING space.

"Código razón interno
SHIFT w_estrc_prv2-codrazinte LEFT DELETING LEADING space.

"Procesador Transacción
SHIFT w_estrc_prv2-prostrans LEFT DELETING LEADING space.

"Estatus transacción
SHIFT w_estrc_prv2-estatran LEFT DELETING LEADING space.

"Filler

SHIFT w_estrc_prv2-filler  LEFT DELETING LEADING space.

*** Concatenar Detalles ***
CONCATENATE w_estrc_prv2-tiporeg
            w_estrc_prv2-idcompania
            w_estrc_prv2-sucu2
            w_estrc_prv2-secutran
            w_estrc_prv2-cuentades
            w_estrc_prv2-tipcuenta
             w_estrc_prv2-moneda
            w_estrc_prv2-codiban
            w_estrc_prv2-digvenban
            w_estrc_prv2-codiope
            w_estrc_prv2-montransa
            w_estrc_prv2-tipidentifi
            w_estrc_prv2-numeidentf
            w_estrc_prv2-nombenef
            w_estrc_prv2-numrefer
            w_estrc_prv2-descri
            w_estrc_prv2-fechavenci
            w_estrc_prv2-formconta
            w_estrc_prv2-emailbene
            w_estrc_prv2-fax
            w_estrc_prv2-prospago
            w_estrc_prv2-numautor
            w_estrc_prv2-codretoremo
            w_estrc_prv2-codirazret
            w_estrc_prv2-codrazinte
            w_estrc_prv2-prostrans
            w_estrc_prv2-estatran
            w_estrc_prv2-filler
            INTO w_estructura RESPECTING BLANKS.

APPEND w_estructura TO t_estructura.
clear: w_estructura  ,  w_estrc_prv2 .

ENDLOOP.

  DATA: nombretxt2 TYPE CHAR50.


SELECT *
  into TABLE lt_ZTR00022
  FROM ZTR0002
  WHERE laufd eq zlaufd and
        laufi  eq zlaufi .

if  lt_ZTR00022 is NOT INITIAL .

MESSAGE 'Ya fue generado un archivo con esta informacion' TYPE 'S' DISPLAY LIKE 'E'.
ELSE.

IF lt_accerror[] is not INITIAL.

  MESSAGE 'Error de Proveedor en cuentas bancarias' TYPE 'S' DISPLAY LIKE 'E'.

  LOOP AT lt_accerror INTO ls_accerror.
    CASE sy-tabix.
      WHEN 1.
        lv_error1 = ls_accerror-text.
      WHEN 2.
        lv_error2 = ls_accerror-text.
      WHEN 3.
        lv_error3 = ls_accerror-text.
      WHEN 4.
        lv_error4 = ls_accerror-text.
    ENDCASE.
  ENDLOOP.

  IF lv_error1 IS NOT INITIAL OR
     lv_error2 IS NOT INITIAL OR
     lv_error3 IS NOT INITIAL OR
     lv_error4 IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel        = 'Error de cuentas bancarias'
      txt1         = lv_error1
      txt2         = lv_error2
      TXT3         = lv_error3
      TXT4         = lv_error4.

  LEAVE SCREEN.

  ENDIF.


ELSE.

  APPEND wa_tvarvc TO ti_tvarvc.

DELETE FROM tvarvc WHERE name EQ zbanco_pag AND type EQ 'P'.
COMMIT WORK AND WAIT.

MODIFY tvarvc FROM TABLE ti_tvarvc.
COMMIT WORK AND WAIT.



IF NOT t_estructura IS INITIAL.

*  DATA: filename LIKE RLGRAP-FILENAME.
  DATA: filename type string.
  DATA: nombretxt TYPE CHAR50.
  DATA: lv_secuh TYPE char7.

  LV_SECUH =  wa_tvarvc-numb.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
   INPUT = LV_SECUH
 IMPORTING
   OUTPUT = LV_SECUH.


  CONCATENATE 'PE' ID_COM  '02' SY-DATUM+4(2) SY-DATUM+6(2) LV_SECUH 'E' '.TXT' INTO nombretxt.
  CONCATENATE ruta nombretxt INTO filename.


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                  = filename
      filetype                  = 'ASC'
*     trunc_trailing_blanks     = ' '
      trunc_trailing_blanks_eol = ' '    "Deja los espacios en blanco
    TABLES
      data_tab                  = t_estructura.



"Llenado de tabla ZTR0002
ls_ztr0002-LAUFD = zlaufd.
ls_ztr0002-LAUFI = zlaufi.
ls_ztr0002-ZBUKR = ZBUKR.
ls_ztr0002-HBKID = lv_HBKID.
ls_ztr0002-HKTID = lv_HKTID.
ls_ztr0002-RUTA  = ruta.
ls_ztr0002-USNAM = sy-uname.
ls_ztr0002-FECHA = sy-datum.
ls_ztr0002-HORA  = SY-UZEIT.
ls_ztr0002-ESTADO = 'X'.
APPEND ls_ztr0002 to lt_ztr0002.
MODIFY ZTR0002 FROM TABLE lt_ztr0002.
COMMIT WORK AND WAIT.

CLEAR: ls_ztr0002.
REFRESH: lT_ztr0002.

ELSE.

  MESSAGE 'No existe datos para la Propuesta' TYPE 'I'.

"Llenado de tabla ZTR0002
ls_ztr0002-LAUFD = zlaufd.
ls_ztr0002-LAUFI = zlaufi.
ls_ztr0002-ZBUKR = ZBUKR.
ls_ztr0002-HBKID = lv_HBKID.
ls_ztr0002-HKTID = lv_HKTID.
ls_ztr0002-RUTA  = ruta.
ls_ztr0002-USNAM = sy-uname.
ls_ztr0002-FECHA = sy-datum.
ls_ztr0002-HORA  = SY-UZEIT.
ls_ztr0002-ESTADO = ' '.
APPEND ls_ztr0002 to lt_ztr0002.
MODIFY ZTR0002 FROM TABLE lt_ztr0002.
COMMIT WORK AND WAIT.

CLEAR: ls_ztr0002, t_estructura .
REFRESH: lT_ztr0002 .
ENDIF.


ENDIF.

ENDIF.

ENDif.
