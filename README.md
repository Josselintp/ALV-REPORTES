REPORT ZFIFEL_RD001.

INCLUDE:  ZFIFEL_RD001_top,
          ZFIFEL_RD001_01.



*&---------------------------------------------------------------------*
*&  Include           ZFIFEL_RD001_TOP
 *&   REPORTE ALV CON BOTONES 
*&---------------------------------------------------------------------*}}

TABLES:
  bkpf, bseg , lfa1, t001 , t001z , ztsd_felrd03  .

TYPES:
  BEGIN OF ty_zfifel,
     checked(1),     "3
    bukrs       TYPE  bkpf-bukrs,
    belnr       TYPE  bkpf-belnr,
    blart       TYPE  bkpf-blart,
    xblnr       TYPE  bkpf-xblnr,
    lifnr       TYPE  bseg-lifnr,
    name1       TYPE  lfa1-name1,
    butxt       TYPE  t001-butxt,
    dmbtr       TYPE  bseg-dmbtr,
    paval       TYPE  t001z-paval,
    stcd1       TYPE  lfa1-stcd1,
    zestatus(2),
  END OF  ty_zfifel.


 DATA: t_zfifel TYPE TABLE OF ty_zfifel,
       w_zfifel TYPE ty_zfifel.
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE TEXT-001.

PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY .

SELECT-OPTIONS: so_bldat FOR bkpf-bldat,
                so_belnr FOR bkpf-belnr,
                so_gjahr FOR bkpf-gjahr.
PARAMETERS:     so_esta  TYPE zestatus MATCHCODE OBJECT   ztsd_ayudasta DEFAULT '02' .
SELECTION-SCREEN END OF BLOCK bk1.


RANGES: r_anwnd      FOR  febko-anwnd.


*------------------------------------------VARIABLES_VARIANTES_ALV

TYPE-POOLS: slis.
*Tablas. Catálogo de campos
*------------------------------------------VARIABLES_VARIANTES_ALV

DATA: w_disvariant      TYPE disvariant,          "Varint information
      w_es_variant      LIKE disvariant,          "Manejo de variantes
      w_variant_exit(1) TYPE c,                   "Manejo de variantes
      w_repid           LIKE sy-repid,            "Para nombre del prog.
      ok_code           TYPE sy-ucomm.
*-----------------------------------*VARIABLES_ALV
TYPE-POOLS: slis.
*CATALOGO
DATA:    i_fieldcat      TYPE  slis_t_fieldcat_alv.
DATA:    w_fieldcat      TYPE slis_fieldcat_alv.
DATA:    w_layout        TYPE slis_layout_alv.

*----------------------------------------------VARIABLES_MSJ
DATA: lt_report TYPE sy-repid.
DATA: it_listheader TYPE slis_t_listheader,
      wa_listheader TYPE slis_listheader.


DATA: lf_sp_group  TYPE slis_t_sp_group_alv, "MANEJAR GRUPOS DE CAMPOS
      lf_layout    TYPE slis_layout_alv,   "MANEJAR DISEÑO DE LAYOUT
      it_topheader TYPE slis_t_listheader,  "MANEJAR CABECERA DEL REP
      wa_top       LIKE LINE OF it_topheader. "LÍNEA PARA CABECERA


DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.     "Parametros del catalogo
DATA msj TYPE string.



*&---------------------------------------------------------------------*
*&  Include           ZFIFEL_RD001_01
*&---------------------------------------------------------------------*

*CONSULTAS

 SELECT  *
  INTO TABLE  @DATA(lt_felrd03)
  FROM  ztsd_felrd03
  WHERE bukrs EQ @p_bukrs.


 SELECT *
   INTO TABLE @DATA(lt_bkpf)
   FROM bkpf
   WHERE
   bukrs  EQ @p_bukrs  AND
    gjahr IN @so_gjahr AND
    bldat IN @so_bldat AND
    blart IN ( 'KI', 'KM' ).

 SELECT *
   INTO TABLE @DATA(lt_bseg)
   FROM bseg
   FOR ALL ENTRIES IN @lt_bkpf
   WHERE
   bukrs  EQ @lt_bkpf-bukrs AND
   gjahr  EQ @lt_bkpf-gjahr AND
   belnr  EQ @lt_bkpf-belnr AND
   shkzg = 'S' AND
   bschl = '40'.



 LOOP AT  lt_bkpf  INTO   DATA(wa_bkpf).

   w_zfifel-bukrs   =   wa_bkpf-bukrs. "Sociedad
   w_zfifel-belnr   =   wa_bkpf-belnr. "Documento
   w_zfifel-blart   =   wa_bkpf-blart. "Clase
   w_zfifel-xblnr   =   wa_bkpf-xblnr. "Comprobante Fiscal

   READ TABLE lt_bseg INTO DATA(wa_bseg)  WITH  KEY bukrs = wa_bkpf-bukrs
                                                    belnr = wa_bkpf-belnr
                                                    gjahr = wa_bkpf-gjahr.

   w_zfifel-lifnr = wa_bseg-lifnr. " Proveedor
   w_zfifel-dmbtr = wa_bseg-dmbtr. " Importe

   IF wa_bkpf-blart = 'KI'.

     SELECT SINGLE name1   stcd1
       INTO (w_zfifel-name1 , w_zfifel-stcd1)
       FROM lfa1
       WHERE  lifnr = wa_bseg-lifnr.
   ENDIF .

   IF  wa_bkpf-blart = 'KM'.

     SELECT SINGLE butxt
          INTO w_zfifel-butxt
          FROM t001
          WHERE  bukrs = wa_bkpf-bukrs.

        SELECT SINGLE PAVAL
          INTO w_zfifel-PAVAL
          FROM T001Z
         WHERE  bukrs = wa_bkpf-bukrs.
   ENDIF .

  w_zfifel-zestatus  = so_esta .

   APPEND w_zfifel to t_zfifel .
    CLEAR: w_zfifel.
 ENDLOOP.



 IF t_zfifel[] IS INITIAL.
    MESSAGE 'No existen datos para su seleccion' TYPE 'I'.
    EXIT.
  ELSE.

    PERFORM do_layout.
    PERFORM do_fieldcat.
    PERFORM call_alv.
ENDIF.
*&---------------------------------------------------------------------*
*&      Form  DO_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_layout .
  CLEAR w_layout.
  w_layout-box_fieldname     = 'CHECKED'.
  w_layout-zebra             = 'X'.
  w_layout-colwidth_optimize = ' '.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DO_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_fieldcat .


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'BUKRS'.
  w_fieldcat-reptext_ddic   = 'Sociedad  '.
  w_fieldcat-seltext_l      = 'Sociedad '.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'BELNR'.
  w_fieldcat-reptext_ddic   = 'Documento'.
  w_fieldcat-seltext_l      = 'Documento'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'XBLNR'.
  w_fieldcat-reptext_ddic   = 'Comprobante Fiscal'.
  w_fieldcat-seltext_l      = 'Comprobante Fiscal'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'LIFNR'.
  w_fieldcat-reptext_ddic   = 'Proveedor'.
  w_fieldcat-seltext_l      = 'Proveedor'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'NAME1'.
  w_fieldcat-reptext_ddic   = 'Nombre(KI)'.
  w_fieldcat-seltext_l      = 'Nombre(KI)'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'BUTXT'.
  w_fieldcat-reptext_ddic   = 'Nombre(KM)'.
  w_fieldcat-seltext_l      = 'Nombre(KM)'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'DMBTR'.
  w_fieldcat-reptext_ddic   = 'Importe'.
  w_fieldcat-seltext_l      = 'Importe'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.

  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'PAVAL'.
  w_fieldcat-reptext_ddic   = 'Identificación Fiscal (KM)'.
  w_fieldcat-seltext_l      = 'Identificación Fiscal (KM)'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'STCD1'.
  w_fieldcat-reptext_ddic   = 'Identificación Fiscal(KI)'.
  w_fieldcat-seltext_l      = 'Identificación Fiscal(KI)'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'ACCION'.
  w_fieldcat-reptext_ddic   = 'Sociedad  '.
  w_fieldcat-seltext_l      = 'Sociedad '.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.


  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'T_ZFIFEL'.
  w_fieldcat-fieldname      = 'ZESTATUS '.
  w_fieldcat-reptext_ddic   = 'Estado de documento'.
  w_fieldcat-seltext_l      = 'Estado de documento'.
  w_layout-colwidth_optimize = 'X'.
  w_fieldcat-emphasize     = 'C200'.
  APPEND w_fieldcat TO i_fieldcat.

ENDFORM.

FORM call_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout                = w_layout
      it_fieldcat              = i_fieldcat[]
      i_callback_pf_status_set = 'MI_STANDARD'
      i_callback_user_command  = 'MI_USER_COMMAND'
      i_save                   = 'X'
    TABLES
      t_outtab                 = T_ZFIFEL[].
*    EXCEPTIONS
*      program_error      = 1
*      OTHERS             = 2.

ENDFORM.

FORM mi_standard USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'. "Nombre del Status GUI
ENDFORM.
          
