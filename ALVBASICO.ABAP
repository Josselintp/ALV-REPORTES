*&---------------------------------------------------------------------*
*& Report  ZPRODUCTO_PROCESO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zproducto_proceso.

TABLES: vbak, vbfa, kna1,  vbap.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
  SELECT-OPTIONS: s_vkorg   FOR vbak-vkorg OBLIGATORY. "Organización de ventas
  SELECT-OPTIONS: s_audat   FOR vbak-audat. "Fecha
  PARAMETERS:     p_check   AS CHECKBOX. "Documentos sin facturar
SELECTION-SCREEN: END OF BLOCK b1.



TYPES: BEGIN OF st_alv,
       vbelv   LIKE vbfa-vbelv,    " Número de Cotización
       vbeln1   LIKE vbfa-vbeln,   " Número de Factura.
       vbeln   LIKE vbak-vbeln,    " Numero de Pedido
       bstnk   LIKE vbak-bstnk,    " Orden de compra cliente
       arktx   LIKE vbap-arktx,    " Descripción del material
       matnr   LIKE vbap-matnr,    " Código material
       netwr   LIKE vbap-netwr,    "  Monto
       waerk   LIKE vbap-waerk,   " Moneda
       aufnr   LIKE aufk-aufnr,
       name1   LIKE kna1-name1,   " Nombre cliente
       name2   LIKE kna1-name2,   " Nombre cliente
       des(100),                   " Nombre cliente

       END OF st_alv.


    DATA:
      it_alv  TYPE TABLE OF st_alv,
      wa_alv  TYPE  st_alv.

************************************************************************
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



START-OF-SELECTION.

SELECT  *
INTO TABLE  @DATA(lt_vbak)
FROM  vbak
WHERE vkorg IN @s_vkorg
 AND  audat IN @s_audat.


IF lt_vbak IS NOT INITIAL.

SELECT *
  INTO TABLE @DATA(lt_vbap)
  FROM vbap
  FOR ALL ENTRIES IN @lt_vbak
  WHERE vbeln EQ @lt_vbak-vbeln.

SELECT *
  INTO TABLE @DATA(lt_vbfa)
  FROM vbfa
  FOR ALL ENTRIES IN @lt_vbap
  WHERE vbelv EQ  @lt_vbap-vbeln.
*  AND   vbtyp_n EQ 'M' .

  SELECT *
    INTO TABLE @DATA(lt_vbrk)
    FROM vbrk
    FOR ALL ENTRIES IN @lt_vbfa
    WHERE vbeln  EQ @lt_vbfa-vbeln.

  SELECT *
    INTO TABLE @DATA(lt_aufk)
    FROM aufk
    FOR ALL ENTRIES IN @lt_vbak
    WHERE kdauf EQ @lt_vbak-vbeln.

    SELECT *
      INTO TABLE @DATA(lt_kna1)
     FROM kna1
      FOR ALL ENTRIES IN  @lt_vbak
      WHERE  kunnr =  @lt_vbak-kunnr .
  ENDIF.



LOOP  AT  lt_vbap  INTO  DATA(wa_vbap).

wa_alv-arktx =  wa_vbap-arktx.   "Descripción del material:
wa_alv-matnr =  wa_vbap-matnr.   "Código material
wa_alv-netwr =  wa_vbap-netwr.   "Monto
wa_alv-waerk =  wa_vbap-waerk.   "Moneda



 READ TABLE lt_vbak INTO DATA(wa_vbak) WITH KEY vbeln = wa_vbap-vbeln.

  wa_alv-vbeln = wa_vbak-vbeln.  " Número de Pedido
  wa_alv-bstnk = wa_vbak-bstnk.  " Orden de compra cliente


READ TABLE lt_vbfa INTO DATA(wa_vbfa) WITH KEY  vbelv = wa_vbak-vbeln.

 IF wa_vbfa-vbtyp_v = 'B'.
  wa_alv-vbelv = wa_vbfa-vbelv.   "Número de Cotizació
  ENDIF.


READ TABLE  lt_vbrk INTO DATA(wa_vbrk) WITH  KEY vbeln = wa_vbfa-vbeln.

  IF wa_vbrk-sfakn IS INITIAL.

  READ TABLE lt_vbfa INTO DATA(wa_vbfa1) WITH KEY VBELV = WA_VBAP-VBELN  posnv = wa_vbap-posnr.


   IF wa_vbfa-vbtyp_n = 'M'.  "ORDEN DE FABRICACION

    wa_alv-vbeln1 =  wa_vbfa1-vbeln  .
     ENDIF.
     ENDIF.


  READ TABLE lt_kna1 INTO DATA(wa_kna1) WITH KEY kunnr = wa_vbak-kunnr.

  CONCATENATE wa_kna1-name1 wa_kna1-name2 INTO  wa_alv-des.  "Nombre cliente:

  READ TABLE lt_aufk INTO DATA(wa_aufk) WITH  KEY kdauf = wa_vbak-vbeln .

   wa_alv-aufnr = wa_aufk-aufnr .  "Orden de Fabricación

    APPEND  wa_alv  TO it_alv. "para el alv
    CLEAR wa_alv .
ENDLOOP.

  IF  p_check  IS NOT INITIAL.

    DELETE it_alv WHERE  vbeln1 NE ' '.

   ENDIF.


  IF it_alv[] IS INITIAL.
  MESSAGE 'No existen datos para su seleccion' TYPE 'I'.
  EXIT.
ELSE.

  PERFORM do_layout.
  PERFORM do_fieldcat.
  PERFORM call_alv.
ENDIF.

**&---------------------------------------------------------------------*
FORM do_layout .

  CLEAR w_layout.
  w_layout-zebra             = 'X'.
  w_layout-colwidth_optimize = ' '.

ENDFORM.                    " DO_LAYOUT
**&---------------------------------------------------------------------*


FORM do_fieldcat .

  REFRESH: i_fieldcat.
  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'VBELV'.
  w_fieldcat-reptext_ddic   = 'Número de Cotización'.
  w_fieldcat-seltext_l      = 'Número de Cotización '.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.

  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'VBELN'.
  w_fieldcat-reptext_ddic   = 'Numero de Pedido'.
  w_fieldcat-seltext_l      = 'Numero de Pedido '.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.


  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'BSTNK'.
  w_fieldcat-reptext_ddic   = 'Orden de compra cliente'.
  w_fieldcat-seltext_l      = 'Orden de compra cliente'.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.


  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'DES'.
  w_fieldcat-reptext_ddic   = 'Nombre cliente'.
  w_fieldcat-seltext_l      = 'Nombre cliente'.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.


  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'ARKTX'.
  w_fieldcat-reptext_ddic   = 'Descripción del material'.
  w_fieldcat-seltext_l      = 'Descripción del material'.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.


  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'MATNR'.
  w_fieldcat-reptext_ddic   = 'Código material'.
  w_fieldcat-seltext_l      = 'Código material'.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.


  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'AUFNR'.
  w_fieldcat-reptext_ddic   = 'Orden de Fabricación'.
  w_fieldcat-seltext_l      = 'Orden de Fabricación'.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.


  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'NETWR'.
  w_fieldcat-reptext_ddic   = 'Monto'.
  w_fieldcat-seltext_l      = 'Monto'.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.


  CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'WAERK'.
  w_fieldcat-reptext_ddic   = 'Moneda'.
  w_fieldcat-seltext_l      = 'Moneda'.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.

 CLEAR:   w_fieldcat.
  w_fieldcat-outputlen      = '30'.
  w_fieldcat-tabname        = 'IT_ALV'.
  w_fieldcat-fieldname      = 'VBELN1'.
  w_fieldcat-reptext_ddic   = 'Número de Factura'.
  w_fieldcat-seltext_l      = 'Número de Factura'.
  w_layout-colwidth_optimize = 'X'.
  APPEND w_fieldcat TO i_fieldcat.



ENDFORM.

FORM call_alv.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
*      i_callback_top_of_page   = 'TOP_OF_PAGE'
      is_layout                = w_layout
      it_fieldcat              =  i_fieldcat[]
*      i_callback_pf_status_set = 'SET_PF_STATUS'
*      i_callback_user_command  = 'USER_COMMAND'

    TABLES
      t_outtab                 = it_alv[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.


ENDFORM.

**&---------------------------------------------------------------------*
*FORM set_pf_status USING rt_extab TYPE slis_t_extab.
*  SET PF-STATUS 'STANDARD'.
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form top_of_page
**&---------------------------------------------------------------------*
*
*FORM user_command USING r_ucomm LIKE sy-ucomm
*                        rs_selfield TYPE slis_selfield.


*  ENDFORM.
