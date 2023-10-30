TYPES: t_soli_t      TYPE TABLE OF soli.

 DATA: send_request  TYPE REF TO cl_bcs,
        send_document TYPE REF TO cl_document_bcs,
        sender        TYPE REF TO if_sender_bcs,
        recipient     TYPE REF TO if_recipient_bcs,
        bcs_exception TYPE REF TO cx_bcs,
       li_message     TYPE t_soli_t,
      lw_message     LIKE LINE OF li_message,
        mensaje(150)      type c.

  DATA: lv_result TYPE string,
        lv_msgv1  TYPE sy-msgv1.

  DATA: ld_text         TYPE soli_tab,
        ld_format       TYPE soodk-objtp,
        ld_email        TYPE adr6-smtp_addr,
        CORREO          TYPE adr6-smtp_addr,
        ld_filename(50) TYPE c,
        ld_subject(50)  TYPE c.

  DATA: att_size TYPE sood-objlen,
        att_hex  TYPE solix_tab.

  DATA: sent_to_all  TYPE os_boolean.

DATA:   my_grid_data1    TYPE merel_t_grid1,
        t_status         TYPE  merel_t_grid1.
  DATA: ls_data LIKE LINE OF my_grid_data1.
  DATA: ls_data2 LIKE LINE OF my_grid_data1.

IMPORT my_grid_data1 TO t_status FROM SHARED MEMORY indx(aa) ID 'MC'.


  if  IM_EBELN is NOT INITIAL  and sy-tcode = 'ME21N'.
TRY.
*   Crea solicitud de envio
      send_request = cl_bcs=>create_persistent( ).

     ld_subject = 'Liberación Pendiente OC'.

lw_message-line = 'Buen día,' .
   APPEND lw_message TO li_message.

  lw_message-line = '<BR></BR>'.
  APPEND lw_message TO li_message.

  lw_message-line = '<BR></BR>'.
  APPEND lw_message TO li_message.

CONCATENATE 'Usted tiene pendiente la liberación del siguiente pedido:' IM_EBELN  INTO   mensaje SEPARATED BY ' '. .
      lw_message-line =  mensaje.
  APPEND lw_message TO li_message.

    lw_message-line = '<BR></BR>'.
  APPEND lw_message TO li_message.
   lw_message-line = '<BR></BR>'.
  APPEND lw_message TO li_message.


  lw_message-line = 'Muchas gracias. ' .
   APPEND lw_message TO li_message.

  lw_message-line = '<BR></BR>'.
  APPEND lw_message TO li_message.


      send_document = cl_document_bcs=>create_document(
        i_type    = 'HTM'
         i_text    =  li_message
          i_subject = ld_subject ).


SELECT SINGLE EMAIL
 into correo
FROM ZUSERLIBER
  WHERE  FRGCO = '05'.

TRANSLATE correo TO LOWER CASE.
*    Establece Mensaje y/o Adjuntos del correo a la Solicitud de Envio
   send_request->set_document( send_document ).

*result = 'proveedor.nacional@lpliderpollo.com'.
     sender = cl_sapuser_bcs=>create( sy-uname ). "sy-uname Nombre de usuario dummy (Galletas Dondé Proveedores)
      send_request->set_sender( sender ).
ld_email =  correo.
*   Dirección Destinatarios
      recipient = cl_cam_address_bcs=>create_internet_address( ld_email ).
      send_request->add_recipient( recipient ).

*   Se envía el correo
      send_request->send( i_with_error_screen = abap_true ).

*   Captura errores
    CATCH cx_bcs INTO bcs_exception.

      CALL METHOD bcs_exception->if_message~get_text
        RECEIVING
          result = lv_result.
      lv_msgv1 = lv_result.
      MESSAGE s368(00) WITH lv_msgv1 DISPLAY LIKE 'E'.


  ENDTRY.
ENDIF.
