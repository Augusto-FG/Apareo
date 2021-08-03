       IDENTIFICATION DIVISION.
       PROGRAM-ID. APAREO.
       ENVIRONMENT DIVISION.
      *--------------------

       CONFIGURATION SECTION.
      *---------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
      *--------------------
       FILE-CONTROL.
      *------------

           SELECT CLIENTES ASSIGN TO
           'C:\Users\a.b.fernandez.garcia\Documents\clientes.DAT'
                                     FILE STATUS IS FS-CLIENTES.
      *                               ACCESS MODE IS SEQUENTIAL.
           SELECT VENTAS ASSIGN TO
           'C:\Users\a.b.fernandez.garcia\Documents\ventas.DAT'
                                     FILE STATUS IS FS-VENTAS.
      *                               ACCESS MODE IS SEQUENTIAL.
           SELECT APAREO ASSIGN TO
           'C:\Users\a.b.fernandez.garcia\Documents\APAREO.TXT'
                                     FILE STATUS IS FS-APAREO.
      *                               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
      *-------------
       FILE SECTION.
      *------------

       FD  CLIENTES.
      *****************************************************************
      *    REGISTRO ARCHIVO CLIENTES
      *****************************************************************
       01  REG-CLIENTES-FD.
           05  IDE                        PIC 9(02).
           05  DESCRIPCION                PIC X(15).
           05  DIRECCIÓN                  PIC X(30).

       FD  VENTAS.
      *****************************************************************
      *              REGISTRO ARCHIVO DE VENTAS
      *****************************************************************
       01  REG-VENTAS-FD.
           05  IDEVE                      PIC 9(09).
           05  IDECLI                     PIC 9(02).
           05  PRODUCTO                   PIC X(15).
           05 CANTIDAD                    PIC X(06).
           05 PRECIO                      PIC x(09).
      *****************************************************************
      *              REGISTRO ARCHIVO DE APAREO
      *****************************************************************

       FD  APAREO.
       01  REG-APAREO-FD.
           05  APA-CLI              PIC 9(02).
           05  APA-DESCRIP          PIC X(15).
           05  APA-CANTIDAD         PIC X(06).
           05  APA-IMPORTE          PIC X(09).
           05  APA-DPV              PIC X(15).


       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      *MARGEN SUPERIOR DEL ENCABEZADO
      *-----------------------------------------------------------------
        01 LINEA1.
           05 DESCRIP PIC x(38) VALUE "DESCRIPCION DEL CLIENTE:".

        01 LINEA2.
           05 CANTIDADVENDIDA PIC X(30) VALUE "LA CANTIDAD VENDIDA ES:".
           05 IMPORTE PIC X(8) VALUE "IMPORTE".
           05 DECPRODUCTO PIC X(20) VALUE "DES. DEL PRODUCTO:".
        01 LINEA3.
            05 CANTTOTAL PIC X(25) VALUE "LA CANTIDAD TOTAL ES:".
            05 IMPORTETOTAL PIC X(25) VALUE "LA CANTIDAD TOTAL ES:".

        01 FS-CLIENTES                    PIC X(02).
        01 FS-VENTAS                      PIC X(02).
        01 FS-APAREO                      PIC X(02).

       PROCEDURE DIVISION.
      *-------------------
       MAIN-PROCEDURE.
           DISPLAY 'COMIENZA EL PROGRAMA'
           PERFORM 100-INICIO.

           PERFORM 200-PROCESO
            UNTIL IDE = 99
            AND IDECLI = 99
      *CORTE DE CONTROL - SIGUE EN LINEA 178 Y 197
           PERFORM 300-FINALIZO.

           STOP RUN.

       100-INICIO.

           PERFORM 1000-ABRO-ARCHIVOS

           PERFORM 1102-LEO-CLIENTES

           PERFORM 1103-LEO-VENTAS.

       200-PROCESO.
           DISPLAY "IDE " IDE.
           DISPLAY "IDECLI " IDECLI.

              IF IDE = IDECLI
                   PERFORM 3000-SALIDA
      *PARA SEGUIR LEYENDO LOS REGISTRO (LOS DEL .DAT) UTILIZO LOS SIGUIENTES PERFORM
                   PERFORM 1103-LEO-VENTAS
                   PERFORM 1102-LEO-CLIENTES

              ELSE
                  IF IDE > IDECLI
                      PERFORM 1103-LEO-VENTAS
                  ELSE
                      PERFORM 1102-LEO-CLIENTES
                  END-IF
              END-IF.


       3000-SALIDA.
           INITIALIZE REG-APAREO-FD.
              MOVE IDE          TO APA-CLI.
              MOVE DESCRIPCION  TO APA-DESCRIP.
              MOVE CANTIDAD     TO APA-CANTIDAD.
              MOVE PRECIO       TO APA-IMPORTE.
              MOVE PRODUCTO     TO APA-DPV.
              WRITE REG-APAREO-FD
           IF FS-APAREO = '00'
              CONTINUE
              DISPLAY "ESCRIBIO CORRECTAMENTE"
           ELSE
              DISPLAY 'ERROR EN LEER ARCHIVO CLIENTES'
              DISPLAY 'ARCHIVO CLIENTES'
              DISPLAY 'FS-CLIENTES: ' FS-APAREO
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


       300-FINALIZO.

           PERFORM 5000-CIERRO-ARCHIVOS.

       1000-ABRO-ARCHIVOS.

           OPEN INPUT CLIENTES

           IF FS-CLIENTES = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 1000-ABRO-ARCHIVOS'
              DISPLAY 'ARCHIVO CLIENTES'
              DISPLAY 'FS-CLIENTES: ' FS-CLIENTES
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


           OPEN INPUT VENTAS

           IF FS-VENTAS = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 1000-ABRO-ARCHIVOS'
              DISPLAY 'ARCHIVO VENTAS'
              DISPLAY 'FS-VENTAS: ' FS-VENTAS
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF


           OPEN OUTPUT APAREO

           IF FS-APAREO = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 1000-ABRO-ARCHIVOS'
              DISPLAY 'ARCHIVO APAREO'
              DISPLAY 'FS-APAREO: ' FS-APAREO
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.

       1102-LEO-CLIENTES.
           INITIALIZE REG-CLIENTES-FD.
      * Initialize= inicializa los campos
            READ CLIENTES
                AT END
                     DISPLAY 'ENCONTRO FIN DE ARCHIVO CLIENTES'
                     MOVE 99 TO IDE
      *          NOT AT END
      *             DISPLAY 'NO ES FIN DE ARCHIVO CLIENTES'
           IF FS-CLIENTES = '00' OR FS-CLIENTES = '10'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN LEER ARCHIVO CLIENTES'
              DISPLAY 'ARCHIVO CLIENTES'
              DISPLAY 'FS-CLIENTES: ' FS-CLIENTES
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


       1103-LEO-VENTAS.
           INITIALIZE REG-VENTAS-FD.
            READ VENTAS
                AT END
                     DISPLAY 'ENCONTRO FIN DE ARCHIVO VENTAS'
                     MOVE 99 TO IDECLI
      *          NOT AT END
      *             DISPLAY 'NO ES FIN DE ARCHIVO VENTAS'
            IF FS-VENTAS = '00' OR FS-VENTAS = '10'
              CONTINUE
           DISPLAY  "IDEVE: " IDEVE
           DISPLAY "IDECLI: " IDECLI
           DISPLAY "PRODUCTO: " PRODUCTO
           DISPLAY "CANTIDAD: " CANTIDAD
           DISPLAY "PRECIO: " PRECIO
            ELSE
              DISPLAY 'ERROR LEER ARCHIVOS VENTAS'
              DISPLAY 'ARCHIVO CLIENTES'
              DISPLAY 'FS-VENTAS: ' FS-VENTAS
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
            END-IF.



       5000-CIERRO-ARCHIVOS.

           CLOSE CLIENTES.

           IF FS-CLIENTES = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 5000-CIERRO-ARCHIVOS'
              DISPLAY 'ARCHIVO CLIENTES'
              DISPLAY 'FS-CLIENTES: ' FS-CLIENTES
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


           CLOSE VENTAS.

           IF FS-VENTAS = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 5000-CIERRO-ARCHIVOS'
              DISPLAY 'ARCHIVO VENTAS'
              DISPLAY 'FS-VENTAS: ' FS-VENTAS
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.


           CLOSE APAREO.

           IF FS-APAREO = '00'
              CONTINUE
           ELSE
              DISPLAY 'ERROR EN 5000-CIERRO-ARCHIVOS'
              DISPLAY 'ARCHIVO APARERO'
              DISPLAY 'FS-APAREO: ' FS-APAREO
              DISPLAY 'SE CANCELA EL PROGRAMA'
              STOP RUN
           END-IF.
