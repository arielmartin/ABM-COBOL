******************************************************************
      * Author: ARIEL MARTIN
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************

      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. "ABM".

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL CLIENTES ASSIGN TO "d:\data\clientes.dat"
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS ID_CLIENTE
                  ALTERNATE KEY CLI_NOMBRE WITH DUPLICATES
                  ALTERNATE KEY CLI_ALT_2 WITH DUPLICATES
                  FILE STATUS ST-FILE.

             SELECT ID-FILE ASSIGN TO "d:\data\last-id.dat"
                            FILE STATUS ST-ID.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.

       FD  CLIENTES.
       01  REG-CLIENTES.
           03  ID_CLIENTE.
               05 CLI_ID           PIC 9(7).
           03 CLI_SALDO            PIC S9(7)V9(3).
           03 CLI_NOMBRE           PIC X(60).
           03 CLI_DIRECCION        PIC X(80).
           03 CLI_CODPOST          PIC X(10).
           03 CLI_CATEGORIA        PIC X.
           03 CLI_ALT_2.
               05 CLI_CATEGORIA_2  PIC X.
               05 CLI_NOMBRE_2     PIC X(60).
           03  FILLER              PIC X(240).

       FD ID-FILE.
           01 REG-ID               PIC 9(7).

       WORKING-STORAGE SECTION.

       01  ST-FILE         PIC XX.
       01  ST-ID           PIC XX.

       01  MENSAJE         PIC X(70).
       01  FIN             PIC X VALUES "N".
       01  EXISTE          PIC X.
       01  HUBO-ERROR      PIC 9 VALUES 0.
       01  GUIONES         PIC X(80) VALUES ALL "-".
       01  OPCION          PIC X.

       01  W-CLI-ID        PIC 9(07).
       01  W-CLI-ID-Z      PIC Z(06)9.

       01  DATOS.
           02 W-CLI-NOMBRE PIC X(70).
           02 W-CLI-NOMBRE-ANT PIC X(70).
           02 W-CLI-DIRECCION PIC X(70).
           02 W-CLI-CODPOST PIC X(70).
           02 W-CLI-CATEGORIA PIC X(70).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       MAIN-PROCEDURE.
           PERFORM INICIALIZACION.
      *    PERFORM ABRO-ARCHIVO.
           PERFORM PROCESO THRU F-PROCESO UNTIL FIN = "S".
           PERFORM CIERRO-ARCHIVO.
           GO TO FINALIZAR.

       INICIALIZACION.
           MOVE "N" TO FIN.
           MOVE "S" TO EXISTE.

       ABRO-ARCHIVO.
           OPEN I-O CLIENTES.
           IF ST-FILE > "07"
             STRING "ERROR AL ABRIR ARCHIVO CLIENTES " ST-FILE
               DELIMITED BY SIZE INTO MENSAJE
              DISPLAY MENSAJE LINE 10 COL 20
              MOVE "S" TO FIN.

           OPEN I-O ID-FILE.
           IF ST-ID > "07"
             STRING "ERROR AL ABRIR ARCHIVO ID-FILE " ST-ID
               DELIMITED BY SIZE INTO MENSAJE
              DISPLAY MENSAJE LINE 10 COL 20.


       CIERRO-ARCHIVO.
           CLOSE CLIENTES.
           CLOSE ID-FILE.

       FINALIZAR.
      *    stop "  precione una tecla para CERRAR...".
           STOP RUN.

       PROCESO.

           PERFORM OPCIONES THRU F-OPCIONES.
           IF FIN = "N"
               PERFORM LEO-CLIENTES THRU F-LEO-CLIENTES
               IF HUBO-ERROR = 1
                   MOVE "S" TO FIN
                   GO TO F-PROCESO
               END-IF

               IF EXISTE = "S"
                   PERFORM MUESTRO-DATOS
               ELSE
                   PERFORM CARGO-DATOS THRU F-CARGO-DATOS
               END-IF
               PERFORM OPCIONES.

       F-PROCESO.
           EXIT.

       OPCIONES.

           DISPLAY "A.B.M. Clientes"       LINE 03 COL 32
                   GUIONES                 LINE 04 COL 01

           IF OPCION <> "A" AND <> "B" AND <> "M" AND <> "G"
      
           DISPLAY SPACES                  LINE 6 COL 1 SIZE 80
           DISPLAY "[A] ALTA"              LINE 06 COL 05
                   "[B] BUSCAR"            LINE 06 COL 20
                   "[S] SALIR"             LINE 06 COL 65
                   GUIONES                 LINE 08 COL 01
           DISPLAY SPACES LINE 12 COL 1 SIZE 80
           DISPLAY SPACES LINE 14 COL 1 SIZE 80
           DISPLAY SPACES LINE 16 COL 1 SIZE 80
           DISPLAY SPACES LINE 18 COL 1 SIZE 80

           ELSE IF OPCION = "A" OR "M"

           DISPLAY SPACES                  LINE 6 COL 1 SIZE 80
           DISPLAY "[G] GRABAR"            LINE 06 COL 05
                   "[1 2 3 4]   MODIFICAR-DATOS"   LINE 06 COL 20
                   "[V] VOLVER"            LINE 06 COL 65
                   GUIONES                 LINE 08 COL 01

           ELSE IF OPCION = "B" OR "G"

           DISPLAY SPACES                  LINE 6 COL 1 SIZE 80
           DISPLAY "[E] ELIMINAR"          LINE 06 COL 05
                   "[1 2 3 4]   MODIFICAR-DATOS"   LINE 06 COL 20
                   "[V] VOLVER"            LINE 06 COL 65
                   GUIONES                 LINE 08 COL 01.

           DISPLAY "OPCION [ ]"            LINE 23 COL 66
                    GUIONES                LINE 22 COL 01.

           PERFORM CIERRO-ARCHIVO.

           ACCEPT  OPCION                  LINE 23 COL 74.
           inspect OPCION converting "abemsgv" to "ABEMSGV".

      * LIMPIO MENSAJE
           DISPLAY SPACES  LINE 20  COL 1 SIZE 80.
     
           IF FIN = "N"
              EVALUATE OPCION
               WHEN 1
                   MOVE "M" TO OPCION
                   PERFORM INGRESO-NOMBRE
               WHEN 2
                   MOVE "M" TO OPCION
                   PERFORM INGRESO-DIRECCION
               WHEN 3
                   MOVE "M" TO OPCION
                   PERFORM INGRESO-CODPOSTAL
               WHEN 4
                   MOVE "M" TO OPCION
                   PERFORM INGRESO-CATEGORIA
               WHEN "A"
                   PERFORM CARGO-DATOS THRU F-CARGO-DATOS
               WHEN "E"
                   PERFORM BORRAR
               WHEN "B"
                   PERFORM INGRESO-ID THRU F-BUSCAR
               WHEN "G"
                   PERFORM GRABAR THRU F-GRABAR
               WHEN "V"
                   MOVE "S" TO EXISTE
                   GO TO OPCIONES
               WHEN "S"
                   MOVE "S" TO FIN
                   GO TO CIERRO-ARCHIVO
               WHEN OTHER
                   MOVE "OPCION INCORRECTA" TO MENSAJE
                   PERFORM MOSTRAR-MENSAJE
                   GO TO OPCIONES
              END-EVALUATE

                   GO TO OPCIONES.
       F-OPCIONES.
           EXIT.

       INGRESO-ID.
           DISPLAY "INGRESE ID : " LINE 10 COL 5
           ACCEPT W-CLI-ID LINE 10 COL 23.
           MOVE W-CLI-ID TO W-CLI-ID-Z.
           DISPLAY W-CLI-ID-Z LINE 10 COL 23.
           IF W-CLI-ID = 0
               MOVE "NO PUEDE INTRODUCIR ID = 0" TO MENSAJE
               PERFORM MOSTRAR-MENSAJE
               MOVE SPACE TO OPCION
               PERFORM OPCIONES.

       F-INGRESO-ID.
           EXIT.

       LEO-CLIENTES.
           PERFORM ABRO-ARCHIVO.
      
           MOVE W-CLI-ID TO CLI_ID.
           MOVE "S" TO EXISTE.
           
           READ CLIENTES INVALID KEY MOVE "N" TO EXISTE.
           IF ST-FILE = "99" GO TO LEO-CLIENTES.
           IF ST-FILE > "07" AND ST-FILE NOT = "23"
                   STRING "ERROR LEYENDO CLIENTES. STATUS = " ST-FILE
                   DELIMITED BY SIZE INTO MENSAJE
                   DISPLAY MENSAJE LINE 23 COL 1
                   MOVE 1 TO HUBO-ERROR
                   MOVE "N" TO EXISTE.

       F-LEO-CLIENTES.
           EXIT.

       MUESTRO-DATOS.

           IF OPCION = "A" OR "B" AND EXISTE <> "N"
           DISPLAY
      *            "ID CLIENTE     : "   LINE 07 COL 10
                   "01. NOMBRE     : "   LINE 12 COL 10
                   "02. DIRECCION  : "   LINE 14 COL 10
                   "03. COD.POSTAL : "   LINE 16 COL 10
                   "04. CATEGORIA  : "   LINE 18 COL 10
                   GUIONES               LINE 22 COL 01.

           IF EXISTE = "S" AND OPCION = "B"
               MOVE CLI_NOMBRE    TO W-CLI-NOMBRE
               MOVE CLI_DIRECCION TO W-CLI-DIRECCION
               MOVE CLI_CODPOST   TO W-CLI-CODPOST
               MOVE CLI_CATEGORIA TO W-CLI-CATEGORIA
           
           DISPLAY CLI_NOMBRE    LINE 12 COL 36
                   CLI_DIRECCION LINE 14 COL 36
                   CLI_CODPOST   LINE 16 COL 36
                   CLI_CATEGORIA LINE 18 COL 36.

           IF EXISTE = "N" AND OPCION <> "A"
               MOVE "ID NO ENCONTRADO" TO MENSAJE
               MOVE SPACE TO OPCION
               PERFORM MOSTRAR-MENSAJE.

       F-BUSCAR.
           EXIT.

       CARGO-DATOS.
      * CHEQUEAR ID
           INITIALIZE DATOS.
           MOVE "N" TO EXISTE.
           PERFORM MUESTRO-DATOS.

       INGRESO-NOMBRE.
           MOVE W-CLI-NOMBRE TO W-CLI-NOMBRE-ANT.
           ACCEPT W-CLI-NOMBRE LINE 12 COL 36 UPDATE.
           IF W-CLI-NOMBRE = SPACES
               MOVE W-CLI-NOMBRE-ANT TO W-CLI-NOMBRE
               GO TO INGRESO-NOMBRE.
           DISPLAY W-CLI-NOMBRE LINE 12 COL 36.

       INGRESO-DIRECCION.
           ACCEPT W-CLI-DIRECCION LINE 14 COL 36 UPDATE.
           IF W-CLI-NOMBRE = SPACES
               GO TO INGRESO-DIRECCION.
           DISPLAY W-CLI-DIRECCION LINE 14 COL 36.

       INGRESO-CODPOSTAL.
           ACCEPT W-CLI-CODPOST LINE 16 COL 36 UPDATE.
           IF W-CLI-CODPOST = SPACES
               GO TO INGRESO-CODPOSTAL.
           DISPLAY W-CLI-CODPOST LINE 16 COL 36.

       INGRESO-CATEGORIA.
           ACCEPT W-CLI-CATEGORIA LINE 18 COL 36 UPDATE.
           IF W-CLI-CATEGORIA = SPACES
               GO TO INGRESO-CATEGORIA.
           DISPLAY W-CLI-CATEGORIA LINE 18 COL 36.

       F-CARGO-DATOS.
           EXIT.



       GRABAR.

       PERFORM ABRO-ARCHIVO.

      * resolver id
           MOVE W-CLI-NOMBRE       TO CLI_NOMBRE CLI_NOMBRE_2
           MOVE W-CLI-DIRECCION    TO CLI_DIRECCION.
           MOVE W-CLI-CODPOST      TO CLI_CODPOST.
           MOVE W-CLI-CATEGORIA    TO CLI_CATEGORIA CLI_CATEGORIA_2.

       GRABO.

           IF EXISTE = "S" GO TO REGRABO.

           READ ID-FILE INTO REG-ID.
           ADD 1 TO REG-ID.
           REWRITE REG-ID.
           MOVE REG-ID TO CLI_ID.
      *    DISPLAY "EL ID GUARDADO ES: " REG-ID.
           
           WRITE REG-CLIENTES.
           IF ST-FILE = "99" GO TO GRABO.
           IF ST-FILE > "07"
               STRING "ERROR " ST-FILE " AL GRABAR CLIENTES " 
                   DELIMITED BY SIZE INTO MENSAJE
                   PERFORM MOSTRAR-MENSAJE
           ELSE
           MOVE "ARCHIVO GUARDADO CON EXITO" TO MENSAJE
           PERFORM MOSTRAR-MENSAJE.

           MOVE "S" TO EXISTE.
           GO TO F-GRABAR.

       REGRABO.
           REWRITE REG-CLIENTES
           IF ST-FILE = "99" GO TO REGRABO.

           IF ST-FILE > "07"
               STRING "ERROR AL GRABAR CLIENTES " ST-FILE
                   DELIMITED BY SIZE INTO MENSAJE
               PERFORM MOSTRAR-MENSAJE
           ELSE
           MOVE "ARCHIVO MODIFICADO CON EXITO" TO MENSAJE
           PERFORM MOSTRAR-MENSAJE.

       F-GRABAR.
           EXIT.

       BORRAR.
           PERFORM ABRO-ARCHIVO.
           DELETE CLIENTES.
           IF ST-FILE = "99" GO TO BORRAR.

           IF ST-FILE > "07"
               STRING "ERROR AL BORRAR CLIENTES " ST-FILE
                   DELIMITED BY SIZE INTO MENSAJE
              PERFORM MOSTRAR-MENSAJE
           ELSE
           MOVE "---ARCHIVO ELIMINADO---" TO MENSAJE
           PERFORM MOSTRAR-MENSAJE.

       MOSTRAR-MENSAJE.
           DISPLAY SPACES  LINE 20  COL 1 SIZE 80
           DISPLAY MENSAJE LINE 20 COL 25

       END PROGRAM "ABM".