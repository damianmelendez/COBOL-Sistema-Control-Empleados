      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NOMINA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WKS-DATOS-TRAB OCCURS 100 TIMES DEPENDING ON WKS-FIN
                                     INDEXED BY WKS-INDICE.
           02 WKS-NOMBRE-TRAB   PIC X(50).
           02 WKS-APELL-PAT     PIC X(50).
           02 WKS-APELL-MAT     PIC X(50).
           02 WKS-SALARIO       PIC S9(5)V99.
           02 WKS-ID-TRAB       PIC 9(2).
           02 WKS-DEPARTAMENTO  PIC 9(2) OCCURS 3 TIMES.

       77  WKS-FIN              PIC 9(03).
       77  WKS-SAL-FORMATO      PIC ZZZZ9.99.
       77  WKS-CONTADOR         PIC 9(2).
       77  WKS-TERMINA          PIC 9(01).
       77  WKS-RECORRE          PIC 9(03).
       77  WKS-ID-AUX           PIC 9(02).
       77  WKS-COL              PIC 9(1).
       77  WKS-ELECCION         PIC 9(1).
       77  WKS-INDICADOR        PIC 9(01).

       01  WKS-OPCION           PIC X(01) VALUE SPACE.
           88 WKS-OPC-ALTA      VALUE 'A'.
           88 WKS-OPC-CONS      VALUE 'C'.
           88 WKS-OPC-S         VALUE 'S'.
           88 WKS-OPC-N         VALUE 'N'.
           88 WKS-OPC-T         VALUE 'T'.
           88 WKS-OPC-M         VALUE 'M'.
           88 WKS-OPC-B         VALUE 'B'.

       77  WKS-AUX-NOMBRE       PIC X(50).
       77  WKS-AUX-SALARIO      PIC S9(5)V99.
       77  WKS-AUX-DEPTO        PIC 9(2).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PRINCIPAL.
               DISPLAY "NOMINA TRABAJADORES".
               MOVE ZERO TO WKS-TERMINA
                            WKS-FIN.
               PERFORM MENU UNTIL WKS-TERMINA = 1.

           STOP RUN.

           MENU.
           DISPLAY "A. ALTAS C. CONSULTA GENERAL T.CONSULTA TRABAJADOR",
                   "M-MODIFICAR TRAB B-BAJA S. SALIR "
           ACCEPT WKS-OPCION.
           IF WKS-OPC-ALTA
               PERFORM CAPTURRA-TRAB
           ELSE
           IF WKS-OPC-CONS
               MOVE 1 TO WKS-RECORRE
               PERFORM CONSULTA-GENERAL UNTIL WKS-RECORRE > WKS-FIN
           ELSE
           IF WKS-OPC-T
               PERFORM CONSULTA-TRAB

           ELSE
           IF WKS-OPC-M
               PERFORM MODIFICAR-TRAB

           ELSE
           IF WKS-OPC-B
               PERFORM BAJA-TRABAJADOR

           ELSE
           IF WKS-OPC-S
               MOVE 1 TO  WKS-TERMINA
           ELSE
               DISPLAY "OPCION INVALIDA VERIFIQUE..".




           CAPTURRA-TRAB.

           ADD 1 TO  WKS-FIN.
           IF WKS-FIN > 100
                   DISPLAY "YA NO SE PUEDE CUPTURAR MAS TRABAJADORES ",
                            "CONTACTE A SISTEMAS"
           ELSE

               DISPLAY "NOMBRE DEL EMPLEADO: "
               ACCEPT WKS-NOMBRE-TRAB(WKS-FIN)

               DISPLAY "APELLIDO PATERNO: "
               ACCEPT WKS-APELL-PAT(WKS-FIN)

               DISPLAY "APELLIDO MATERNO: "
               ACCEPT WKS-APELL-MAT(WKS-FIN)

               DISPLAY "SASLARIO TRABAJADOR: "
               ACCEPT WKS-SALARIO(WKS-FIN)


               MOVE 1 TO WKS-COL.
               PERFORM CAPTURA-DEPTO UNTIL WKS-COL > 3.

               ADD 10 TO WKS-CONTADOR
               MOVE WKS-CONTADOR TO WKS-ID-TRAB(WKS-FIN)
               PERFORM OTRA-CAPTURA.


           OTRA-CAPTURA.
           DISPLAY "DESEA CAPTURAR OTRO TRABAJADOR? S-SI N-NO".
           ACCEPT WKS-OPCION.

           IF WKS-OPC-S
               PERFORM CAPTURRA-TRAB
           ELSE
           IF WKS-OPC-N
               NEXT SENTENCE
           ELSE
               DISPLAY "OPCION VALIDA VERIFIQUE."
               PERFORM OTRA-CAPTURA.




           CONSULTA-GENERAL.
           IF WKS-ID-TRAB(WKS-RECORRE) > 0
               DISPLAY "ID-TRAB: " WKS-ID-TRAB(WKS-RECORRE)
               DISPLAY "NOMBRE: " WKS-NOMBRE-TRAB(WKS-RECORRE)
               DISPLAY "APELLIDOS: " WKS-APELL-PAT(WKS-RECORRE)
                                 WKS-APELL-MAT(WKS-RECORRE)

               MOVE WKS-SALARIO(WKS-RECORRE) TO  WKS-SAL-FORMATO
               DISPLAY "SALARIO: " WKS-SAL-FORMATO
               MOVE 1 TO WKS-COL
               PERFORM CONSULTA-DEPTO-GRAL UNTIL WKS-COL > 3
               ADD 1 TO WKS-RECORRE.


           CONSULTA-TRAB.
           DISPLAY "CUAL ES EL ID DEL TRABAJADOR?.."
           ACCEPT WKS-ID-AUX.

           SET WKS-INDICE TO 1
           SEARCH WKS-DATOS-TRAB
               AT END DISPLAY " TRABAJADOR NO ENCONTRADO"
               WHEN WKS-ID-TRAB(WKS-INDICE) = WKS-ID-AUX
           DISPLAY "ID-TRAB: " WKS-ID-TRAB(WKS-INDICE)
           DISPLAY "NOMBRE: " WKS-NOMBRE-TRAB(WKS-INDICE)
           DISPLAY "APELLIDOS: " WKS-APELL-PAT(WKS-INDICE)
                                 WKS-APELL-MAT(WKS-INDICE)

           MOVE WKS-SALARIO(WKS-INDICE) TO  WKS-SAL-FORMATO
           DISPLAY "SALARIO: " WKS-SAL-FORMATO
           MOVE 1 TO WKS-COL
           PERFORM CONSULTA-DEPTO-TRAB UNTIL WKS-COL > 3
           IF WKS-INDICADOR = 1
               PERFORM MODIFICACIONES
               MOVE ZERO TO WKS-INDICADOR
           ELSE
           IF WKS-INDICADOR = 2
               PERFORM BAJAS
               MOVE ZERO TO WKS-INDICADOR.





           CAPTURA-DEPTO.
               IF WKS-COL > 3
                   DISPLAY "SOLO SE PUEDE CAPTURAR 3 DEPARTAMETOS"
               ELSE
                   DISPLAY "CAPTURA DEPARTAMENTO DE TRABAJADOR:"
                   ACCEPT  WKS-DEPARTAMENTO(WKS-FIN,WKS-COL)
                   PERFORM OTRO-DEPTO UNTIL WKS-COL > 3.

           OTRO-DEPTO.
               DISPLAY "DESEA CAPTURAR OTRO DEPARTAMENTO S-SI N-NO".
               ACCEPT WKS-OPCION.

               IF WKS-OPC-S
                   ADD 1 TO WKS-COL
                   PERFORM CAPTURA-DEPTO
               ELSE
               IF WKS-OPC-N
                   MOVE 4 TO WKS-COL
               ELSE
                   DISPLAY "OPCION INVALIDA VERIFIQUE"
                   PERFORM OTRO-DEPTO.

           CONSULTA-DEPTO-GRAL.
            IF  WKS-DEPARTAMENTO(WKS-RECORRE, WKS-COL) NOT = ""
            OR  WKS-DEPARTAMENTO(WKS-RECORRE, WKS-COL) NOT = SPACES
               DISPLAY "DEPARTAMENTO " ,WKS-COL, ": "
               DISPLAY WKS-DEPARTAMENTO(WKS-RECORRE, WKS-COL)
               ADD 1 TO WKS-COL
            ELSE
                MOVE 4 TO WKS-COL.

           CONSULTA-DEPTO-TRAB.
            IF  WKS-DEPARTAMENTO(WKS-INDICE, WKS-COL) NOT = ""
            OR  WKS-DEPARTAMENTO(WKS-INDICE, WKS-COL) NOT = SPACES
               DISPLAY "DEPARTAMENTO " ,WKS-COL, ": "
               DISPLAY WKS-DEPARTAMENTO(WKS-INDICE, WKS-COL)
               ADD 1 TO WKS-COL
            ELSE
                MOVE 4 TO WKS-COL.

           MODIFICAR-TRAB.
               MOVE 1 TO WKS-INDICADOR
               PERFORM CONSULTA-TRAB.



           MODIFICACIONES.
               DISPLAY "QUE DESEAS MODIFICAR: 1-NOMBRE 2-APELLIDO PAT ",
                       "3-APELLIDO MAT 4-SALARIO 5-DEPTOS 6-CANCELAR".
               ACCEPT WKS-ELECCION.

               IF WKS-ELECCION = 1
                   DISPLAY "DAME EL NUVO NOMBRE"
                   ACCEPT WKS-AUX-NOMBRE
                   MOVE WKS-AUX-NOMBRE TO WKS-NOMBRE-TRAB(WKS-INDICE)

               ELSE
               IF WKS-ELECCION = 2
                   DISPLAY "DAME EL NUEVO APELLIDO PATERNO"
                   ACCEPT WKS-AUX-NOMBRE
                   MOVE WKS-AUX-NOMBRE TO WKS-APELL-PAT(WKS-INDICE)

               ELSE
               IF WKS-ELECCION = 3
                   DISPLAY "DAME EL NUEVO APELLIDO MATERNO"
                   ACCEPT WKS-AUX-NOMBRE
                   MOVE WKS-AUX-NOMBRE TO WKS-APELL-MAT(WKS-INDICE)

               ELSE
               IF WKS-ELECCION = 4
                   DISPLAY "DAME EL NUEVO SALARIO"
                   ACCEPT WKS-AUX-SALARIO
                   MOVE WKS-AUX-SALARIO TO WKS-SALARIO(WKS-INDICE)

               ELSE
               IF WKS-ELECCION = 5
                   MOVE 1 TO WKS-COL
                   PERFORM MODIFICA-DEPTO UNTIL WKS-COL > 3

               ELSE
               IF WKS-ELECCION = 6
                   NEXT SENTENCE
               ELSE
                   DISPLAY "MODIFICACION INVALIDA VERIFIQUE"
                   PERFORM MODIFICACIONES.



           MODIFICA-DEPTO.

           IF WKS-COL < 4
               DISPLAY "DAME UN NUEVO DEPARTAMENTO" WKS-COL ": "
               ACCEPT WKS-AUX-DEPTO
               MOVE WKS-AUX-DEPTO TO ,
                                  WKS-DEPARTAMENTO(WKS-INDICE, WKS-COL)
               ADD 1 TO WKS-COL
               PERFORM MODIFICA-OTRO-DEPTO
           ELSE
               DISPLAY "SOLO SE PUEDEN CAPTURAR 3 DEPARTAMENTOS ".

           MODIFICA-OTRO-DEPTO.
               DISPLAY "DESEAS AGREGAR OTRO DEPARTAMENTO? S-SI N-NO "
               ACCEPT WKS-OPCION
               IF WKS-OPC-S
                   PERFORM MODIFICA-DEPTO
               ELSE
               IF WKS-OPC-N
                   MOVE 4 TO WKS-COL
               ELSE
                   DISPLAY "OPCION INVALIDA VERIFIQUE"
                   PERFORM MODIFICA-OTRO-DEPTO.

           BAJA-TRABAJADOR.
               MOVE 2 TO WKS-INDICADOR.
               PERFORM CONSULTA-TRAB.

           BAJAS.
           DISPLAY "CONFIRMA QUE QUIERES DAR DE BAJA AL TRABAJADOR ID:",
                   WKS-ID-TRAB(WKS-INDICE) "? S-SI N-NO".
           ACCEPT WKS-OPCION.
           IF WKS-OPC-S
               MOVE LOW-VALUES TO WKS-DATOS-TRAB(WKS-INDICE)

           ELSE
           IF WKS-OPC-N

           ELSE
               DISPLAY "OPCION INVALIDA VERIFIQUE"
               PERFORM BAJAS.

       END PROGRAM NOMINA.
