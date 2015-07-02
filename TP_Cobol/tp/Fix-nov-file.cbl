       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP1.
       AUTHOR. UN GRUPO.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCH-NOV1 ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-NOV1.

           SELECT ARCH-NOV2-formata-fecha-bien ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-NOV2.

       DATA DIVISION.
       FILE SECTION.

       FD ARCH-NOV1 LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'NOV3.DAT'
             DATA RECORD IS REG-NOV1.

       01 REG-NOV1.
           03 REG-NOV1-CLAVE.
               05 REG-NOV1-NUMERO        PIC X(5).
               05 REG-NOV1-FECHA.
                    07   NOV1-DIA        PIC X(2).
                    07   NOV1-MES        PIC X(2).
                    07   NOV1-ANIO        PIC X(4).

           03 REG-NOV1-EMPRESA           PIC 9(3).
           03 REG-NOV1-TAREA             PIC X(4).
           03 REG-NOV1-HORAS             PIC 9(2)v99.

       FD ARCH-NOV2-formata-fecha-bien LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'NOV3-FIXED.DAT'
             DATA RECORD IS REG-NOV2.

       01 REG-NOV2.
           03 REG-NOV2-CLAVE.
               05 REG-NOV2-NUMERO        PIC X(5).
               05 REG-NOV2-FECHA.
                    07   NOV2-ANIO        PIC X(4).
                    07   NOV2-MES        PIC X(2).
                    07   NOV2-DIA        PIC X(2).
           03 REG-NOV2-EMPRESA           PIC 9(3).
           03 REG-NOV2-TAREA             PIC X(4).
           03 REG-NOV2-HORAS             PIC 9(2)v99.

       WORKING-STORAGE SECTION.


       77 FS-NOV1                    PIC XX.
           88 NOV1-OK                VALUE '00'.
           88 NOV1-EOF               VALUE '10'.

       77 FS-NOV2                    PIC XX.
           88 NOV2-OK                VALUE '00'.
           88 NOV2-EOF               VALUE '10'.

       PROCEDURE DIVISION.

       TP1.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM LEER-NOV1.
           PERFORM CARGAR-NOV-FIXED
               UNTIL NOV1-EOF.

           PERFORM FIN.

        ABRIR-ARCHIVOS.
           OPEN INPUT ARCH-NOV1.
           IF NOV1-OK THEN
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO NOV1 ' FS-NOV1
              PERFORM FIN.

           OPEN OUTPUT ARCH-NOV2-formata-fecha-bien.
           IF NOV2-OK THEN
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO ARCH-NOV2 ' FS-NOV2
              PERFORM FIN.



       LEER-NOV1.
           READ ARCH-NOV1 AT END MOVE '10' TO FS-NOV1.

       CARGAR-NOV-FIXED.
           MOVE REG-NOV1-NUMERO TO REG-NOV2-NUMERO.
           MOVE NOV1-DIA TO NOV2-DIA.
           MOVE NOV1-MES TO NOV2-MES.
           MOVE NOV1-ANIO TO NOV2-ANIO.
           MOVE REG-NOV1-EMPRESA TO REG-NOV2-EMPRESA.
           MOVE REG-NOV1-TAREA TO REG-NOV2-TAREA.
           MOVE REG-NOV1-HORAS TO REG-NOV2-HORAS.

           WRITE REG-NOV2.

           PERFORM LEER-NOV1.

       FIN.
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

       CERRAR-ARCHIVOS.
           CLOSE ARCH-NOV1.
           CLOSE ARCH-NOV2-formata-fecha-bien.
