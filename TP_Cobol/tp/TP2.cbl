      ******************************************************************
      * Authors: Eisner - Ferreyra
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. TP2.
       AUTHOR. UN GRUPO.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARCH-PARAM ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-PARAM.

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
         FD ARCH-PARAM LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'PARAM.DAT'
             DATA RECORD IS REG-PARAM.

       01 REG-PARAM.
           03 REG-PARAM-CUIT-DESDE    PIC 9(11).
           03 REG-PARAM-CUIT-HASTA    PIC 9(11)v99.

      *-----------------------
       WORKING-STORAGE SECTION.

      * files status
       77 FS-PARAM                    PIC XX.
           88 PARAM-OK                VALUE '00'.
           88 PARAM-EOF               VALUE '10'.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
       PERFORM ABRIR_LST_PARAMETROS.
       PERFORM LEER_LST_PARAMETROS.
       CALL 'GENERO_LISTADO' USING
         BY CONTENT REG-PARAM-CUIT-DESDE
         BY CONTENT REG-PARAM-CUIT-HASTA

       PERFORM FIN.

      **
       ABRIR_LST_PARAMETROS.
           OPEN INPUT ARCH-PARAM.
           IF PARAM-OK THEN
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO PARAMS ' FS-PARAM
              PERFORM FIN.

       LEER_LST_PARAMETROS.
         READ ARCH-PARAM AT END MOVE '10' TO FS-PARAM.

       FIN.
         PERFORM CERRAR_LST_PARAMETROS.
         STOP RUN.

       CERRAR_LST_PARAMETROS.
         CLOSE ARCH-PARAM.

      **
       END PROGRAM TP2.
