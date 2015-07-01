      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PRINCIPAL.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.

       01  X   PIC 9(3).
       01  Y   PIC 9(4).
       01  Z   PIC 9(4).

       PROCEDURE DIVISION.

       MOVE 10 TO X.
       MOVE 2 TO Y.
       MOVE 0 TO Z.

       ADD X TO Z.
       ADD Y TO Z.

       DISPLAY 'DEBE DEVOLVER ' Z.

       CALL "SUB-PROGRAMA" USING
               BY REFERENCE X
               BY REFERENCE Y

       DISPLAY 'DEVOLVIO: ' Y.
