       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPRESAS.
       AUTHOR. Grupo-NO-C-Q-Nro.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
          FILE-CONTROL.
              SELECT ARCH-EMP-IDX ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS REG-EMP-IND-COD
              ALTERNATE RECORD KEY IS REG-EMP-IND-CUIT
              FILE STATUS IS FS-ARCH-EMP-IDX.

          DATA DIVISION.
          FILE SECTION.
            
          FD ARCH-EMP-IDX LABEL RECORD IS STANDARD
                     VALUE OF FILE-ID IS 'EMPRESAS-INDEXED.DAT'
                     DATA RECORD IS REG-EMP-INDEXED.

          01 REG-EMP-INDEXED.
               03 REG-EMP-IND-COD           PIC 9(3).
               03 REG-EMP-IND-RAZON         PIC X(25).
               03 REG-EMP-IND-DIRECCION     PIC X(20).
               03 REG-EMP-IND-TEL           PIC X(20).
               03 REG-EMP-IND-CUIT          PIC 9(11).

       WORKING-STORAGE SECTION.

       77 FS-ARCH-EMP-IDX       PIC XX.
           88 OK                VALUE '00'.
           88 EOF               VALUE '10'.

       LINKAGE SECTION.
       01 COD-OPER                   PIC X.
       01 CUIT                       PIC 9(11).
       01 RAZON-SOCIAL               PIC X(25).
       01 COD-ERROR                  PIC XX.

       PROCEDURE DIVISION USING COD-OPER, CUIT, RAZON-SOCIAL, COD-ERROR.
       PRINCIPAL.
           MOVE SPACES TO COD-ERROR.
           IF COD-OPER = 'O'
              PERFORM ABRIR.
           IF COD-OPER = 'C'
              PERFORM CERRAR.
           IF COD-OPER = 'R'
              PERFORM LEER.
           EXIT PROGRAM.

       ABRIR.
           OPEN INPUT ARCH-EMP-IDX.
           IF OK
              MOVE 'OK' TO COD-ERROR
           ELSE
              MOVE 'XX' TO COD-ERROR.

       CERRAR.
           CLOSE ARCH-EMP-IDX.

       LEER.
           MOVE CUIT TO REG-EMP-IND-CUIT.
           READ ARCH-EMP-IDX RECORD KEY IS REG-EMP-IND-CUIT.
           IF OK
              MOVE 'OK' TO COD-ERROR
              MOVE REG-EMP-IND-RAZON TO RAZON-SOCIAL
           ELSE
              MOVE 'ZZ' TO COD-ERROR.
