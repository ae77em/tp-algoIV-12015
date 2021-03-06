       IDENTIFICATION DIVISION.
       PROGRAM-ID. CreaArchivoTarifasIndex.
       AUTHOR. Fede.

        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        SELECT ARCH-TAR-SEQ ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL
            FILE STATUS IS FS-ARCH-TAR-SEQ.

        SELECT ARCH-TAR-IDX ASSIGN TO DISK
            ORGANIZATION IS INDEXED
            ACCESS MODE IS SEQUENTIAL
            RECORD KEY IS REG-TAR-IND
            FILE STATUS IS FS-ARCH-TAR-IDX.

        DATA DIVISION.
        FILE SECTION.

        FD ARCH-TAR-SEQ
             VALUE OF FILE-ID IS 'TARIFAS.DAT'
             DATA RECORD IS REG-TAR-SEC.

       01 REG-TAR-SEC.
       03 REG-TAR-SRT           PIC X(2).
       03 REG-TAR-VIG-DESDE     PIC 9(8).
       03 REG-TAR-TARIFA         PIC 9(5)V99.

        FD ARCH-TAR-IDX LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'TARIFAS-INDEXED.DAT'
             DATA RECORD IS REG-TAR-INDEXED.

        01 REG-TAR-INDEXED.
       03 REG-TAR-IND.
            05 REG-TAR-SRT-IND              PIC X(2).
               05 REG-TAR-VIG-DESDE-IND     PIC 9(8).
       03 REG-TAR-TARIFA-IND                PIC 9(5)V99.

        WORKING-STORAGE SECTION.

        01 WS-LETRA   PIC X.
        01 WS-ACUM-REG PIC 99 value 00.

        77 FS-ARCH-TAR-SEQ PIC XX.
        88 SEQ-OK        VALUE '00'.
        88 SEQ-NOENC    VALUE '23'.
        88 SEQ-EOF    VALUE '10'.

        77 FS-ARCH-TAR-IDX    PIC XX.
        88 IDX-OK        VALUE '00'.
        88 IDX-NOENC    VALUE '23'.
        88 IDX-EOF    VALUE '10'.

        PROCEDURE DIVISION.

        CREAR-EMP-INDEXED.
        MOVE 0 TO WS-ACUM-REG.
        PERFORM ABRIR-ARCHIVOS.
        PERFORM LEER-SEQUENCIAL.
        PERFORM CARGAR-INDEXADO UNTIL SEQ-EOF.
        PERFORM TERMINAR.

        ABRIR-ARCHIVOS.
        OPEN OUTPUT ARCH-TAR-IDX.
        IF IDX-OK
           DISPLAY 'ARCHIVO IDX CREADO CON EXITO!'
        ELSE
           DISPLAY 'NO PUDO CREARSE ARCHIVO IDX ' FS-ARCH-TAR-IDX.

        OPEN INPUT ARCH-TAR-SEQ.
        IF SEQ-OK
           DISPLAY 'ARCHIVO SEQ ABIERTO CON EXITO!'
        ELSE
           DISPLAY 'NO PUDO ABRIRSE ARCHIVO SEQ ' FS-ARCH-TAR-SEQ.

        LEER-SEQUENCIAL.
	        READ ARCH-TAR-SEQ AT END MOVE '10' TO FS-ARCH-TAR-SEQ.

        CARGAR-INDEXADO.
        MOVE REG-TAR-SEC TO REG-TAR-INDEXED.
        WRITE REG-TAR-INDEXED.

        PERFORM LEER-SEQUENCIAL.


        TERMINAR.
        CLOSE ARCH-TAR-IDX.
        CLOSE ARCH-TAR-SEQ.
        DISPLAY 'SE ESCREIBIERON ' WS-ACUM-REG '  REGISTROS INDEXADOS'.
        DISPLAY 'INGRESE UN NRO PARA SALIR ' .
        ACCEPT WS-LETRA.
        STOP RUN.
