        IDENTIFICATION DIVISION.
        PROGRAM-ID. CREAR-ARCHIVO-TIMES-MAESTRO.
        AUTHOR. Fede.

        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT TIMES-SEQ ASSIGN TO DISK
                ORGANIZATION IS LINE SEQUENTIAL
                FILE STATUS IS FS-TIMES-SEQ.
        
            SELECT TIMES-IDX ASSIGN TO DISK
                ORGANIZATION IS INDEXED
                ACCESS MODE IS SEQUENTIAL
                RECORD KEY IS REG-KEY-TIMES
                ALTERNATE RECORD KEY IS REG-KEY-CUIT WITH DUPLICATES
                FILE STATUS IS FS-TIMES-IDX.

              SELECT ARCH-TEMP-TIMES ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-TEMP-TIMES.

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

        
        FD TIMES-SEQ LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS 'TIMES.DAT'
                 DATA RECORD IS REG-TIMES-SEC.

           01 REG-TIMES-SEC.
           03 REG-TIMES-NUMERO              PIC X(5).
           03 REG-TIMES-FECHA               PIC 9(8).
           03 REG-TIMES-EMPRESA           PIC X(3).
           03 REG-TIMES-TAREA             PIC X(4).
           03 REG-TIMES-HORAS             PIC 9(2)v99.
        
        FD TIMES-IDX LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS 'TIMES-INDEXED.DAT'
                 DATA RECORD IS REG-TIMES-INDEXED.

        01 REG-TIMES-INDEXED.
            03 REG-KEY-TIMES.    
                05     REG-KEY-NUM               PIC X(5).
                05     REG-KEY-FECHA              PIC 9(8).
                05     REG-KEY-CUIT               PIC 9(11).
            03 REG-TIMES-IND-TAREA                 PIC X(4).
               03 REG-TIMES-IND-HORAS                 PIC 9(2)v99.
        
        SD ARCH-TEMP-TIMES
              DATA RECORD IS REG-TEMP-TIMES.

          01 REG-TEMP-TIMES.
              03 REG-TEMP-TIMES-NRO              PIC X(5).
              03 REG-TEMP-TIMES-FECHA              PIC 9(8).
              03 REG-TEMP-TIMES-CUIT               PIC 9(11).
              03 REG-TEMP-TIMES-TAREA              PIC X(4).
              03 REG-TEMP-TIMES-HORAS              PIC 9(2)V99.


        WORKING-STORAGE SECTION.

        01 WS-PADRON               PIC X.
        01 WS-ACUM-REG             PIC 9(6).
        
        01 WS-LETRA PIC X.
          77 FS-ARCH-EMP-IDX  PIC XX.
              88 EMP-OK   VALUE '00'.
              88 EMP-NOENC  VALUE '23'.
              88 EMP-EOF  VALUE '10'.


        77 FS-TIMES-SEQ    PIC XX.
            88 SEQ-OK        VALUE '00'.
            88 SEQ-NOENC    VALUE '23'.
            88 SEQ-EOF    VALUE '10'.

        77 FS-TEMP-TIMES    PIC XX.
            88 TEMP-OK        VALUE '00'.
            88 TEMP-NOENC    VALUE '23'.
            88 TEMP-EOF    VALUE '10'.

        77 FS-TIMES-IDX    PIC XX.
            88 IDX-OK        VALUE '00'.
            88 IDX-NOENC    VALUE '23'.
            88 IDX-EOF    VALUE '10'.
            
        PROCEDURE DIVISION.
        CREAR-ARCHIVO-TIMES-MAESTRO.
            SORT ARCH-TEMP-TIMES ON ASCENDING KEY REG-TEMP-TIMES-NRO, 
                                            REG-TEMP-TIMES-FECHA, 
                                            REG-TEMP-TIMES-CUIT
                  INPUT PROCEDURE IS ENTRADA
                  OUTPUT PROCEDURE IS SALIDA.
            DISPLAY "Fin de la ejecucion total..".

            DISPLAY 'INGRESE UN NRO PARA SALIR ' .
            ACCEPT WS-PADRON.
            STOP RUN.

            
        ENTRADA SECTION.
        ARMAR-ENTRADA.
             MOVE 0 TO WS-ACUM-REG.
            PERFORM ABRIR-ARCHIVOS.
            PERFORM LEER-SEQUENCIAL.
            PERFORM CARGAR-REG-SORT UNTIL SEQ-EOF.
            PERFORM CERRAR-ARCHIVOS.

            DISPLAY "SE CARGARON EN EL ARCHIVO SORT " WS-ACUM-REG " 
                        REGISTROS".
            DISPLAY "--------------Fin INPUT PROCEDURE".
            DISPLAY " ".

        AUXILIAR-SECTION.
        ABRIR-ARCHIVOS.
            OPEN INPUT TIMES-SEQ.
            IF SEQ-OK
               DISPLAY 'ARCHIVO SEQ ABIERTO CON EXITO!'
            ELSE
               DISPLAY 'NO PUDO ABRIRSE ARCHIVO SEQ ' FS-TIMES-SEQ.

            OPEN INPUT ARCH-EMP-IDX.

        CERRAR-ARCHIVOS.
            CLOSE TIMES-IDX.
            CLOSE TIMES-SEQ.
            CLOSE ARCH-EMP-IDX.

        LEER-SEQUENCIAL.
            READ TIMES-SEQ.

        CARGAR-REG-SORT.
            ADD 1 TO WS-ACUM-REG.
            MOVE REG-TIMES-NUMERO TO REG-TEMP-TIMES-NRO.
            MOVE REG-TIMES-FECHA TO REG-TEMP-TIMES-FECHA.
            MOVE REG-TIMES-TAREA TO REG-TEMP-TIMES-TAREA.
            MOVE REG-TIMES-HORAS TO REG-TEMP-TIMES-HORAS.

            PERFORM BUSCAR-CUIT.

            MOVE REG-EMP-IND-CUIT TO REG-TEMP-TIMES-CUIT.        
                  
            DISPLAY REG-TEMP-TIMES.   
            RELEASE REG-TEMP-TIMES.
              
            PERFORM LEER-SEQUENCIAL.

        BUSCAR-CUIT.
            MOVE REG-TIMES-EMPRESA TO REG-EMP-IND-COD.
            READ ARCH-EMP-IDX RECORD KEY IS REG-EMP-IND-COD.

        



        SALIDA SECTION.
        ARMAR-SALIDA.
            OPEN OUTPUT TIMES-IDX.
            IF IDX-OK
               DISPLAY 'ARCHIVO IDX CREADO CON EXITO!'
            ELSE
               DISPLAY 'NO PUDO CREARSE ARCHIVO IDX ' FS-TIMES-IDX.
            

            MOVE 0 TO WS-ACUM-REG.

            PERFORM LEER-ARCH-TEMP.
            DISPLAY "\nREGISTROS CONTENIDOS EN EL ARCHIVO DEVUELTO 
                    POR LA FUNCION SORT, HACER CORTE DE CONTROL 
                    SOBRE ESTA DATA \n".

            PERFORM CARGAR-INDEXADO UNTIL FS-TEMP-TIMES NOT EQUAL '00'.

            CLOSE TIMES-IDX.
            DISPLAY WS-ACUM-REG " REGISTRO GRABADOS ".
            DISPLAY "--------------Fin OUTPUT PROCEDURE".
            DISPLAY "     ".

        ULTIMA-SECTION.
        LEER-ARCH-TEMP.
            RETURN ARCH-TEMP-TIMES AT END SET TEMP-EOF TO TRUE   
            END-RETURN.

        CARGAR-INDEXADO.
            WRITE REG-TIMES-INDEXED FROM REG-TEMP-TIMES.
            ADD 1 TO WS-ACUM-REG.
            PERFORM LEER-ARCH-TEMP.
