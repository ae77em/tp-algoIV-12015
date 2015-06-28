       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP2.
       AUTHOR. Grupo-NO-C-Q-Nro.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          SELECT ARCH-PARAMETROS ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS FS-PARAMETROS.

          SELECT ARCH-TRABAJOS-X-EMPRESA ASSIGN TO DISK
              ORGANIZATION IS LINE SEQUENTIAL
              FILE STATUS IS FS-T-X-E.

          SELECT ARCH-TAR-IDX ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS REG-TAR-IND
              FILE STATUS IS FS-ARCH-TAR-IDX.

          SELECT ARCH-EMP-IDX ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS REG-EMP-IND-COD
              ALTERNATE RECORD KEY IS REG-EMP-IND-CUIT
              FILE STATUS IS FS-ARCH-EMP-IDX.

          SELECT ARCH-CONS-INDEXED ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS REG-KEY-CODE-CONS
              FILE STATUS IS FS-ARCH-CONS-INDEXED.

          SELECT ARCH-TIMES-IDX ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS REG-KEY-TIMES
              ALTERNATE RECORD KEY IS REG-KEY-CUIT WITH DUPLICATES
              FILE STATUS IS FS-ARCH-TIMES-IDX.
      
      DATA DIVISION.
      FILE SECTION.

      FD ARCH-PARAMETROS LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS 'PARAMETROS.DAT'
                 DATA RECORD IS REG-PARAMETROS.

      01 REG-PARAMETROS                 PIC 9(11).

      FD ARCH-TIMES-IDX LABEL RECORD IS STANDARD
                 VALUE OF FILE-ID IS 'TIMES-INDEXED.DAT'
                 DATA RECORD IS REG-TIMES-INDEXED.

      01 REG-TIMES-INDEXED.
          03 REG-KEY-TIMES. 
            05  REG-KEY-NUM             PIC X(5).
            05  REG-KEY-FECHA           PIC 9(8).
            05  REG-KEY-CUIT            PIC 9(11).
          03 REG-TIMES-TAREA              PIC X(4).
          03 REG-TIMES-HORAS              PIC 9(2)v99.

      FD ARCH-EMP-IDX LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'EMPRESAS-INDEXED.DAT'
             DATA RECORD IS REG-EMP-INDEXED.

      01 REG-EMP-INDEXED.
           03 REG-EMP-IND-COD           PIC 9(3).
           03 REG-EMP-IND-RAZON         PIC X(25).
           03 REG-EMP-IND-DIRECCION     PIC X(20).
           03 REG-EMP-IND-TEL           PIC X(20).
           03 REG-EMP-IND-CUIT          PIC 9(11).
       
      FD ARCH-CONS-INDEXED LABEL RECORD IS STANDARD
          VALUE OF FILE-ID IS 'CONSULTORES-INDEXED.DAT'
          DATA RECORD IS REG-CONSULTORES-INDEXED.

      01 REG-CONSULTORES-INDEXED.
          03 REG-KEY-CODE-CONS      PIC X(5).
          03 REG-CONS-DNI           PIC 9(8).
          03 REG-CONS-SRT           PIC X(2).
          03 REG-CONS-NOMBRE        PIC X(25).
          03 REG-CONS-DIRECCION     PIC X(20).
          03 REG-CONS-TELEFONO      PIC X(20).

      FD ARCH-TAR-IDX LABEL RECORD IS STANDARD
          VALUE OF FILE-ID IS 'TARIFAS-INDEXED.DAT'
          DATA RECORD IS REG-TAR-INDEXED.

      01 REG-TAR-INDEXED.
           03 REG-TAR-IND.
                05 REG-TAR-SRT-IND       PIC X(2).
              05 REG-TAR-VIG-DESDE-IND     PIC 9(8).
           03 REG-TAR-TARIFA-IND         PIC 9(5)V99.

      SD ARCH-TRABAJOS-X-EMPRESA
          DATA RECORD IS REG-T-X-E.

      01 REG-T-X-E.
          03 REG-T-X-E-RAZON              PIC X(25).
          03 REG-T-X-E-CUIT               PIC 9(11).
          03 REG-T-X-E-FECHA              PIC 9(8).
          03 REG-T-X-E-COD-CONS           PIC X(5).
          03 REG-T-X-E-HORAS              PIC 9(2)V99.

      WORKING-STORAGE SECTION.

      77 FS-PARAMETROS PIC XX.
          88 PARAMETROS-OK   VALUE '00'.
          88 PARAMETROS-NOENC  VALUE '23'.
          88 PARAMETROS-EOF  VALUE '10'.

      77 FS-T-X-E PIC XX.
          88 T-X-E-OK   VALUE '00'.
          88 T-X-E-NOENC  VALUE '23'.
          88 T-X-E-EOF  VALUE '10'.

      77 FS-ARCH-TIMES-IDX PIC XX.
          88 TIMES-OK   VALUE '00'.
          88 TIMES-NOENC  VALUE '23'.
          88 TIMES-EOF  VALUE '10'.


      77 FS-ARCH-EMP-IDX  PIC XX.
          88 EMP-OK   VALUE '00'.
          88 EMP-NOENC  VALUE '23'.
          88 EMP-EOF  VALUE '10'.

      77 FS-ARCH-TAR-IDX  PIC XX.
          88 TAR-OK   VALUE '00'.
          88 TAR-NOENC  VALUE '23'.
          88 TAR-EOF  VALUE '10'.       

      77 FS-ARCH-CONS-INDEXED PIC XX.
          88 CONS-OK   VALUE '00'.
          88 CONS-NOENC  VALUE '23'.
          88 CONS-EOF  VALUE '10'.

      01  WS-LETRA        PIC X.
      01  WS-CANT-REG     PIC 9(3).

      01  WS-MIN-CUIT     PIC 9(11).
      01  WS-MAX-CUIT     PIC 9(11).
      
      01 WS-COD-OPER                   PIC X.
      01 WS-CUIT                       PIC 9(11).
      01 WS-RAZON                      PIC X(25).
      01 WS-COD-ERROR                  PIC XX.
      01 ACUM		PIC 999 VALUE 000.

      PROCEDURE DIVISION.
       TP2.
           SORT ARCH-TRABAJOS-X-EMPRESA ON ASCENDING KEY REG-T-X-E-RAZON , REG-T-X-E-CUIT, REG-T-X-E-FECHA,REG-T-X-E-COD-CONS
                  INPUT PROCEDURE IS ENTRADA
                  OUTPUT PROCEDURE IS SALIDA.
           DISPLAY "Fin de la ejecucion total..".
           ACCEPT WS-LETRA.
           STOP RUN.
           
       ENTRADA SECTION.
       ARMAR-ENTRADA.
          PERFORM ABRIR-ARCHIVOS.
          PERFORM LEER-Y-SETEAR-PARAMETROS.
          PERFORM POSICIONAR-MAESTRO-TIMES.
          PERFORM LEER-MAESTRO-TIMES.
          
          DISPLAY "REGISTROS CARGADOS EN EL ARCHIVO PREVIO AL SORT".
          DISPLAY "PARAMETROS MIN: " WS-MIN-CUIT "  CLAVE: " REG-KEY-CUIT "   MAX:" WS-MAX-CUIT.
          
          PERFORM CARGAR-ARCHIVO UNTIL WS-MAX-CUIT IS LESS  REG-KEY-CUIT OR ACUM EQUAL 10.

          PERFORM CERRAR-ARCHIVOS.    
          DISPLAY "Fin INPUT PROCEDURE".
          DISPLAY " ".

      ABRIR-ARCHIVOS.
          OPEN INPUT ARCH-TIMES-IDX.
          OPEN INPUT ARCH-PARAMETROS.
          OPEN INPUT ARCH-EMP-IDX.

      LEER-Y-SETEAR-PARAMETROS.
          READ ARCH-PARAMETROS.
          MOVE REG-PARAMETROS TO WS-MIN-CUIT.

          READ ARCH-PARAMETROS.
          MOVE REG-PARAMETROS TO WS-MAX-CUIT.

      POSICIONAR-MAESTRO-TIMES.
          MOVE WS-MIN-CUIT TO REG-KEY-CUIT.
          DISPLAY "REG-KEY-CUIT "   REG-KEY-CUIT.
          START ARCH-TIMES-IDX KEY IS EQUAL REG-KEY-CUIT.

      LEER-MAESTRO-TIMES.
          READ ARCH-TIMES-IDX NEXT RECORD.      

      CARGAR-ARCHIVO.
      	  ADD 1 TO ACUM.
          MOVE REG-KEY-CUIT TO REG-T-X-E-CUIT.

          PERFORM BUSCAR-RAZON-SOCIAL.
          
          MOVE REG-EMP-IND-RAZON TO REG-T-X-E-RAZON.
          MOVE REG-KEY-FECHA TO REG-T-X-E-FECHA.
          MOVE REG-KEY-NUM TO REG-T-X-E-COD-CONS.
          MOVE REG-TIMES-HORAS TO REG-T-X-E-HORAS.
          
          DISPLAY REG-T-X-E.

          RELEASE REG-T-X-E.
          
          PERFORM LEER-MAESTRO-TIMES.

      BUSCAR-RAZON-SOCIAL.
          MOVE REG-KEY-CUIT TO REG-EMP-IND-CUIT.
          READ ARCH-EMP-IDX RECORD KEY IS REG-EMP-IND-CUIT.                          
          
      CERRAR-ARCHIVOS.
          CLOSE ARCH-TIMES-IDX.
          CLOSE ARCH-PARAMETROS.
          CLOSE ARCH-EMP-IDX.

       SALIDA SECTION.
       PROCESO-SALIDA.
           MOVE 0 TO WS-CANT-REG.

           PERFORM LEER-ARCH-TRABAJOS-X-EMPRESA.

           DISPLAY "\nREGISTROS CONTENIDOS EN EL ARCHIVO DEVUELTO POR LA FUNCION SORT, HACER CORTE DE CONTROL SOBRE ESTA DATA \n".

           PERFORM PROCESAR-LISTADO UNTIL FS-T-X-E NOT EQUAL '00'.

           DISPLAY "fin de OUTPUT PROCEDURE".

        LEER-ARCH-TRABAJOS-X-EMPRESA.
           RETURN ARCH-TRABAJOS-X-EMPRESA AT END SET T-X-E-EOF TO TRUE
           END-RETURN.
           
       PROCESAR-LISTADO.
           ADD 1 TO WS-CANT-REG.

           DISPLAY WS-CANT-REG ") "REG-T-X-E-RAZON " " REG-T-X-E-CUIT " " REG-T-X-E-FECHA " " REG-T-X-E-COD-CONS " " REG-T-X-E-HORAS.

           PERFORM LEER-ARCH-TRABAJOS-X-EMPRESA.

