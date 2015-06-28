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
           SELECT ARCH-NOV2 ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-NOV2.
           SELECT ARCH-NOV3 ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-NOV3.
           SELECT ARCH-TIMES ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-TIMES.
           SELECT ARCH-CONSULTORES ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-CONS.
           SELECT ARCH-EMPRESAS ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-EMP.
           SELECT ARCH-CATEGORIAS ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-CAT.

       DATA DIVISION.

       FILE SECTION.
       FD ARCH-NOV1 LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'NOV1.DAT'
             DATA RECORD IS REG-NOV1.

       01 REG-NOV1.
           03 REG-NOV1-CLAVE.
               05 REG-NOV1-NUMERO        PIC X(5).
               05 REG-NOV1-FECHA         PIC 9(8).
           03 REG-NOV1-EMPRESA           PIC 9(3).
           03 REG-NOV1-TAREA             PIC X(4).
           03 REG-NOV1-HORAS             PIC 9(2)v99.

       FD ARCH-NOV2 LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'NOV2.DAT'
             DATA RECORD IS REG-NOV2.

       01 REG-NOV2.
           03 REG-NOV2-CLAVE.
               05 REG-NOV2-NUMERO        PIC X(5).
               05 REG-NOV2-FECHA         PIC 9(8).
           03 REG-NOV2-EMPRESA           PIC 9(3).
           03 REG-NOV2-TAREA             PIC X(4).
           03 REG-NOV2-HORAS             PIC 9(2)v99.

       FD ARCH-NOV3 LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'NOV3.DAT'
             DATA RECORD IS REG-NOV3.

       01 REG-NOV3.
           03 REG-NOV3-CLAVE.
               05 REG-NOV3-NUMERO        PIC X(5).
               05 REG-NOV3-FECHA         PIC 9(8).
           03 REG-NOV3-EMPRESA           PIC 9(3).
           03 REG-NOV3-TAREA             PIC X(4).
           03 REG-NOV3-HORAS             PIC 9(2)v99.

       FD ARCH-TIMES LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'TIMES.DAT'
             DATA RECORD IS REG-TIMES.

       01 REG-TIMES.
           03 REG-TIMES-CLAVE.
               05 REG-TIMES-NUMERO        PIC X(5).
               05 REG-TIMES-FECHA         PIC 9(8).
           03 REG-TIMES-EMPRESA           PIC 9(3).
           03 REG-TIMES-TAREA             PIC X(4).
           03 REG-TIMES-HORAS             PIC 9(2)v99.

       FD ARCH-CONSULTORES LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'CONSULTORES.DAT'
             DATA RECORD IS REG-CONS.

       01 REG-CONS.
           03 REG-CONS-NUMERO        PIC X(5).
           03 REG-CONS-DNI           PIC 9(8).
           03 REG-CONS-SRT           PIC X(2).
           03 REG-CONS-NOMBRE        PIC X(25).
           03 REG-CONS-DIRECCION     PIC X(20).
           03 REG-CONS-TELEFONO      PIC X(20).

       FD ARCH-EMPRESAS LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'EMPRESAS.DAT'
             DATA RECORD IS REG-EMP.

       01 REG-EMP.
           03 REG-EMP-TAB.
              05 REG-EMP-COD           PIC 9(3).
              05 REG-EMP-RAZON         PIC X(25).
           03 REG-EMP-DIRECCION      PIC X(20).
           03 REG-EMP-TEL            PIC X(20).
           03 REG-EMP-CUIT          PIC 9(11).

       FD ARCH-CATEGORIAS LABEL RECORD IS STANDARD
             VALUE OF FILE-ID IS 'CATEGORIAS.DAT'
             DATA RECORD IS REG-CAT.

       01 REG-CAT.
           03 REG-CAT-SRT           PIC X(2).
           03 REG-CAT-DESC          PIC X(20).
           03 REG-CAT-TARIFA        PIC 9(5)V99.

       WORKING-STORAGE SECTION.
      ******************************************************************
      * TABLAS
      ******************************************************************

       01 REG-TAB-EMP-TEMP.
         05 TAB-EMP-COD-TEMP           PIC 9(3).
         05 TAB-EMP-RAZON-TEMP         PIC X(25).

       01 TABLA-EMPRESAS.
           03 TAB-EMP OCCURS 999 TIMES INDEXED BY IND-TAB-EMP.
               05 TAB-EMP-COD           PIC 9(3).
               05 TAB-EMP-RAZON         PIC X(25).

       01 TABLA-CATEGORIAS.
           03 TAB-CAT OCCURS 30 TIMES INDEXED BY IND-TAB-CAT.
               05 TAB-CAT-SRT           PIC X(2).
               05 TAB-CAT-DES           PIC X(20).
               05 TAB-CAT-TARIFA        PIC 9(5)V99.

       01 REG-MEN.
           03 REG-MEN-CLAVE.
               05 REG-MEN-NUMERO        PIC X(5) VALUE "00000".
               05 REG-MEN-FECHA.
                  07 REG-MEN-DD         PIC 9(2).
                  07 REG-MEN-MM         PIC 9(2).
                  07 REG-MEN-AAAA       PIC 9(4).
           03 REG-MEN-EMPRESA           PIC 9(3).
           03 REG-MEN-TAREA             PIC X(4).
           03 REG-MEN-HORAS             PIC 9(2)v99.



      ******************************************************************
      * REGISTROS DE ARCHIVOS
      ******************************************************************
       77 FS-NOV1                    PIC XX.
           88 NOV1-OK                VALUE '00'.
           88 NOV1-EOF               VALUE '10'.
       77 FS-NOV2                    PIC XX.
           88 NOV2-OK                VALUE '00'.
           88 NOV2-EOF               VALUE '10'.
       77 FS-NOV3                    PIC XX.
           88 NOV3-OK                VALUE '00'.
           88 NOV3-EOF               VALUE '10'.

       77 FS-TIMES                   PIC XX.
           88 TIM-OK                 VALUE '00'.
           88 TIM-EOF                VALUE '10'.
           88 TIM-ABIERTO            VALUE '41'.

       77 FS-EMP                     PIC XX.
           88 EMP-OK                 VALUE '00'.
           88 EMP-EOF                VALUE '10'.

       77 FS-CAT                     PIC XX.
           88 CAT-OK                 VALUE '00'.
           88 CAT-EOF                VALUE '10'.

       77 FS-CONS                     PIC XX.
           88 CONS-OK                 VALUE '00'.
           88 CONS-EOF                VALUE '10'.


      ******************************************************************
      * FLAGS, ACUMULADORES, VARIABLES, ETC...
      ******************************************************************
       01 WS-ACUM-IMP-CONS-FECHA     PIC 9(8)v99.
       01 WS-ACUM-IMP-CONS           PIC 9(8)v99.
       01 WS-ACUM-HOR-CONS           PIC 9(8)v99.
       01 WS-ACUM-IMP-GEN            PIC 9(8)v99.

       01 WS-ACUM-NRO-HOJAS          PIC 9(3).
       01 WS-ACUM-LINEAS             PIC 9(4).
       01 WS-I                       PIC 9(4).
       01 WS-J                       PIC 9(4).
       01 WS-CANT-EMP                PIC 9(4).

       01 WS-CORTE-CONS-NUM          PIC X(5).
       01 WS-CORTE-CONS-FECHA        PIC 9(8).

       01 WS-CAT-DES                 PIC X(20).
       01 WS-CAT-TARIFA              PIC 9(5)V99.

       01 WS-RAZON-SOCIAL            PIC X(25).

       01 WS-ACUM-HORAS-X-FECHA     PIC 9(3)V99 VALUE 000,00.

       01 WS-ACUM-HORAS-X-CONS      PIC 9(3)V99 VALUE 000,00.
       01 WS-ACUM-IMPORTE-X-CONS    PIC 9(6)V99 VALUE 000000,00.

       01 WS-IMPORTE-UN-TRABAJO     PIC 9(6)V99 VALUE 000000,00.
       01 WS-ACUM-IMP-X-FECHA       PIC 9(6)V99 VALUE 000000,00.
       01 WS-ACUM-IMP-X-CONS        PIC 9(10)V99 VALUE 0000000000,00.

       01 WS-ACUM-IMP-TOTAL         PIC 9(10)V99 VALUE 0000000000,00.

       01 ACUM PIC 99 value 00.

       01 WS-TECLA PIC X.

       01 FECHA.
          03 FECHA-DD     PIC 99.
          03 FECHA-MM     PIC 99.
          03 FECHA-AAAA   PIC 9999.

       01 WS-FECHA-TAB               PIC X(10).

       01 FILLER REDEFINES WS-FECHA-TAB.
           03 WS-ANIO-TAB           PIC 9999.
           03 FILLER                PIC X.
           03 WS-MES-TAB            PIC 99.
           03 FILLER                PIC X.
           03 WS-DIA-TAB            PIC 99.

       01 WS-ANIO-ACTUAL            PIC 9999.

       01 WS-ANIO-AAAA              PIC 9999.
       01 WS-MES-MM                 PIC 99.
       01 WS-EMPRESA                PIC X(88).
       01 WS-ANIO-TEMP              PIC X(2).
         88 WS-ANIO-TEMP-OK         VALUE 'OK'.
         88 WS-ANIO-TEMP-MAL        VALUE 'NO'.

       01 WS-OFFSET-MES             PIC 9(2).
         88 WS-OFFSET-MES-MAL       VALUE 99.

       01 WS-OFFSET-ANIO-EST        PIC 99.
         88 WS-OFFSET-ANIO-EST-MAL  VALUE 99.

      * es el offset a la ultima fila del reporte de estadisticas
      * que contiene el total del año correspondiente a la fila
       01 WS-OFFSET-TOT-MES         PIC 9 VALUE 6.

      * es el offset a la última columna del reporte de estadisticas,
      * que contiene el total de los ultimos 5 años de cada mes
       01 WS-OFFSET-TOT-ANIO        PIC 999 VALUE 79.

      ******************************************************************
      * ENCABEZADOS REPORTES
      ******************************************************************

      * REPORTE HORAS APLICADAS POR CONSULTOR
       01 ENCABE-LINEA2.
           03 FILLER       PIC X(28) VALUE SPACES.
           03 FILLER       PIC X(30) VALUE 'Listado de horas aplicadas'.

       01 ENCABE-CONSULTOR-LINEA1.
           03 FILLER       PIC X(11) VALUE 'Consultor: '.
           03 ENCABE-CONSULTOR-LINEA1-NUMERO   PIC X(5).
           03 FILLER       PIC X(17) VALUE SPACES.
           03 FILLER       PIC X(8) VALUE 'Nombre: '.
           03 ENCABE-CONSULTOR-LINEA1-NOMBRE   PIC X(25).

       01 ENCABE-CONSULTOR-LINEA2.
           03 FILLER       PIC X(30) VALUE SPACES.
           03 FILLER       PIC X(11) VALUE 'Categoria:'.
           03 ENCABE-CONSULTOR-LINEA2-CAT      PIC X(20).

       01 ENCABE-CONSULTOR-LINEA3.
           03 FILLER       PIC X(33) VALUE SPACES.
           03 FILLER       PIC X(8) VALUE "Tarifa: ".
           03 ENCABE-CONSULTOR-LINEA3-TARIFA   PIC 9(4)V99.

       01 ENCABE-CONSULTOR-POR-FECHA.
           03 FILLER       PIC X(13) VALUE "   Fecha     ".
           03 FILLER       PIC X(13) VALUE "   Empresa  ".
           03 FILLER       PIC X(28) VALUE "       Razon Social      ".
           03 FILLER       PIC X(11) VALUE "   Horas   ".
           03 FILLER       PIC X(13) VALUE "     Importe".

       01 LINEA-DATOS-POR-TRABAJO.
           03 FILLER              PIC XX VALUE SPACES.
           03 LIN-DIA             PIC XX.
           03 FILLER              PIC X VALUE '/'.
           03 LIN-MES             PIC XX.
           03 FILLER              PIC X VALUE '/'.
           03 LIN-ANIO            PIC XXXX.
           03 FILLER              PIC X(6) VALUE ALL SPACES.
           03 LIN-COD-EMP         PIC 9(3).
           03 FILLER              PIC X(4) VALUE ALL SPACES.
           03 LIN-RAZON           PIC X(25).
           03 FILLER              PIC X(7) VALUE ALL SPACES.
           03 LIN-HORAS           PIC 9(2)V99.
           03 FILLER              PIC X(10) VALUE ALL SPACES.
           03 LIN-IMPORTE         PIC 9(3)V99.

       01 LINEA-DATOS-POR-FECHA.
           03 FILLER              PIC X(17) VALUE "Totales por fecha".
           03 FILLER              PIC X(39) VALUE ALL SPACES.
           03 LIN-TOTAL-HORAS-X-FECHA     PIC 9(3)V99 VALUE 000,00.
           03 FILLER              PIC X(7) VALUE ALL SPACES.
           03 LIN-TOTAL-IMPORTE-X-FECHA   PIC 9(6)V99 VALUE 00000000,00.

       01 LINEA-DATOS-POR-CONS.
           03 FILLER           PIC X(21) VALUE "Totales por Consultor".
           03 FILLER           PIC X(35) VALUE ALL SPACES.
           03 LIN-TOTAL-HORAS-X-CONS      PIC 9(3)V99 VALUE 000,00.
           03 FILLER           PIC X(5) VALUE ALL SPACES.
           03 LIN-TOTAL-IMPORTE-X-CONS    PIC 9(8)V99 VALUE 00000000,00.

       01 LINEA-IMPORTE-TOTAL-CONS.
           03 FILLER           PIC X(21) VALUE "Total por General".
           03 FILLER           PIC X(45) VALUE ALL SPACES.
           03 LIN-TOTAL-GENERAL-CONS      PIC 9(8)V99 VALUE 00000000,00.

      *  REPORTE HORAS APLICADAS EMPRESA EN LOS ULTIMOS 5 AÑOS
       01 ENCABE-LINEA2-EST.
           03 FILLER       PIC X(10) VALUE SPACES.
           03 FILLER       PIC X(20) VALUE 'Listado Estadistico'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(19) VALUE 'de horas aplicadas'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(14) VALUE 'por año y mes'.

       01 ENCABE-LISTADO-EST.
           03 FILLER       PIC X(4) VALUE SPACES.
           03 FILLER       PIC X(7) VALUE 'Empresa'.
           03 FILLER       PIC X(4) VALUE SPACES.
           03 FILLER       PIC X(4) VALUE 'Año'.
           03 FILLER       PIC X(4) VALUE SPACES.
           03 FILLER       PIC X(3) VALUE 'Ene'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Feb'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Mar'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Abr'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'May'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Jun'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Jul'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Ago'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Sep'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Oct'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Nov'.
           03 FILLER       PIC X(1) VALUE SPACE.
           03 FILLER       PIC X(3) VALUE 'Dic'.
           03 FILLER       PIC X(4) VALUE SPACES.
           03 FILLER       PIC X(5) VALUE 'Total'.

       01 TOTAL-GLOBAL-EST.
           03 FILLER           PIC X(4) VALUE SPACES.
           03 FILLER           PIC X(7) VALUE 'Totales'.
           03 FILLER           PIC X(11) VALUE SPACES.
           03 TOTAL-EST-ENE    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-FEB    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-MAR    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-ABR    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-MAY    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-JUN    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-JUL    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-AGO    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-SEP    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-OCT    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-NOV    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(1) VALUE SPACE.
           03 TOTAL-EST-DIC    PIC 9(3) VALUE 0.
           03 FILLER           PIC X(5) VALUE SPACES.
           03 TOTAL-EST-TOT    PIC 9(4) VALUE 0.

       01 TABLA-DETALLE-ESTA.
         03 LINEA-LISTADO-EST OCCURS 6 TIMES INDEXED BY I.
           05 LINEA-EST-COL-EMPRESA    PIC X(13) VALUE SPACES.
           05 FILLER               PIC X(2) VALUE SPACES.
           05 LINEA-EST-COL-ANIO       PIC X(4) VALUE SPACES.
           05 FILLER               PIC X(3) VALUE SPACES.
           05 LINEA-EST-COL-MESES.
               07 LINEA-EST-COL-ENE        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-FEB        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-MAR        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-ABR        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-MAY        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-JUN        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-JUL        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-AGO        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-SEP        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-OCT        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-NOV        PIC 9(3) VALUE 0.
               07 FILLER               PIC X(1) VALUE SPACE.
               07 LINEA-EST-COL-DIC        PIC 9(3) VALUE 0.
           05 FILLER               PIC X(5) VALUE SPACES.
           05 LINEA-EST-COL-TOT        PIC 9(4) VALUE 0.

      * COMUNES
       01 LINEA-DIVISORIA-PUNTEADA.
         03 FILLER PIC X(80) VALUE ALL "-".

       01 LINEA-DIVISORIA-CONTINUA.
         03 FILLER PIC X(80) VALUE ALL "_".

       01 LINEA-EN-BLANCO.
         03 FILLER PIC X(80) VALUE ALL SPACES.

       01 ENCABE-LINEA1.
           03 FILLER                   PIC X(8) VALUE ' Fecha: '.
           03 ENCABE-LINEA1-FECHA-DD   PIC XX.
           03 FILLER                   PIC X VALUE '/'.
           03 ENCABE-LINEA1-FECHA-MM   PIC XX.
           03 FILLER                   PIC X VALUE '/'.
           03 ENCABE-LINEA1-FECHA-AAAA PIC XXXX.
           03 FILLER                   PIC X(51) VALUE SPACES.
           03 FILLER                   PIC X(7) VALUE 'HOJA : '.
           03 ENCABE-LINEA1-NRO-HOJA   PIC 999 VALUE ZERO.

       PROCEDURE DIVISION.

       TP1.
           PERFORM INICIO.
           PERFORM LEER-NOVEDADES.

      *    actualizo el maestro de horas (archivo TIMES)
           PERFORM CARGAR-MAESTRO
               UNTIL NOV1-EOF AND NOV2-EOF AND NOV3-EOF.

           PERFORM ESTADISTICAS-X-EMPRESA.

           PERFORM FIN.

       INICIO.
           PERFORM INICIALIZACION-VARIABLES.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM CARGAR-TABLA-CATEGORIAS.
           PERFORM CARGAR-TABLA-EMPRESAS.

       LEER-NOVEDADES.
           PERFORM LEER-NOV1.
           PERFORM LEER-NOV2.
           PERFORM LEER-NOV3.

       LEER-NOV1.
           READ ARCH-NOV1 AT END MOVE '10' TO FS-NOV1.

           IF NOV1-OK
              NEXT SENTENCE
           ELSE
              MOVE "99999" TO REG-NOV1-NUMERO
              MOVE "99999999" TO REG-NOV1-FECHA.

       LEER-NOV2.
           READ ARCH-NOV2 AT END MOVE '10' TO FS-NOV2.

           IF NOV2-OK
              NEXT SENTENCE
           ELSE
              MOVE "99999" TO REG-NOV2-NUMERO
              MOVE "99999999" TO REG-NOV2-FECHA.

       LEER-NOV3.
           READ ARCH-NOV3 AT END MOVE '10' TO FS-NOV3.

           IF NOV3-OK
              NEXT SENTENCE
           ELSE
              MOVE "99999" TO REG-NOV3-NUMERO
              MOVE "99999999" TO REG-NOV3-FECHA.

       CARGAR-MAESTRO.
           PERFORM DETERMINAR-MENOR.
           PERFORM BUSCAR-DATOS-CONSULTOR.

           PERFORM IMPRIMIR-ENCABEZADO-GENERAL.
           PERFORM CARGA-E-IMP-ENCABE-CONS.

           PERFORM ASIGNO-CORTE-CONS-NUM.

      *    calculo las horas que fueron aplicadas  por cada consultor.
           PERFORM PROCESAR-CONSULTOR
               UNTIL (NOV1-EOF AND NOV2-EOF AND NOV3-EOF)
                OR REG-MEN-NUMERO NOT EQUAL WS-CORTE-CONS-NUM.

           PERFORM CARGAR-IMP-DATOS-X-CONS.

           PERFORM ACUM-IMPORTE-GENERAL-CONS.

           PERFORM CARGAR-IMP-GENERAL-IMPORTE.

           PERFORM RESET-DATOS-CONS.

           PERFORM INCREMENTAR-NRO-HOJA.

       ACUM-IMPORTE-GENERAL-CONS.
           ADD WS-ACUM-IMP-X-CONS TO WS-ACUM-IMP-TOTAL.

       CARGAR-IMP-GENERAL-IMPORTE.
           MOVE WS-ACUM-IMP-TOTAL TO LIN-TOTAL-GENERAL-CONS.

           DISPLAY " ".
           DISPLAY LINEA-IMPORTE-TOTAL-CONS.

           ACCEPT WS-TECLA.

       CARGAR-IMP-DATOS-X-CONS.
          MOVE WS-ACUM-IMP-X-CONS TO LIN-TOTAL-IMPORTE-X-CONS.
          MOVE WS-ACUM-HORAS-X-CONS TO LIN-TOTAL-HORAS-X-CONS.

          DISPLAY LINEA-DATOS-POR-CONS.

       RESET-DATOS-CONS.
          MOVE 0 TO WS-ACUM-IMP-X-CONS.
          MOVE 0 TO WS-ACUM-HORAS-X-CONS.

       BUSCAR-TARIFA-CONSULTOR.
           SET IND-TAB-CAT TO 1.

           SEARCH TAB-CAT
               WHEN TAB-CAT-SRT(IND-TAB-CAT) EQUAL REG-CONS-SRT
                   PERFORM OBTENER-DATOS-CAT-CONSULTOR
           END-SEARCH.

       OBTENER-DATOS-CAT-CONSULTOR.
           MOVE TAB-CAT-DES(IND-TAB-CAT) TO WS-CAT-DES.
           MOVE TAB-CAT-TARIFA(IND-TAB-CAT) TO WS-CAT-TARIFA.

       ASIGNO-CORTE-CONS-NUM.
           MOVE REG-MEN-NUMERO TO WS-CORTE-CONS-NUM.

       ASIGNO-CORTE-CONS-FECHA.
           MOVE REG-MEN-FECHA TO WS-CORTE-CONS-FECHA.

       PROCESAR-CONSULTOR.
           PERFORM IMPRIMIR-ENCABEZADO-POR-FECHA.

           PERFORM ASIGNO-CORTE-CONS-FECHA.

           PERFORM PROCESAR-HORAS-POR-FECHA
               UNTIL (NOV1-EOF AND NOV2-EOF AND NOV3-EOF)
                OR REG-MEN-FECHA NOT EQUAL WS-CORTE-CONS-FECHA

           PERFORM CARGAR-IMPRIMIR-TOTALES-X-FECHA.

           PERFORM ACUM-DATOS-X-CONS.

           PERFORM RESET-DATOS-POR-FECHA.

       ACUM-DATOS-X-CONS.
           ADD WS-ACUM-IMP-X-FECHA TO WS-ACUM-IMP-X-CONS.
           ADD WS-ACUM-HORAS-X-FECHA TO WS-ACUM-HORAS-X-CONS.

       CARGAR-IMPRIMIR-TOTALES-X-FECHA.
           MOVE WS-ACUM-HORAS-X-FECHA TO LIN-TOTAL-HORAS-X-FECHA.
           MOVE WS-ACUM-IMP-X-FECHA TO LIN-TOTAL-IMPORTE-X-FECHA.

           DISPLAY " ".
           DISPLAY LINEA-DATOS-POR-FECHA.
           DISPLAY " ".

       PROCESAR-HORAS-POR-FECHA.
           PERFORM CALCULAR-IMPORTE-UN-TRABAJO.
           PERFORM BUSCAR-RAZON-SOCIAL.
           PERFORM CARGAR-E-IMPRIMIR-LINEA-FECHA.
           PERFORM ACUM-DATOS-POR-FECHA.
           PERFORM CARGAR-TIMES-MAESTRO.

           ADD 1 TO ACUM.
           PERFORM LEER-MENOR.

       CARGAR-TIMES-MAESTRO.
           MOVE REG-MEN TO REG-TIMES.
           WRITE REG-TIMES.

       BUSCAR-RAZON-SOCIAL.
           SET IND-TAB-EMP TO 1.

           SEARCH TAB-EMP
               WHEN TAB-EMP-COD(IND-TAB-EMP) EQUAL REG-MEN-EMPRESA
                   PERFORM OBTENER-RAZON-SOCIAL
           END-SEARCH.

       OBTENER-RAZON-SOCIAL.
           MOVE TAB-EMP-RAZON(IND-TAB-EMP) TO WS-RAZON-SOCIAL.

       CALCULAR-IMPORTE-UN-TRABAJO.
           COMPUTE WS-IMPORTE-UN-TRABAJO
                           = (REG-MEN-HORAS * WS-CAT-TARIFA).

       CARGAR-E-IMPRIMIR-LINEA-FECHA.
           MOVE REG-MEN-DD TO LIN-DIA.
           MOVE REG-MEN-MM TO LIN-MES.
           MOVE REG-MEN-AAAA TO LIN-ANIO.

           MOVE REG-MEN-EMPRESA TO LIN-COD-EMP.
           MOVE REG-MEN-HORAS TO LIN-HORAS.
           MOVE WS-IMPORTE-UN-TRABAJO TO LIN-IMPORTE.

           MOVE WS-RAZON-SOCIAL TO LIN-RAZON.

           DISPLAY LINEA-DATOS-POR-TRABAJO.

       ACUM-DATOS-POR-FECHA.
           ADD REG-MEN-HORAS TO WS-ACUM-HORAS-X-FECHA.
           ADD WS-IMPORTE-UN-TRABAJO TO WS-ACUM-IMP-X-FECHA.

       RESET-DATOS-POR-FECHA.
           MOVE 0 TO WS-ACUM-HORAS-X-FECHA.
           MOVE 0 TO WS-ACUM-IMP-X-FECHA.

       DETERMINAR-MENOR.
           MOVE REG-NOV1 TO REG-MEN.

           IF REG-MEN-NUMERO IS GREATER THAN REG-NOV2-NUMERO
                MOVE REG-NOV2 TO REG-MEN.

           IF REG-MEN-NUMERO IS GREATER THAN REG-NOV3-NUMERO
                MOVE REG-NOV3 TO REG-MEN.

       LEER-MENOR.
           IF REG-MEN-NUMERO IS EQUAL REG-NOV1-NUMERO
               AND NOT NOV1-EOF THEN
                PERFORM LEER-NOV1
                MOVE REG-NOV1 TO REG-MEN.

           IF REG-MEN-NUMERO IS EQUAL REG-NOV2-NUMERO
               AND NOT NOV2-EOF THEN
                PERFORM LEER-NOV2
                MOVE REG-NOV2 TO REG-MEN.

           IF REG-MEN-NUMERO IS EQUAL REG-NOV3-NUMERO
               AND NOT NOV3-EOF THEN
                PERFORM LEER-NOV3
                MOVE REG-NOV3 TO REG-MEN.

       BUSCAR-DATOS-CONSULTOR.
           PERFORM LEER-ARCH-CONSULTORES.
           PERFORM LEER-ARCH-CONSULTORES
               UNTIL REG-CONS-NUMERO EQUAL REG-MEN-NUMERO.

           PERFORM BUSCAR-TARIFA-CONSULTOR.

       LEER-ARCH-CONSULTORES.
           READ ARCH-CONSULTORES
               AT END MOVE '10' TO FS-CONS.

       IMPRIMIR-ENCABEZADO-GENERAL.
           PERFORM CARGAR-E-IMPRIMIR-PRIMERA-LINEA.

       INCREMENTAR-NRO-HOJA.
           ADD 1 TO WS-ACUM-NRO-HOJAS.

       CARGAR-E-IMPRIMIR-PRIMERA-LINEA.
           MOVE FECHA-AAAA TO ENCABE-LINEA1-FECHA-AAAA.
           MOVE FECHA-MM TO ENCABE-LINEA1-FECHA-MM.
           MOVE FECHA-DD TO ENCABE-LINEA1-FECHA-DD.
           MOVE WS-ACUM-NRO-HOJAS TO ENCABE-LINEA1-NRO-HOJA.

           DISPLAY ENCABE-LINEA1.
           DISPLAY ENCABE-LINEA2.

           DISPLAY " ".

       CARGA-E-IMP-ENCABE-CONS.
          MOVE REG-CONS-NUMERO TO ENCABE-CONSULTOR-LINEA1-NUMERO.
          MOVE REG-CONS-NOMBRE TO ENCABE-CONSULTOR-LINEA1-NOMBRE.
          DISPLAY ENCABE-CONSULTOR-LINEA1.

          MOVE WS-CAT-DES TO ENCABE-CONSULTOR-LINEA2-CAT.
          DISPLAY ENCABE-CONSULTOR-LINEA2.

          MOVE WS-CAT-TARIFA TO ENCABE-CONSULTOR-LINEA3-TARIFA.
          DISPLAY ENCABE-CONSULTOR-LINEA3.

          DISPLAY " ".

       IMPRIMIR-ENCABEZADO-POR-FECHA.
          DISPLAY ENCABE-CONSULTOR-POR-FECHA.
          DISPLAY LINEA-DIVISORIA-PUNTEADA.

       CARGAR-TABLA-CATEGORIAS.
           READ ARCH-CATEGORIAS
               AT END MOVE '10' TO FS-CAT.

           MOVE 1 TO IND-TAB-CAT.

           PERFORM CARGA-CATEGORIA-EN-TABLA
               UNTIL CAT-EOF OR IND-TAB-CAT NOT LESS THAN 50.

       CARGA-CATEGORIA-EN-TABLA.
           MOVE REG-CAT TO TAB-CAT(IND-TAB-CAT).

           ADD 1 TO IND-TAB-CAT.

           READ ARCH-CATEGORIAS
               AT END MOVE '10' TO FS-CAT.

       CARGAR-TABLA-EMPRESAS.
         READ ARCH-EMPRESAS
           AT END MOVE '10' TO FS-EMP.

         MOVE 1 TO IND-TAB-EMP.

         PERFORM CARGAR-EMPRESA-EN-TABLA
           UNTIL EMP-EOF
           OR IND-TAB-EMP NOT LESS THAN 999.

      *  guardo la cantidd de empresas
         MOVE IND-TAB-EMP TO WS-CANT-EMP.

       CARGAR-EMPRESA-EN-TABLA.
           MOVE REG-EMP-TAB TO TAB-EMP(IND-TAB-EMP).

           ADD 1 TO IND-TAB-EMP.

           READ ARCH-EMPRESAS AT END MOVE '10' TO FS-EMP.

       ORD-EMP.

       ORDERNAR-EMPRESAS-X-RAZ-SOC.
         MOVE 1 TO WS-I.
           PERFORM UNTIL WS-I = WS-CANT-EMP
             MOVE WS-I TO WS-J
             PERFORM UNTIL WS-J = WS-CANT-EMP
               IF (TAB-EMP-RAZON(WS-I) > TAB-EMP-RAZON(WS-J))
                 MOVE TAB-EMP(WS-I) TO REG-TAB-EMP-TEMP
                 MOVE TAB-EMP(WS-J) TO TAB-EMP(WS-I)
                 MOVE REG-TAB-EMP-TEMP TO TAB-EMP(WS-J)
               END-IF
               ADD 1 TO WS-J GIVING WS-J
             END-PERFORM
             ADD 1 TO WS-I GIVING WS-I
           END-PERFORM.
           MOVE 1 TO WS-I.

       INICIALIZACION-VARIABLES.
           MOVE 90 TO WS-ACUM-LINEAS.
           MOVE 1 TO WS-ACUM-NRO-HOJAS.
           MOVE 1 TO WS-I.
           MOVE ZERO TO WS-ACUM-IMP-CONS-FECHA.
           MOVE ZERO TO WS-ACUM-IMP-CONS.
           MOVE ZERO TO WS-ACUM-HOR-CONS.
           MOVE ZERO TO WS-ACUM-IMP-GEN.

           ACCEPT FECHA FROM DATE.

       ABRIR-ARCHIVOS.

           OPEN INPUT ARCH-NOV1.
           IF NOV1-OK THEN
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO NOV1 ' FS-NOV1
              PERFORM FIN.

           OPEN INPUT ARCH-NOV2.
           IF NOV2-OK THEN
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO ARCH-NOV2 ' FS-NOV2
              PERFORM FIN.

           OPEN INPUT ARCH-NOV3.
           IF NOV3-OK THEN
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO ARCH-NOV3 ' FS-NOV3
              PERFORM FIN.

           OPEN INPUT ARCH-CATEGORIAS.
           IF CAT-OK
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO CATEGORIAS ' FS-CAT
              PERFORM FIN.

           OPEN INPUT ARCH-EMPRESAS.
           IF EMP-OK
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO EMPRESAS ' FS-CAT
              PERFORM FIN.

           OPEN INPUT ARCH-CONSULTORES
           IF CONS-OK
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO ABRIRSE ARCHIVO CATEGORIAS ' FS-CONS
              PERFORM FIN.

           OPEN OUTPUT ARCH-TIMES.
           IF TIM-OK
              NEXT SENTENCE
           ELSE
              DISPLAY 'NO PUDO CREARSE ARCHIVO MAE-TIMES ' FS-TIMES
              PERFORM FIN.

       CARGAR-ANIOS-EST.
      *  obtengo el año actual
         PERFORM OBTENER-ANIO-ACTUAL.
      *  cargo el año que quiero mostrar
         COMPUTE WS-ANIO-ACTUAL = WS-ANIO-ACTUAL - WS-I + 1.
      *  cargo el año que quiero mostrar
         MOVE WS-ANIO-ACTUAL TO LINEA-EST-COL-ANIO(I).

      *  actualizo los indices
         COMPUTE WS-I = WS-I - 1.
         COMPUTE I = I + 1.

       ABRIR-TIMES-LECTURA.

         CLOSE ARCH-TIMES

         OPEN INPUT ARCH-TIMES.
         IF TIM-OK
            NEXT SENTENCE
         ELSE
            DISPLAY 'NO PUDO CREARSE ARCHIVO MAE-TIMES ' FS-TIMES
            PERFORM FIN.

       LIMPIAR-ESTADISTICAS.
      *  limpio los valores en los meses
         SET LINEA-EST-COL-ENE(WS-I) TO 0.
         SET LINEA-EST-COL-FEB(WS-I) TO 0.
         SET LINEA-EST-COL-MAR(WS-I) TO 0.
         SET LINEA-EST-COL-ABR(WS-I) TO 0.
         SET LINEA-EST-COL-MAY(WS-I) TO 0.
         SET LINEA-EST-COL-JUN(WS-I) TO 0.
         SET LINEA-EST-COL-JUL(WS-I) TO 0.
         SET LINEA-EST-COL-AGO(WS-I) TO 0.
         SET LINEA-EST-COL-SEP(WS-I) TO 0.
         SET LINEA-EST-COL-OCT(WS-I) TO 0.
         SET LINEA-EST-COL-NOV(WS-I) TO 0.
         SET LINEA-EST-COL-DIC(WS-I) TO 0.
      *  limpio los valores de los totales
         SET LINEA-EST-COL-TOT(WS-I) TO 0.

         ADD 1 TO WS-I.

       LEER-TIMES.
         READ ARCH-TIMES AT END MOVE '10' TO FS-TIMES.

         IF TIM-OK
            NEXT SENTENCE
         ELSE
            MOVE "99999" TO REG-NOV1-NUMERO
            MOVE "99999999" TO REG-NOV1-FECHA.

       OBTENER-ANIO-ACTUAL.
         MOVE FUNCTION CURRENT-DATE (1:4) TO WS-ANIO-ACTUAL.

       VERIFICAR-ANIO-EST.
         PERFORM OBTENER-ANIO-ACTUAL.
         IF (WS-ANIO-AAAA = WS-ANIO-ACTUAL)
           MOVE 'OK' TO WS-ANIO-TEMP
           SET WS-OFFSET-ANIO-EST TO 5
         ELSE IF (WS-ANIO-AAAA = WS-ANIO-ACTUAL - 1)
           MOVE 'OK' TO WS-ANIO-TEMP
           SET WS-OFFSET-ANIO-EST TO 4
         ELSE IF (WS-ANIO-AAAA = WS-ANIO-ACTUAL - 2)
           MOVE 'OK' TO WS-ANIO-TEMP
           SET WS-OFFSET-ANIO-EST TO 3
         ELSE IF (WS-ANIO-AAAA = WS-ANIO-ACTUAL - 3)
           MOVE 'OK' TO WS-ANIO-TEMP
           SET WS-OFFSET-ANIO-EST TO 2
         ELSE IF (WS-ANIO-AAAA = WS-ANIO-ACTUAL - 4)
           MOVE 'OK' TO WS-ANIO-TEMP
           SET WS-OFFSET-ANIO-EST TO 1
         ELSE
           MOVE 'NO' TO WS-ANIO-TEMP
           SET WS-OFFSET-ANIO-EST TO 99.

      * se van sumando los valores obtenidos en el correspondiente mes
      * y año en la tabla que se muestra por pantalla.
       CARGAR-DATOS-EN-MATRIZ-EST.

      *  Se hacen los siguientes calculos, en el siguiente orden, dentro
      *  de cada sentecia if, por cada mes del año:
      *   > recalculo el valor mensual
      *   > recalculo el valor mensual de los ultimos 5 años
      *   > calculo el total mensual global
      *  ENERO
         IF (WS-MES-MM = '01')
           COMPUTE LINEA-EST-COL-ENE(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-ENE(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-ENE(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-ENE(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-ENE = TOTAL-EST-ENE + REG-TIMES-HORAS

      *  FEBRERO
         ELSE IF WS-MES-MM = '02'
           COMPUTE LINEA-EST-COL-FEB(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-FEB(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-FEB(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-FEB(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-FEB = TOTAL-EST-FEB + REG-TIMES-HORAS

      *  MARZO
         ELSE IF WS-MES-MM = '03'
           COMPUTE LINEA-EST-COL-MAR(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-MAR(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-MAR(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-MAR(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-MAR = TOTAL-EST-MAR + REG-TIMES-HORAS

      *  ABRIL
         ELSE IF WS-MES-MM = '04'
           COMPUTE LINEA-EST-COL-ABR(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-ABR(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-ABR(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-ABR(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-ABR = TOTAL-EST-ABR + REG-TIMES-HORAS

      *  MAYO
         ELSE IF WS-MES-MM = '05'
           COMPUTE LINEA-EST-COL-MAY(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-MAY(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-MAY(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-MAY(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-MAY = TOTAL-EST-MAY + REG-TIMES-HORAS

      *  JUNIO
         ELSE IF WS-MES-MM = '06'
           COMPUTE LINEA-EST-COL-JUN(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-JUN(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-JUN(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-JUN(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-JUN = TOTAL-EST-JUN + REG-TIMES-HORAS

      *  JULIO
         ELSE IF WS-MES-MM = '07'
           COMPUTE LINEA-EST-COL-JUL(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-JUL(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-JUL(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-JUL(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-JUL = TOTAL-EST-JUL + REG-TIMES-HORAS

      *  AGOSTO
         ELSE IF WS-MES-MM = '08'
           COMPUTE LINEA-EST-COL-AGO(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-AGO(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-AGO(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-AGO(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-AGO = TOTAL-EST-AGO + REG-TIMES-HORAS

      *  SEPTIEMBRE
         ELSE IF WS-MES-MM = '09'
           COMPUTE LINEA-EST-COL-SEP(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-SEP(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-SEP(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-SEP(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-SEP = TOTAL-EST-SEP + REG-TIMES-HORAS

      *  OCTUBRE
         ELSE IF WS-MES-MM = '10'
           COMPUTE LINEA-EST-COL-OCT(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-OCT(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-OCT(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-OCT(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-OCT = TOTAL-EST-OCT + REG-TIMES-HORAS

      *  NOVIEMBRE
         ELSE IF WS-MES-MM = '11'
           COMPUTE LINEA-EST-COL-NOV(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-NOV(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-NOV(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-NOV(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-NOV = TOTAL-EST-NOV + REG-TIMES-HORAS

      *  DICIEMBRE
         ELSE IF WS-MES-MM = '12'
           COMPUTE LINEA-EST-COL-DIC(WS-OFFSET-ANIO-EST)
               = LINEA-EST-COL-DIC(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
           COMPUTE LINEA-EST-COL-DIC(WS-OFFSET-TOT-MES)
               = LINEA-EST-COL-DIC(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
           COMPUTE TOTAL-EST-DIC = TOTAL-EST-DIC + REG-TIMES-HORAS

      *  MAL OFFSET
         ELSE SET WS-OFFSET-MES TO 99.

         IF NOT WS-OFFSET-MES-MAL
      *    recalculo el total anual de la empresa
           COMPUTE LINEA-EST-COL-TOT(WS-OFFSET-ANIO-EST)
             = LINEA-EST-COL-TOT(WS-OFFSET-ANIO-EST) + REG-TIMES-HORAS
      *    recalculo el total global de la empresa
           COMPUTE LINEA-EST-COL-TOT(WS-OFFSET-TOT-MES)
             = LINEA-EST-COL-TOT(WS-OFFSET-TOT-MES) + REG-TIMES-HORAS
      *    recalculo el total global
           COMPUTE TOTAL-EST-TOT = TOTAL-EST-TOT + REG-TIMES-HORAS.

       CALCULAR-EST-X-EMPRESA.
      *  leo el registro en el archivo de horas
         PERFORM LEER-TIMES.
      *  si corresponde a la empresa que estoy filtrando
         IF (REG-TIMES-EMPRESA IS EQUALS TAB-EMP-COD(IND-TAB-EMP))
      *    identifico el año a cargar
           MOVE REG-TIMES-FECHA (5:4) TO WS-ANIO-AAAA
           PERFORM VERIFICAR-ANIO-EST
      *    si el año esta dentro del rango que tengo que mostrar
      *    (los últimos 5), calculo la cantidad
           IF WS-ANIO-TEMP-OK
      *      identifico el mes a cargar
             MOVE REG-TIMES-FECHA (3:2) TO WS-MES-MM
      *      lo cargo en la matriz
             PERFORM CARGAR-DATOS-EN-MATRIZ-EST.



       CARGAR-EST-POR-EMPRESA.
         SET WS-I TO 1.
         PERFORM LIMPIAR-ESTADISTICAS UNTIL WS-I = 7.

         PERFORM ABRIR-TIMES-LECTURA.
         PERFORM CALCULAR-EST-X-EMPRESA UNTIL TIM-EOF.

      *  cargo el nombre de la empresa
         MOVE TAB-EMP-RAZON(IND-TAB-EMP) TO LINEA-EST-COL-EMPRESA(1).

         MOVE 1 TO I.
         PERFORM MOSTRAR-ESTADISTICAS UNTIL I = 7.

         ADD 1 TO IND-TAB-EMP.

         IF IND-TAB-EMP IS NOT EQUAL WS-CANT-EMP
           DISPLAY ' '
         ELSE
           DISPLAY LINEA-DIVISORIA-CONTINUA.

       MOSTRAR-ESTADISTICAS.
         DISPLAY LINEA-LISTADO-EST(I).
         ADD 1 TO I.

       IMPRIMIR-TAB-EMP.
         DISPLAY 'EMPRESA ' IND-TAB-EMP ': ' TAB-EMP(IND-TAB-EMP).
         ADD 1 TO IND-TAB-EMP.

       ESTADISTICAS-X-EMPRESA.

         DISPLAY ENCABE-LINEA1.
         DISPLAY ENCABE-LINEA2-EST.
         DISPLAY LINEA-EN-BLANCO.
         DISPLAY ENCABE-LISTADO-EST.
         DISPLAY LINEA-DIVISORIA-CONTINUA.
         DISPLAY ' '.

         PERFORM ORDERNAR-EMPRESAS-X-RAZ-SOC.
         MOVE 1 TO I.
         MOVE 5 TO WS-I.
         PERFORM CARGAR-ANIOS-EST UNTIL I = 6.

         MOVE 1 TO IND-TAB-EMP.
         PERFORM CARGAR-EST-POR-EMPRESA
                 UNTIL IND-TAB-EMP IS EQUAL WS-CANT-EMP.

         DISPLAY ' '.
         DISPLAY TOTAL-GLOBAL-EST.

       FIN.
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

       CERRAR-ARCHIVOS.
           CLOSE ARCH-NOV1.

           CLOSE ARCH-NOV2.
           CLOSE ARCH-NOV3.
           CLOSE ARCH-CONSULTORES.
           CLOSE ARCH-CATEGORIAS.
           CLOSE ARCH-EMPRESAS.
           CLOSE ARCH-TIMES.
