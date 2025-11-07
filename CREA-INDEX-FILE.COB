       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREA-INDEX-FILE.
      * AUTOR. ALEJANDRO ROS.
      * PROPOSITO: Convierte el archivo de cuentas secuencial
      * (CUENTAS.DAT) a un archivo indexado (CUENTAS-INDEXADAS.DAT).
      *-----------------------------------------------------------------------*
      *CAMBIOS:
      *C0001 --> Fecha: 07/11/2025
      *          Descripción: Corrección del error
      *          'libcob: LISTADO-CUENTAS.cbl: 56: 'MR-BALANCE'
      *           not numeric: '  00015000'.
      *
      *           Modificación: Lee los campos como texto y luego los
      *           transforma a numero con FUNCTION NUMVAL().
      *
      *
      *
      *-----------------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Archivo de ENTRADA (su archivo de texto plano)
           SELECT SEQ-IN  ASSIGN TO 'CUENTAS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

      * Archivo de SALIDA (el nuevo archivo indexado)
           SELECT IDX-OUT ASSIGN TO 'CUENTAS-INDEXADAS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MR-CUENTA-NUM OF IDX-RECORD-OUT
               FILE STATUS IS WS-IDX-STATUS.

       DATA DIVISION.
       FILE SECTION.

      * Estructura de ENTRADA
       FD  SEQ-IN.
       01  SEQ-RECORD-IN.
 C0001*    05 MR-CUENTA-NUM  PIC 9(10).
 C0001     05 MR-CUENTA-NUM-TXT  PIC X(10).
           05 MR-NOMBRE         PIC X(30).
 C0001*    05 MR-BALANCE        PIC 9(8)V99.
 C0001     05 MR-BALANCE-TXT   PIC X(10).

      * Estructura de SALIDA
       FD  IDX-OUT.
       01  IDX-RECORD-OUT.
           05 MR-CUENTA-NUM  PIC 9(10).
           05 MR-NOMBRE      PIC X(30).
           05 MR-BALANCE     PIC 9(8)V99.

       WORKING-STORAGE SECTION.

       01  WS-FLAGS.
           05 WS-SEQ-EOF-FLAG PIC X VALUE 'N'.
              88 END-OF-SEQ-FILE VALUE 'S'.

       01  WS-STATUS-FIELDS.
           05 WS-IDX-STATUS   PIC XX VALUE ZERO.
           05 WS-RECORDS-OUT  PIC 9(5) VALUE ZERO.

       PROCEDURE DIVISION.
           PERFORM 000-INICIO-CONVERSION
           PERFORM 100-PROCESS-RECORDS UNTIL END-OF-SEQ-FILE
           PERFORM 900-FINALIZAR-CONVERSION
           STOP RUN.

       000-INICIO-CONVERSION.
           DISPLAY '*** INICIANDO CONVERSION DE ARCHIVO ***'
           OPEN INPUT SEQ-IN
           OPEN OUTPUT IDX-OUT

           IF WS-IDX-STATUS NOT = '00'
               DISPLAY
               'ERROR AL ABRIR EL ARCHIVO INDEXADO: ' WS-IDX-STATUS
               STOP RUN
           END-IF.
           PERFORM 110-READ-SEQ-FILE.

       100-PROCESS-RECORDS.
C0001I*    MOVE SEQ-RECORD-IN TO IDX-RECORD-OUT
      *    WRITE IDX-RECORD-OUT
      *        INVALID KEY
      *            DISPLAY
      *            'ERROR AL ESCRIBIR REGISTRO (CLAVE DUPLICADA): '
      *               MR-CUENTA-NUM OF SEQ-RECORD-IN
      *               ' STATUS: ' WS-IDX-STATUS
      *            PERFORM 110-READ-SEQ-FILE
      *       NOT INVALID KEY
      *           ADD 1 TO WS-RECORDS-OUT
      *            PERFORM 110-READ-SEQ-FILE
C0001F*    END-WRITE.

C0001I     MOVE FUNCTION NUMVAL(MR-CUENTA-NUM-TXT)
                TO MR-CUENTA-NUM OF IDX-RECORD-OUT

           MOVE MR-NOMBRE OF SEQ-RECORD-IN
            TO MR-NOMBRE OF IDX-RECORD-OUT

           MOVE FUNCTION NUMVAL(MR-BALANCE-TXT)
                TO MR-BALANCE OF IDX-RECORD-OUT

           WRITE IDX-RECORD-OUT
               INVALID KEY
                   DISPLAY 'ERROR: CLAVE DUPLICADA ->' MR-CUENTA-NUM-TXT
               NOT INVALID KEY
                   ADD 1 TO WS-RECORDS-OUT
           END-WRITE

C0001F     PERFORM 110-READ-SEQ-FILE.


       110-READ-SEQ-FILE.
           READ SEQ-IN
               AT END
                   MOVE 'S' TO WS-SEQ-EOF-FLAG
           END-READ.

       900-FINALIZAR-CONVERSION.
           CLOSE SEQ-IN, IDX-OUT.
           DISPLAY '--- CONVERSION FINALIZADA ---'
           DISPLAY WS-RECORDS-OUT ' REGISTROS CONVERTIDOS A INDEXADOS.'.
