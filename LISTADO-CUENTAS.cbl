       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIST-MASTER-FILE.
      * PROPOSITO: Lee secuencialmente e imprime el contenido
      * del archivo maestro indexado (CUENTAS-INDEXADAS.DAT).

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE  ASSIGN TO 'CUENTAS-INDEXADAS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MR-CUENTA-NUM
               FILE STATUS IS WS-MASTER-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  MASTER-FILE.
       01  MASTER-RECORD.
           05 MR-CUENTA-NUM  PIC 9(10).
           05 MR-NOMBRE         PIC X(30).
           05 MR-BALANCE      PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05 WS-MASTER-STATUS  PIC XX.
           05 WS-MASTER-EOF     PIC X VALUE 'N'.
              88 END-OF-MASTER  VALUE 'S'.

       01  WS-DISPLAY-FIELDS.
           05 WS-BALANCE-OUT  PIC Z,ZZZ,ZZ9.99.
           05 WS-COUNTER      PIC 9(5) VALUE ZERO.

       PROCEDURE DIVISION.
           PERFORM 000-INICIO-SIST
           PERFORM 100-PROCESS-RECORDS
               UNTIL END-OF-MASTER
           PERFORM 900-FINALIZAR-SIST
           STOP RUN.

       000-INICIO-SIST.
           DISPLAY '*** LISTADO DEL ARCHIVO MAESTRO INDEXADO ***'
           OPEN INPUT MASTER-FILE

           IF WS-MASTER-STATUS NOT = '00'
            DISPLAY 'ERROR AL ABRIR ARCHIVO. STATUS: '
            DISPLAY WS-MASTER-STATUS
               MOVE 'S' TO WS-MASTER-EOF
           END-IF

           PERFORM 110-READ-MASTER-FILE.

       100-PROCESS-RECORDS.
           ADD 1 TO WS-COUNTER

           MOVE MR-BALANCE TO WS-BALANCE-OUT

           DISPLAY '------------------------------------'
           DISPLAY 'REGISTRO No. ' WS-COUNTER
           DISPLAY 'CUENTA:  ' MR-CUENTA-NUM
           DISPLAY 'NOMBRE:  ' MR-NOMBRE
           DISPLAY 'SALDO:   ' WS-BALANCE-OUT  ' EUR'

           PERFORM 110-READ-MASTER-FILE.

       110-READ-MASTER-FILE.
           READ MASTER-FILE NEXT RECORD
               AT END
                   MOVE 'S' TO WS-MASTER-EOF
           END-READ.

       900-FINALIZAR-SIST.
           CLOSE MASTER-FILE.
           DISPLAY '------------------------------------'
           DISPLAY 'TOTAL DE REGISTROS LISTADOS: ' WS-COUNTER.
