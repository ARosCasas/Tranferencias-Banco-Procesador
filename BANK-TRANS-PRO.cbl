       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-TRANS-PROCESSOR.
      * AUTHOR. Alejandro Ros.
      *-----------------------------------------------------------------------*
      *CAMBIOS:
      *C0001 --> Fecha: 06/11/2025
      *          Descripción: Modificación para eliminar el uso del archivo
      *                       NUEVO-CUENTAS.dat, y que los cambios se hagan en
      *                       el propio archivo CUENTAS.dat.
      *
      *-----------------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
 C0001*  El archivo MASTER-FILE es ahora INDEXED
      * y se accede de forma RANDOM (aleatoria) por clave.
           SELECT MASTER-FILE  ASSIGN TO 'CUENTAS-INDEXADAS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MR-CUENTA-NUM
               FILE STATUS IS WS-MASTER-STATUS.

           SELECT TRANS-IN   ASSIGN TO 'TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
 C0001*  Elimino MASTER-OUT.

           SELECT ERROR-RPT  ASSIGN TO 'ERRORES.RPT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

 C0001* FD único para el archivo maestro.
       FD  MASTER-FILE.
       01  MASTER-RECORD.
           05 MR-CUENTA-NUM  PIC 9(10).
           05 MR-NOMBRE         PIC X(30).
           05 MR-BALANCE      PIC 9(8)V99.

       FD  TRANS-IN.
       01  TRANS-RECORD-IN.
           05 TR-CUENTA-NUM  PIC 9(10).
           05 TR-TIPO         PIC X(1).
              88 DEPOSITO   VALUE 'D'.
              88 RETIRADA  VALUE 'R'.
           05 TR-CANTIDAD       PIC 9(8)V99.

       FD  ERROR-RPT.
       01  ERROR-RECORD-OUT.
           05 ERO-CUENTA-NUM  PIC 9(10).
           05 ERO-TIPO         PIC X.
           05 ERO-CANTIDAD       PIC 9(8)V99.
           05 ERO-SEPARADOR    PIC X(3) VALUE ' - '.
           05 ERO-MENSAJE      PIC X(50).

       WORKING-STORAGE SECTION.

       01  WS-END-FLAGS.
           05 WS-TRANS-EOF-FLAG  PIC X VALUE 'N'.
              88 END-OF-TRANS  VALUE 'S'.

       01  WS-WORK-AREAS.
           05 WS-MASTER-STATUS  PIC XX.
              88 MASTER-FOUND VALUE '00'.
              88 MASTER-NOT-FOUND VALUE '23'.
           05 WS-TOTAL-TRANS     PIC 9(5) VALUE ZERO.
           05 WS-BALANCE-ACTUAL PIC S9(8)V99.
           05 WS-ERROR-MSG       PIC X(50).

       PROCEDURE DIVISION.
           PERFORM 000-INICIO-SIST
           PERFORM 100-PROCESAR-TRANSACCION
               UNTIL END-OF-TRANS
           PERFORM 900-FINALIZAR-SIST
           STOP RUN.

       000-INICIO-SIST.
 C0001* MASTER-FILE se abre en modo I-O (Input-Output).
           OPEN I-O MASTER-FILE
           OPEN INPUT TRANS-IN
           OPEN OUTPUT ERROR-RPT
           PERFORM 150-READ-TRANS-FILE.

       100-PROCESAR-TRANSACCION.
           ADD 1 TO WS-TOTAL-TRANS

 C0001* Movemos el número de cuenta de la transacción
      *     al campo clave del archivo maestro.
           MOVE TR-CUENTA-NUM TO MR-CUENTA-NUM

      * Intenta leer el registro maestro usando la clave (MR-CUENTA-NUM)
           READ MASTER-FILE
               INVALID KEY
                   MOVE 'ERROR: CUENTA MAESTRA NO ENCONTRADA (KEY: '
                        TO WS-ERROR-MSG
                   STRING TR-CUENTA-NUM DELIMITED BY SIZE
                          ' - STATUS: ' WS-MASTER-STATUS
                       INTO WS-ERROR-MSG
                   END-STRING
                   PERFORM 400-WRITE-ERROR-TRANS
               NOT INVALID KEY
      * El registro maestro existe, proceso la transacción.
                   PERFORM 300-APLICAR-TRANSACCION
           END-READ

           PERFORM 150-READ-TRANS-FILE.

       150-READ-TRANS-FILE.
           READ TRANS-IN
               AT END
                   MOVE 'S' TO WS-TRANS-EOF-FLAG
           END-READ.

       300-APLICAR-TRANSACCION.
           MOVE MR-BALANCE TO WS-BALANCE-ACTUAL

           IF TR-TIPO NOT = 'D' AND TR-TIPO NOT = 'R'
               MOVE 'ERROR: TIPO DE TRANSACCION INVALIDO'
                    TO WS-ERROR-MSG
               PERFORM 400-WRITE-ERROR-TRANS
           ELSE
               IF DEPOSITO
                   ADD TR-CANTIDAD TO WS-BALANCE-ACTUAL
               ELSE IF RETIRADA
                   IF WS-BALANCE-ACTUAL >= TR-CANTIDAD
                       SUBTRACT TR-CANTIDAD FROM WS-BALANCE-ACTUAL
                   ELSE
                       MOVE 'ERROR: SALDO INSUFICIENTE (SOBREGIRO)'
                            TO WS-ERROR-MSG
                       PERFORM 400-WRITE-ERROR-TRANS
                   END-IF
               END-IF
           END-IF

 C0001*Actualiza el balance del registro y REWRITE.
           MOVE WS-BALANCE-ACTUAL TO MR-BALANCE
           PERFORM 310-REWRITE-MASTER.

       310-REWRITE-MASTER.
           REWRITE MASTER-RECORD
               INVALID KEY
                   MOVE 'ERROR CRITICO: REWRITE FALLIDO' TO WS-ERROR-MSG
                   PERFORM 400-WRITE-ERROR-TRANS
           END-REWRITE.

       400-WRITE-ERROR-TRANS.
           MOVE TR-CUENTA-NUM  TO ERO-CUENTA-NUM
           MOVE TR-TIPO        TO ERO-TIPO
           MOVE TR-CANTIDAD    TO ERO-CANTIDAD
           MOVE WS-ERROR-MSG   TO ERO-MENSAJE
           WRITE ERROR-RECORD-OUT.

       900-FINALIZAR-SIST.
 C0001* Cerrar MASTER-FILE.
           CLOSE MASTER-FILE, TRANS-IN, ERROR-RPT.
           DISPLAY '--- PROCESAMIENTO BANCARIO FINALIZADO ---'
           DISPLAY WS-TOTAL-TRANS ' TRANSACCIONES PROCESADAS.'.
