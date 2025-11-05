       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-TRANS-PROCESSOR.
      * AUTHOR. Alejandro Ros.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-IN  ASSIGN TO 'CUENTAS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANS-IN   ASSIGN TO 'TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MASTER-OUT ASSIGN TO 'NUEVO-CUENTAS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROR-RPT  ASSIGN TO 'ERRORES.RPT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  MASTER-IN.
       01  MASTER-RECORD-IN.
           05 MR-CUENTA-NUM  PIC 9(10).
           05 MR-NOMBRE         PIC X(30).
           05 MR-BALANCE      PIC S9(8)V99.

       FD  TRANS-IN.
       01  TRANS-RECORD-IN.
           05 TR-CUENTA-NUM  PIC 9(10).
           05 TR-TIPO         PIC X(1).
              88 DEPOSITO   VALUE 'D'.
              88 RETIRADA  VALUE 'R'.
           05 TR-CANTIDAD       PIC 9(8)V99.

       FD  MASTER-OUT.
       01  MASTER-RECORD-OUT.
           05 MO-CUENTA-NUM  PIC 9(10).
           05 MO-NOMBRE         PIC X(30).
           05 MO-BALANCE      PIC S9(8)V99.

       FD  ERROR-RPT.
       01  ERROR-RECORD-OUT.
           05 ERO-CUENTA-NUM  PIC 9(10).
           05 ERO-TIPO         PIC X.
           05 ERO-CANTIDAD       PIC 9(8)V99.
           05 ERO-SEPARADOR    PIC X(3) VALUE ' - '.
           05 ERO-MENSAJE      PIC X(50).

       WORKING-STORAGE SECTION.

       01  WS-END-FLAGS.
           05 WS-MASTER-EOF-FLAG PIC X VALUE 'N'.
              88 END-OF-MASTER VALUE 'S'.
           05 WS-TRANS-EOF-FLAG  PIC X VALUE 'N'.
              88 END-OF-TRANS  VALUE 'S'.

       01  WS-WORK-AREAS.
           05 WS-TOTAL-TRANS     PIC 9(5) VALUE ZERO.
           05 WS-BALANCE-ACTUAL PIC S9(8)V99.
           05 WS-ERROR-MSG       PIC X(50).

       PROCEDURE DIVISION.
           PERFORM 000-INICIO-SIST
           PERFORM 100-PROCESAR-FILES
               UNTIL END-OF-MASTER AND END-OF-TRANS
           PERFORM 900-FINALIZAR-SIST
           STOP RUN.

       000-INICIO-SIST.
           OPEN INPUT MASTER-IN, TRANS-IN
           OPEN OUTPUT MASTER-OUT, ERROR-RPT
           PERFORM 110-READ-MASTER-FILE
           PERFORM 150-READ-TRANS-FILE.

       100-PROCESAR-FILES.
           IF MR-CUENTA-NUM < TR-CUENTA-NUM
      * Registro maestro sin transacciones
               PERFORM 200-WRITE-MASTER-OUT
               PERFORM 110-READ-MASTER-FILE
           ELSE IF MR-CUENTA-NUM = TR-CUENTA-NUM
      * Registro maestro con transacciones coincidentes
               PERFORM 300-PROCESAR-TRANSACIONES
           ELSE
      * Registro de transacción sin registro maestro coincidente
               MOVE 'ERROR: CUENTA MAESTRA NO ENCONTRADA'
                    TO WS-ERROR-MSG
               PERFORM 400-WRITE-ERROR-TRANS
               PERFORM 150-READ-TRANS-FILE
           END-IF.

       110-READ-MASTER-FILE.
           READ MASTER-IN
               AT END
                   MOVE 'S' TO WS-MASTER-EOF-FLAG
      * Clave de valor alto para asegurar la terminación del bucle de comparación
      * 'MOVE HIGH-VALUES' no es correcto. Da Error.
                   MOVE 9999999999 TO MR-CUENTA-NUM
           END-READ.

       150-READ-TRANS-FILE.
           READ TRANS-IN
               AT END
                   MOVE 'S' TO WS-TRANS-EOF-FLAG
      * Clave de valor alto para asegurar la terminación del bucle de comparación
                   MOVE 9999999999 TO TR-CUENTA-NUM
           END-READ.

       200-WRITE-MASTER-OUT.
           MOVE MASTER-RECORD-IN TO MASTER-RECORD-OUT
           WRITE MASTER-RECORD-OUT.

       300-PROCESAR-TRANSACIONES.
           MOVE MR-BALANCE TO WS-BALANCE-ACTUAL

      * Procesar todas las transacciones para cuenta maestra actual.
           PERFORM UNTIL TR-CUENTA-NUM > MR-CUENTA-NUM OR END-OF-TRANS
               ADD 1 TO WS-TOTAL-TRANS

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

               PERFORM 150-READ-TRANS-FILE
           END-PERFORM

           MOVE WS-BALANCE-ACTUAL TO MR-BALANCE
           PERFORM 200-WRITE-MASTER-OUT
           PERFORM 110-READ-MASTER-FILE.

       400-WRITE-ERROR-TRANS.
           MOVE TR-CUENTA-NUM TO ERO-CUENTA-NUM
           MOVE TR-TIPO        TO ERO-TIPO
           MOVE TR-CANTIDAD      TO ERO-CANTIDAD
           MOVE WS-ERROR-MSG   TO ERO-MENSAJE
           WRITE ERROR-RECORD-OUT.

       900-FINALIZAR-SIST.
           CLOSE MASTER-IN, TRANS-IN, MASTER-OUT, ERROR-RPT.
           DISPLAY '--- PROCESAMIENTO BANCARIO FINALIZADO ---'
           DISPLAY WS-TOTAL-TRANS ' TRANSACCIONES PROCESADAS.'.
