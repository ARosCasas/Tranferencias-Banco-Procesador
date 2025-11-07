       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANS-CLEANER.
      * PROPOSITO: Crea un nuevo archivo de transacciones excluyendo las
      * transacciones de cuentas que ya no existen en el NUEVO maestro.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-IN    ASSIGN TO 'NUEVO-CUENTAS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANS-IN     ASSIGN TO 'TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANS-OUT    ASSIGN TO 'NUEVO-TRANS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  MASTER-IN.
       01  MASTER-RECORD-IN.
           05 MR-CUENTA-NUM      PIC 9(10).
           05 FILLER             PIC X(42).

       FD  TRANS-IN.
       01  TRANS-RECORD-IN.
           05 TR-CUENTA-NUM      PIC 9(10).
           05 TR-RESTO           PIC X(11).

       FD  TRANS-OUT.
       01  TRANS-RECORD-OUT      PIC X(21).

       WORKING-STORAGE SECTION.
       01  WS-END-FLAGS.
           05 WS-MASTER-EOF-FLAG PIC X VALUE 'N'.
              88 END-OF-MASTER VALUE 'S'.
           05 WS-TRANS-EOF-FLAG  PIC X VALUE 'N'.
              88 END-OF-TRANS  VALUE 'S'.

       01  WS-WORK-AREAS.
           05 WS-TRANS-MANTENIDAS PIC 9(5) VALUE ZERO.
           05 WS-TRANS-ELIMINADAS PIC 9(5) VALUE ZERO.

       PROCEDURE DIVISION.
           PERFORM 000-INICIO
           PERFORM 100-PROCESAR-FILES
               UNTIL END-OF-MASTER AND END-OF-TRANS
           PERFORM 900-FINALIZAR
           STOP RUN.

      *======================================================
      * INICIO Y LECTURAS
      *======================================================
       000-INICIO.
           OPEN INPUT MASTER-IN, TRANS-IN
           OPEN OUTPUT TRANS-OUT
           PERFORM 110-READ-MASTER-FILE
           PERFORM 150-READ-TRANS-FILE.

       110-READ-MASTER-FILE.
           READ MASTER-IN
               AT END
                   MOVE 'S' TO WS-MASTER-EOF-FLAG
                   MOVE 9999999999 TO MR-CUENTA-NUM
           END-READ.

       150-READ-TRANS-FILE.
           READ TRANS-IN
               AT END
                   MOVE 'S' TO WS-TRANS-EOF-FLAG
                   MOVE 9999999999 TO TR-CUENTA-NUM
           END-READ.

      *======================================================
      * BUCLE PRINCIPAL (Match-Merge de Maestro y Transacciones)
      *======================================================
       100-PROCESAR-FILES.
           IF MR-CUENTA-NUM < TR-CUENTA-NUM
      * La cuenta Maestra no tiene más transacciones. Avanzar al siguiente maestro.
               PERFORM 110-READ-MASTER-FILE

           ELSE IF MR-CUENTA-NUM = TR-CUENTA-NUM
      * Coincidencia: La transacción pertenece a una cuenta existente (mantener).
               PERFORM 200-WRITE-TRANS-OUT
               PERFORM 150-READ-TRANS-FILE

           ELSE
      * TR-CUENTA-NUM < MR-CUENTA-NUM: Transacción huérfana de una cuenta eliminada.
               PERFORM 300-ELIMINAR-TRANS
               PERFORM 150-READ-TRANS-FILE
           END-IF.

       200-WRITE-TRANS-OUT.
      * Escribe la transacción porque su cuenta existe en el NUEVO maestro.
           MOVE TRANS-RECORD-IN TO TRANS-RECORD-OUT
           WRITE TRANS-RECORD-OUT
           ADD 1 TO WS-TRANS-MANTENIDAS.

       300-ELIMINAR-TRANS.
      * No escribe la transacción. Se considera eliminada (huérfana).
           ADD 1 TO WS-TRANS-ELIMINADAS
           DISPLAY 'TRANSACCION HUERFANA ELIMINADA: ' TR-CUENTA-NUM.

      *======================================================
      * CIERRE
      *======================================================
       900-FINALIZAR.
           CLOSE MASTER-IN, TRANS-IN, TRANS-OUT.
           DISPLAY '--- LIMPIEZA DE TRANSACCIONES FINALIZADA ---'
           DISPLAY WS-TRANS-MANTENIDAS ' TRANSACCIONES MANTENIDAS.'
           DISPLAY WS-TRANS-ELIMINADAS
           ' TRANSACCIONES ELIMINADAS (HUERFANAS).'.
