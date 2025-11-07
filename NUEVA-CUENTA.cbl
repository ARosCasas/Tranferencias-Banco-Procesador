       IDENTIFICATION DIVISION.
       PROGRAM-ID. NUEVA-CUENTA.
      * PROPOSITO: Añade un nuevo registro al archivo indexado
      * leyendo los datos del usuario a través de la consola.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE  ASSIGN TO 'CUENTAS-INDEXADAS.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
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
           05 WS-CONTINUE-FLAG  PIC X VALUE 'Y'.
              88 CONTINUE-ADDING VALUE 'Y'.

       01  WS-INPUT-FIELDS.
           05 WS-INPUT-BALANCE  PIC 9(8)V99.
           05 WS-INPUT-NAME     PIC X(30).

       PROCEDURE DIVISION.
           PERFORM 000-INICIO-SIST

      * El bucle solo se ejecuta si la apertura fue exitosa ('Y')
           PERFORM 100-PROCESAR-ADICIONES
               UNTIL WS-CONTINUE-FLAG NOT = 'Y'

           PERFORM 900-FINALIZAR-SIST
           STOP RUN.

       000-INICIO-SIST.
           MOVE 'Y' TO WS-CONTINUE-FLAG

           OPEN I-O MASTER-FILE

           IF WS-MASTER-STATUS NOT = '00'
               DISPLAY '*** ERROR CRITICO AL ABRIR ARCHIVO INDEXADO ***'
               DISPLAY 'FILE STATUS: ' WS-MASTER-STATUS
               DISPLAY 'EL PROGRAMA FINALIZARA.'
               MOVE 'N' TO WS-CONTINUE-FLAG
           END-IF.

       100-PROCESAR-ADICIONES.
           DISPLAY '-------------------------------------------'
           DISPLAY 'INTRODUZCA DATOS DE LA NUEVA CUENTA :'

      * 1. Solicitar Número de Cuenta (Clave) - CLÁUSULA REMOVIDA
           DISPLAY 'No. de Cuenta (10 dígitos): '
           ACCEPT MR-CUENTA-NUM

      * 2. Solicitar Nombre - CLÁUSULA REMOVIDA
           DISPLAY 'Nombre (Max 30 caracteres): '
           ACCEPT WS-INPUT-NAME

      * 3. Solicitar Saldo Inicial - CLÁUSULA REMOVIDA
           DISPLAY 'Saldo Inicial (Formato XXXXXXX.XX): '
           ACCEPT WS-INPUT-BALANCE

      * Mover datos de entrada a la estructura del archivo maestro
           MOVE WS-INPUT-NAME TO MR-NOMBRE
           MOVE WS-INPUT-BALANCE TO MR-BALANCE

           PERFORM 200-WRITE-NEW-RECORD

      * Pregunta de continuación - CLÁUSULA REMOVIDA
           DISPLAY '¿Desea añadir otra cuenta? (Y/N): '
           ACCEPT WS-CONTINUE-FLAG.

       200-WRITE-NEW-RECORD.
           WRITE MASTER-RECORD
               INVALID KEY
                   IF WS-MASTER-STATUS = '22'
                       DISPLAY 'ERROR: La cuenta ' MR-CUENTA-NUM
                               ' YA EXISTE (STATUS 22).'
                   ELSE
                       DISPLAY
                       'ERROR AL ESCRIBIR REGISTRO. STATUS: '
                       WS-MASTER-STATUS
                   END-IF
               NOT INVALID KEY
                   DISPLAY '-> CUENTA ' MR-CUENTA-NUM
                   ' AGREGADA EXITOSAMENTE.'
           END-WRITE.

       900-FINALIZAR-SIST.
           CLOSE MASTER-FILE.
           DISPLAY '--- PROGRAMA DE ADICION FINALIZADO ---'.
