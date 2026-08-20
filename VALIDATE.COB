       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             VALIDATE.
       AUTHOR.                 JOAO FERNANDES.
       DATE-WRITTEN.           19/08/2026.
       DATE-COMPILED.          19/08/2026.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       SPECIAL-NAMES.
         DECIMAL-POINT         IS COMMA.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
         SELECT EARQ-EMP 
           ASSIGN TO ".\DATAS\EARQ_EMPLOYEES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-EARQ-EMP.

         SELECT EARQ-MOVES 
           ASSIGN TO ".\DATAS\EARQ_MOVES.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-EARQ-MOVES.

         SELECT SARQ-REJECTED-REG 
           ASSIGN TO ".\SARQ_REJECTED_REG.TXT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SARQ-REJECTED-REG.

         SELECT SARQ-VALIDATED-EMP
           ASSIGN TO ".\SARQ_VALIDATED_EMP.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SARQ-VALIDATED-EMP.

       DATA                    DIVISION.
       FILE                    SECTION.
      * LAYOUT IS 92 POSITIONS
         FD EARQ-EMP.
           COPY ".\BOOKS\MASTER_EMP".
      * LAYOUT IS 19 POSITIONS
         FD EARQ-MOVES.
           COPY ".\BOOKS\MOVES".
         FD SARQ-REJECTED-REG.
       01 F-FORMATTED-REJECTION PIC X(75) VALUES SPACES.
      * LAYOUT IS 92 POSITIONS
         FD SARQ-VALIDATED-EMP.
           COPY ".\BOOKS\VALIDATED_EMP".

       WORKING-STORAGE         SECTION.
       01 REG-REJECTED.
         05 REJ-ID                        PIC 9(6).
         05 REJ-PROCESS-DATE              PIC 9(8).
         05 REJ-REASON-COD                PIC 9(3).
           88 RES-NOT-FOUND               VALUE 001.
           88 RES-ABSENTE-TOO-MANY-TIMES  VALUE 002.
           88 RES-INVALID-HOURS           VALUE 003.
           88 RES-EMP-INACTIVE            VALUE 004.
         05 REJ-REASON-DESC          PIC X(50).

      * FILE STATUS VARIABLES
       01 FS-EARQ-EMP              PIC 9(02).
       01 FS-EARQ-MOVES            PIC 9(02).
       01 FS-SARQ-REJECTED-REG     PIC 9(02).
       01 FS-SARQ-VALIDATED-EMP    PIC 9(02).
      
      * MASTER EMPLOYEE FILE CONTROL
       01 WS-END-EARQ-EMP          PIC X(01).
         88 END-EARQ-EMP           VALUE 'Y'.
         88 NOT-END-EARQ-EMP       VALUE 'N'.

       01 WS-END-EARQ-MOVE         PIC X(01).
         88 END-EARQ-MOVE          VALUE 'Y'.
         88 NOT-END-EARQ-MOVE      VALUE 'N'.

      * FILE HANDLING ERRORS 
       01 WS-ERR-DESC      PIC A(30) VALUES SPACES.
       01 WS-ERR-STATUS    PIC 9(02) VALUE ZERO.

      * TABLES
       01 WS-EMP-TABLE.
         05 WS-EMP-COUNT       PIC 9(3) VALUE ZERO.
         05 WS-EMP-ENTRY       OCCURS 1 TO 500 TIMES
                               DEPENDING ON WS-EMP-COUNT
                               ASCENDING KEY IS TB-ID
                               INDEXED BY TB-IDX.
           10 TB-ID            PIC 9(06).
           10 TB-NAME          PIC X(40).
           10 TB-DEP           PIC X(10).
           10 TB-ROLE          PIC X(20).
           10 TB-BASE-SALARY   PIC 9(7)V99 COMP-3.
           10 TB-STARTED-DATE  PIC 9(8).
           10 TB-STATUS        PIC X(01).
             88 EMP-ACTIVE      VALUE "A".
             88 EMP-INACTIVE    VALUE "I".

       PROCEDURE               DIVISION.
       0100-MAIN               SECTION.
           PERFORM 0200-INIT.
           PERFORM 0300-PROCESS.
           PERFORM 0400-FINALIZER.
           GOBACK.
       0100-END.               EXIT.
       0200-INIT               SECTION.
           OPEN INPUT  EARQ-EMP.
           OPEN INPUT  EARQ-MOVES.
           OPEN OUTPUT SARQ-REJECTED-REG.
           OPEN OUTPUT SARQ-VALIDATED-EMP.

           PERFORM 0201-VALIDATE-INPUT.
           PERFORM 0201-VALIDATE-OUTPUT.
       0200-END.               EXIT.
       0201-VALIDATE-INPUT     SECTION.
           IF FS-EARQ-EMP NOT EQUALS ZERO
             MOVE "MASTER EMPLOYEES FILE" TO WS-ERR-DESC
             MOVE FS-EARQ-EMP             TO WS-ERR-STATUS
             PERFORM 0210-INIT-FAIL
           END-IF
           
           IF FS-EARQ-MOVES NOT EQUALS ZERO
             MOVE "MOVES FOR EMPLOYEES FILE" TO WS-ERR-DESC
             MOVE FS-EARQ-MOVES              TO WS-ERR-STATUS
             PERFORM 0210-INIT-FAIL
           END-IF.
       0201-END.               EXIT.
       0201-VALIDATE-OUTPUT    SECTION.
           IF FS-SARQ-REJECTED-REG NOT EQUALS ZERO
             MOVE "REJECTED REGISTERS" TO WS-ERR-DESC
             MOVE FS-EARQ-EMP          TO WS-ERR-STATUS
             PERFORM 0210-INIT-FAIL
           END-IF
           
           IF FS-SARQ-VALIDATED-EMP NOT EQUALS ZERO
             MOVE "VALIDATED EMPLOYEES"    TO WS-ERR-DESC
             MOVE FS-EARQ-MOVES            TO WS-ERR-STATUS
             PERFORM 0210-INIT-FAIL
           END-IF.
       0201-END.               EXIT.
       0210-INIT-FAIL          SECTION.
           DISPLAY "* ERROR"
           DISPLAY "** COULD NOT OPEN " WS-ERR-DESC
           DISPLAY "** STATUS CODE WAS " WS-ERR-STATUS
           PERFORM 0400-FINALIZER
           GOBACK.
       0210-END.               EXIT.
       0300-PROCESS            SECTION.
           PERFORM UNTIL END-EARQ-EMP
             READ EARQ-EMP
               AT END SET END-EARQ-EMP TO TRUE
               NOT AT END PERFORM 0310-FEED-EMP-TABLE
           END-PERFORM.
           PERFORM 0320-READ-MOVES.
       0300-END.               EXIT.
       0310-FEED-EMP-TABLE     SECTION.
           ADD 1 TO WS-EMP-COUNT OF WS-EMP-TABLE.
      * MOVING FIELDS TO TABLE
             MOVE EMP-ID OF REG-EMPLOYEE
               TO TB-ID(WS-EMP-COUNT).
             MOVE EMP-NAME OF REG-EMPLOYEE
               TO TB-NAME(WS-EMP-COUNT).
             MOVE EMP-DEP OF REG-EMPLOYEE
               TO TB-DEP(WS-EMP-COUNT).
             MOVE EMP-ROLE OF REG-EMPLOYEE
               TO TB-ROLE(WS-EMP-COUNT).
             MOVE EMP-BASE-SALARY OF REG-EMPLOYEE
               TO TB-BASE-SALARY(WS-EMP-COUNT).
             MOVE EMP-STARTED-DATE OF REG-EMPLOYEE
               TO TB-STARTED-DATE(WS-EMP-COUNT).
             MOVE EMP-STATUS OF REG-EMPLOYEE
               TO TB-STATUS(WS-EMP-COUNT).
       0310-END.               EXIT.
       0320-READ-MOVES         SECTION.
           PERFORM UNTIL END-EARQ-MOVE
             READ EARQ-MOVES
               AT END SET END-EARQ-MOVE TO TRUE
               NOT AT END
                 SEARCH ALL WS-EMP-ENTRY
                   AT END
                     SET RES-NOT-FOUND TO TRUE
                     PERFORM 0330-WRITE-REJECTED
                   WHEN TB-ID(TB-IDX) EQUALS MOV-ID
                     PERFORM 0340-WRITE-VALID-EMP
                 END-SEARCH
           END-PERFORM.
       0320-END.               EXIT.
       0330-WRITE-REJECTED     SECTION.
           MOVE MOV-ID TO REJ-ID.
           EVALUATE TRUE
             WHEN RES-NOT-FOUND
               MOVE "FUNCIONARIO NAO ENCONTRADO NO MESTRE"
               TO REJ-REASON-DESC
             WHEN RES-ABSENTE-TOO-MANY-TIMES
               MOVE "FUNCIONARIO AUSENTE MUITAS VEZES (2+)"
               TO REJ-REASON-DESC
             WHEN RES-INVALID-HOURS
               MOVE "FUNCIONARIO COM HORAS INVALIDAS"
               TO REJ-REASON-DESC
             WHEN RES-EMP-INACTIVE
               MOVE "FUNCIONARIO ATUALMENTE INATIVO"
               TO REJ-REASON-DESC
             WHEN OTHER
               MOVE "ERRO INESPERADO"
               TO REJ-REASON-DESC
           END-EVALUATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO REJ-PROCESS-DATE.
           STRING
             "MATRICULA: "        DELIMITED BY SIZE
             REJ-ID               DELIMITED BY SIZE
             " MOTIVO: "          DELIMITED BY SIZE
             REJ-REASON-DESC      DELIMITED BY SIZE
           INTO F-FORMATTED-REJECTION.
           WRITE F-FORMATTED-REJECTION.
       0330-END.               EXIT.
       0340-WRITE-VALID-EMP    SECTION.
           DISPLAY "VALID".
       0340-END.               EXIT.
       0400-FINALIZER          SECTION.
           CLOSE EARQ-EMP 
                 EARQ-MOVES.
           CLOSE SARQ-REJECTED-REG
                 SARQ-VALIDATED-EMP.
       0400-END.               EXIT.
