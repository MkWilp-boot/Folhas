       01 REG-REJECTED.
         05 REJ-ID                        PIC 9(6).
         05 REJ-PROCESS-DATE              PIC 9(8).
         05 REJ-REASON-COD                PIC 9(3).
           88 RES-NOT-FOUND               VALUE 001.
           88 RES-ABSENTE-TOO-MANY-TIMES  VALUE 002.
           88 RES-INVALID-HOURS           VALUE 003.
           88 RES-EMP-INACTIVE            VALUE 004.
         05 REJ-DESCRICAO-MOTIVO          PIC X(50).
