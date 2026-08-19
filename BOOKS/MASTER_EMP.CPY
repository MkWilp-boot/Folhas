       01 REG-EMPLOYEE.
         05 FUNC-ID            PIC 9(6).
         05 FUNC-NAME          PIC X(40).
         05 FUNC-DEP           PIC X(10).
         05 FUNC-ROLE          PIC X(20).
         05 FUNC-BASE-SALARY   PIC 9(7)V99 COMP-3.
         05 FUNC-STARTED-DATE  PIC 9(8).
         05 FUNC-STATUS        PIC X(01).
           88 FUNC-ACTIVE      VALUE "A".
           88 FUNC-INACTIVE    VALUE "I".
