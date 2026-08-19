       01 VALID-EMPLOYEE.
         05 EMP-ID            PIC 9(6).
         05 EMP-NAME          PIC X(40).
         05 EMP-DEP           PIC X(10).
         05 EMP-ROLE          PIC X(20).
         05 EMP-BASE-SALARY   PIC 9(7)V99 COMP-3.
         05 EMP-STARTED-DATE  PIC 9(8).
         05 EMP-STATUS        PIC X(01).
           88 EMP-ACTIVE      VALUE "A".
           88 EMP-INACTIVE    VALUE "I".
