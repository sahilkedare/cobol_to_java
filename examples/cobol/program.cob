       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-UPDATE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-IN ASSIGN TO 'EMPLOYEE-IN.DAT'
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EMP-OUT ASSIGN TO 'EMPLOYEE-OUT.DAT'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMP-IN.
       01 EMP-IN-REC.
           05 EMP-ID        PIC 9(5).
           05 EMP-NAME      PIC X(20).
           05 EMP-SALARY    PIC 9(7)V99.

       FD EMP-OUT.
       01 EMP-OUT-REC.
           05 OUT-ID        PIC 9(5).
           05 OUT-NAME      PIC X(20).
           05 OUT-SALARY    PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       77 WS-EOF           PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT EMP-IN
                OUTPUT EMP-OUT.

           PERFORM UNTIL WS-EOF = 'Y'
              READ EMP-IN INTO EMP-IN-REC
                 AT END MOVE 'Y' TO WS-EOF
              NOT AT END
                 PERFORM PROCESS-RECORD
              END-READ
           END-PERFORM.

           CLOSE EMP-IN EMP-OUT.
           DISPLAY "Processing complete. Check EMPLOYEE-OUT.DAT".
           STOP RUN.

       PROCESS-RECORD.
           MULTIPLY EMP-SALARY BY 1.10 GIVING OUT-SALARY.
           MOVE EMP-ID TO OUT-ID.
           MOVE EMP-NAME TO OUT-NAME.
           WRITE EMP-OUT-REC.
