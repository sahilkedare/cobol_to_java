       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 NUM1         PIC 9(5)V99.
       77 NUM2         PIC 9(5)V99.
       77 RESULT       PIC 9(7)V99.
       77 WS-DIV-ZERO  PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "====================================".
           DISPLAY "          SIMPLE COBOL CALCULATOR   ".
           DISPLAY "====================================".

           DISPLAY "Enter first number: " WITH NO ADVANCING.
           ACCEPT NUM1.
           
           DISPLAY "Enter second number: " WITH NO ADVANCING.
           ACCEPT NUM2.

           DISPLAY "------------------------------------".

           * Addition
           ADD NUM1 TO NUM2 GIVING RESULT.
           DISPLAY "Addition       : " RESULT.

           * Subtraction
           SUBTRACT NUM2 FROM NUM1 GIVING RESULT.
           DISPLAY "Subtraction    : " RESULT.

           * Multiplication
           MULTIPLY NUM1 BY NUM2 GIVING RESULT.
           DISPLAY "Multiplication : " RESULT.

           * Division (check divide by zero)
           IF NUM2 = 0
              MOVE 'Y' TO WS-DIV-ZERO
           END-IF.

           IF WS-DIV-ZERO = 'Y'
              DISPLAY "Division       : ERROR (Divide by Zero)"
           ELSE
              DIVIDE NUM1 BY NUM2 GIVING RESULT
              DISPLAY "Division       : " RESULT
           END-IF.

           DISPLAY "------------------------------------".
           DISPLAY "Program finished.".
           STOP RUN.
