       IDENTIFICATION DIVISION.
       PROGRAM-ID. FortuneTeller.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 RANDOM-NUMBER         PIC 9 VALUE 0.
       01 FORTUNE-TEXT          PIC A(80).

       PROCEDURE DIVISION.

       DISPLAY "Welcome to the COBOL Fortune Teller!".
       DISPLAY "Press ENTER to reveal your fortune...".
       ACCEPT OMITTED.

       COMPUTE RANDOM-NUMBER = FUNCTION RANDOM * 5 + 1
           GIVING RANDOM-NUMBER.

       EVALUATE RANDOM-NUMBER
           WHEN 1
               MOVE "Today, you will find a missing semicolon at the right time!" TO FORTUNE-TEXT
           WHEN 2
               MOVE "A cup of coffee will be your greatest ally today." TO FORTUNE-TEXT
           WHEN 3
               MOVE "You will debug a stubborn bug without even trying." TO FORTUNE-TEXT
           WHEN 4
               MOVE "Your code will compile on the first try — believe in magic!" TO FORTUNE-TEXT
           WHEN 5
               MOVE "A surprise meeting will actually be useful. Miracles happen!" TO FORTUNE-TEXT
           WHEN OTHER
               MOVE "The future is unclear. Try again after a coffee break." TO FORTUNE-TEXT
       END-EVALUATE.

       DISPLAY "Your Fortune: ".
       DISPLAY FORTUNE-TEXT.

       STOP RUN.
