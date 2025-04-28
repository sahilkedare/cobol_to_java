       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALARY-CALCULATOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 EMPLOYEE-NAME        PIC A(30).
       01 BASIC-SALARY         PIC 9(5)V99.
       01 BONUS                PIC 9(4)V99.
       01 GROSS-SALARY         PIC 9(6)V99.
       01 I                    PIC 9(2) VALUE 1.

       01 SALARIES.
           05 SALARY-TABLE OCCURS 5 TIMES.
               10 SALARY-VALUE     PIC 9(5)V99.

       01 AVG-SALARY           PIC 9(5)V99.
       01 TOTAL-SALARY         PIC 9(6)V99 VALUE 0.

       PROCEDURE DIVISION.

       DISPLAY "Enter Employee Name: ".
       ACCEPT EMPLOYEE-NAME.

       DISPLAY "Enter Basic Salary: ".
       ACCEPT BASIC-SALARY.

       DISPLAY "Enter Bonus: ".
       ACCEPT BONUS.

       COMPUTE GROSS-SALARY = BASIC-SALARY + BONUS.
       DISPLAY "Gross Salary is: " GROSS-SALARY.

       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
           MOVE GROSS-SALARY TO SALARY-VALUE(I)
           COMPUTE TOTAL-SALARY = TOTAL-SALARY + SALARY-VALUE(I)
       END-PERFORM.

       COMPUTE AVG-SALARY = TOTAL-SALARY / 5.
       DISPLAY "Average Salary over 5 months is: " AVG-SALARY.

       STOP RUN.
