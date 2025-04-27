       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmployeeSalaryCalculator.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 EMPLOYEE-NAME       PIC A(30).
       01 BASIC-SALARY        PIC 9(6)V99.
       01 BONUS               PIC 9(5)V99.
       01 GROSS-SALARY        PIC 9(7)V99.

       PROCEDURE DIVISION.
       DISPLAY "Enter Employee Name: ".
       ACCEPT EMPLOYEE-NAME.

       DISPLAY "Enter Basic Salary: ".
       ACCEPT BASIC-SALARY.

       DISPLAY "Enter Bonus Amount: ".
       ACCEPT BONUS.

       COMPUTE GROSS-SALARY = BASIC-SALARY + BONUS.

       DISPLAY "Employee Name: " EMPLOYEE-NAME.
       DISPLAY "Gross Salary : " GROSS-SALARY.

       STOP RUN.
