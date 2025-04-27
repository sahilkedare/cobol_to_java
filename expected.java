import java.util.Scanner;

public class SalaryCalculatorApp {

    public static void main(String[] args) {
        Scanner inputScanner = new Scanner(System.in);

        System.out.print("Enter Name of Employee: ");
        String empName = inputScanner.nextLine();

        System.out.print("Enter Basic Pay: ");
        double basicPay = inputScanner.nextDouble();

        System.out.print("Enter Bonus Pay: ");
        double bonusPay = inputScanner.nextDouble();

        double totalSalary = basicPay + bonusPay;

        System.out.println("\nEmployee: " + empName);
        System.out.println("Total Salary: " + totalSalary);

        inputScanner.close();
    }
}
