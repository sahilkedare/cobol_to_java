public class Generated {

    public static void main(String[] args) {
        String employeeName;
        double basicSalary, bonus, grossSalary, avgSalary;
        double[] salaryTable = new double[5];
        double totalSalary = 0;

        java.util.Scanner scanner = new java.util.Scanner(System.in);

        System.out.print("Enter Employee Name: ");
        employeeName = scanner.nextLine();

        System.out.print("Enter Basic Salary: ");
        basicSalary = scanner.nextDouble();

        System.out.print("Enter Bonus: ");
        bonus = scanner.nextDouble();

        grossSalary = basicSalary + bonus;
        System.out.println("Gross Salary is: " + grossSalary);

        for (int i = 0; i < 5; i++) {
            salaryTable[i] = grossSalary;
            totalSalary += salaryTable[i];
        }

        avgSalary = totalSalary / 5;
        System.out.println("Average Salary over 5 months is: " + avgSalary);

        scanner.close();
    }
}