import java.util.Scanner;

public class StudentGradeProcessor {

    public static void main(String[] args) {
        Scanner inputScanner = new Scanner(System.in);

        System.out.print("Enter Student Name: ");
        String fullName = inputScanner.nextLine();

        System.out.print("Enter Marks for Subject 1: ");
        int mathScore = inputScanner.nextInt();

        System.out.print("Enter Marks for Subject 2: ");
        int scienceScore = inputScanner.nextInt();

        System.out.print("Enter Marks for Subject 3: ");
        int englishScore = inputScanner.nextInt();

        int cumulativeMarks = mathScore + scienceScore + englishScore;
        double averageScore = (double) cumulativeMarks / 3;

        char finalGrade;
        if (averageScore >= 90) {
            finalGrade = 'A';
        } else if (averageScore >= 75) {
            finalGrade = 'B';
        } else if (averageScore >= 60) {
            finalGrade = 'C';
        } else {
            finalGrade = 'F';
        }

        System.out.println("\nStudent Name: " + fullName);
        System.out.println("Total Marks: " + cumulativeMarks);
        System.out.println("Average Marks: " + averageScore);
        System.out.println("Final Grade: " + finalGrade);

        inputScanner.close();
    }
}
