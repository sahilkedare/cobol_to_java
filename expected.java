import java.util.Random;
import java.util.Scanner;

public class MysticOracle {

    public static void main(String[] args) {
        // Fun variable names
        Random magicDice = new Random();
        Scanner timeMachine = new Scanner(System.in);
        int cosmicNumber;
        String whisperFromBeyond;

        System.out.println("🔮 Welcome to the Mystic Oracle 🔮");
        System.out.println("Press ENTER to summon your destiny...");
        timeMachine.nextLine(); // Waiting for user

        cosmicNumber = magicDice.nextInt(5) + 1; // Random number between 1 and 5

        switch (cosmicNumber) {
            case 1:
                whisperFromBeyond = "✨ Today, the universe will grant you a missing semicolon at the perfect moment.";
                break;
            case 2:
                whisperFromBeyond = "☕ A mystical cup of coffee shall unlock great wisdom for you today.";
                break;
            case 3:
                whisperFromBeyond = "🐞 A wild bug will disappear mysteriously under your mighty gaze.";
                break;
            case 4:
                whisperFromBeyond = "🎯 Your code will compile flawlessly — even the compiler will be impressed!";
                break;
            case 5:
                whisperFromBeyond = "📅 An unexpected meeting will turn into a golden opportunity.";
                break;
            default:
                whisperFromBeyond = "🌫️ The future is shrouded in mist. Have another coffee and try again.";
        }

        System.out.println("\n🧿 Your Fortune:");
        System.out.println(whisperFromBeyond);

        timeMachine.close();
    }
}
