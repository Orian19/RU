// Prints a crowd cheering output.
public class Cheers {
    public static void main(String[] args) {
   	    String cheerWord = args[0];
        int cheerNum = Integer.parseInt(args[1]);
        String vowels = "aeiouAEIOU";
        String connectWord = "";

        for (int i = 0; i < cheerWord.length(); i++) {
            char curChar = cheerWord.charAt(i);
            if (vowels.indexOf(curChar) == -1) {
                connectWord = "a";
            } else {
                connectWord = "an";
            }
            System.out.println("Give me " + connectWord + " " + curChar + ": " + curChar + "!");
        }

        System.out.println("What does that spell?");

        for (int i = 0; i < cheerNum; i++) {
            System.out.println(cheerWord + "!!!");
        }
    }
}
