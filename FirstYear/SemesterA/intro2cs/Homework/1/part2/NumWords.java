// Prints a given number using a hundreds, tens, and units notation.
public class NumWords {
	public static void main(String args[]) {
	    // get user input
	    int num = Integer.parseInt(args[0]);

	    // calculate hundreds, tens, ones
	    int hundreds = num / 100;
	    int tens = (num / 10) % 10;
	    int ones = num % 10;

	    // print number in words
	    System.out.println(hundreds + " hundreds, " + tens + " tens, and " + ones + " ones.");
	}
}
