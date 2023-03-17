// Splits a restaurant bill evenly among three diners.
public class Bill3 {
	public static void main(String[] args) {
		// get user input
	    String name1 = args[0];
	    String name2 = args[1];
	    String name3 = args[2];
	    int bill = Integer.parseInt(args[3]);

	    // calculate the amount each diner as to pay
	    double splitAmount = Math.ceil(bill / 3.0);

	    // print the amount each diner has to pay
	    System.out.println("Dear " + name3 + ", " + name2 + ", and " + name1 + 
	    	": pay " + splitAmount + " Shekels each.");
	}
}
