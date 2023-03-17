/**
 * Performs a sequence of interactive fraction arithmetic exercises.
 */
public class FracPrac {

	public static void main(String[] args) {
		// Inputs the difficulty limit
		// int limit = Integer.parseInt(args[0]);
		int limit = 10;
		System.out.println("Welcome to fractions practice!");
		System.out.println("Here is your first question:");
		Expression exp = new Expression(limit);
		String ans = "";

		while (!ans.equals("quit")) {
			// Prints the question and gets the answer
			System.out.println(exp + " = ?");
			ans = StdIn.readLine();

			switch (ans) {
				case "hint":
					// Prints a hint and repeats the same question
					printHint(exp);
					break;
				case "pass":
					// Prints the correct answer and creates a new question
					printCorrectAnswer(exp);
					System.out.println("New question:");
					System.out.println();
					exp = new Expression(limit);
					break;
				case "quit":
					System.out.println("Bye now!");
					break;
				default:
					// Gets the user's answer. If the user does not enter a valid answer,
					// catches the runtime exception and handles it.
					try {
						if (isCorrect(exp, ans)) {
							System.out.println("Correct! Next question:");
							System.out.println();
							exp = new Expression(limit);
						} else {
							System.out.println("Incorrect... Try again:");
						}
					} catch (NumberFormatException excep) {
						System.out.println("Enter a valid answer.");
					}
			}
		}
	}

	/**
	 * Computes the value of the given expression, reduces the resulting fraction,
	 * and prints the expression and the value.
	 * For example, if exp is 1/2 + 1/6, prints "1/2 + 1/6 = 2/3"
	 * 
	 * @param exp - the expression
	 */
	public static void printCorrectAnswer(Expression exp) {
		Fraction ans = exp.value();
		ans.reduce();
		System.out.println(exp + " = " + ans);
	}

	/**
	 * Checks if the given answer is the value of the given expression.
	 * 
	 * @param exp - the expression
	 * @param ans - the user's input
	 * @return true iff the value of the expression is consistent woth the user's
	 *         input
	 */
	public static boolean isCorrect(Expression exp, String ans) {
		// Distinguishes between two types of expressions: comparisons, and other
		// expressions
		if (exp.getOp() == '>') {
			return isCorrectComp(exp, ans);
		} else {
			return isCorrectAddSubMultDiv(exp, ans);
		}
	}

	/**
	 * Checks if the given answer represents the value of the given expression.
	 * Assumes that the expression is an addition, subtraction, multiplication, or
	 * division.
	 * The answer must be either "int/int" or "int".
	 * 
	 * @param ans - the input string
	 * @param exp - the expression
	 * @return true iff the answer represents the value of the expression.
	 * @throws NumberFormatException - if the answer is neither "int/int" nor "int"
	 */
	public static boolean isCorrectAddSubMultDiv(Expression exp, String ans) {
		Fraction solution = exp.value();
		solution.reduce();
		Fraction userAns = new Fraction(0);
		int startIdx = 0;

		if (ans.charAt(0) == '-') {
			startIdx++;
		}
		if (ans.indexOf("/") == -1) {
			if (startIdx > 0) {
				ans = ans.substring(1);
			}
			userAns = new Fraction(Integer.parseInt(ans), 1);
		} else {
			int slashIdx = ans.indexOf("/");
			userAns = new Fraction(Integer.parseInt(ans.substring(startIdx, slashIdx)),
					Integer.parseInt(ans.substring(slashIdx + 1)));
		}

		userAns.reduce();
		if (userAns.getNumerator() == 0 && solution.getNumerator() == 0) {
			return true;
		} else if (userAns.getNumerator() == solution.getNumerator() &&
				userAns.getDenominator() == solution.getDenominator()) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the given answer, which must be "true" or "false",
	 * represents the value of the given expression.
	 * Assumes that the expression is a comparison.
	 * 
	 * @param exp - the expression
	 * @param b   - the given Boolean value
	 * @return true iff the boolean value solves the expression.
	 */
	public static boolean isCorrectComp(Expression exp, String ans) {
		if (ans.equals("false") && (exp.value().signum() == -1 || exp.value().signum() == 1)){
			return true;
		} 
		if (ans.equals("true") && exp.value().signum() == 1) {
			return true;
		}
		return false;
	}

	/**
	 * Prints a hint for the given expression.
	 * The hint depends on the expression's operator.
	 * 
	 * @param exp - the expression
	 */
	public static void printHint(Expression exp) {
		switch (exp.getOp()) {
			case '+':
				if (exp.getOp1().getDenominator() == exp.getOp2().getDenominator() || exp.getOp1().getNumerator() == 0 || exp.getOp2().getNumerator() == 0) {
					System.out.println("Add up the numerators and divide by the common denominator. Try again:");
					break;
				} else {
					System.out.println("Notice that " + exp + " = " + exp.hint());
					break;
				}
			case '-':
				if (exp.getOp1().getDenominator() == exp.getOp2().getDenominator() || exp.getOp1().getNumerator() == 0 || exp.getOp2().getNumerator() == 0) {
					System.out.println("Add up the numerators and divide by the common denominator. Try again:");
					break;
				} else {
					System.out.println("Notice that " + exp + " = " + exp.hint());
					break;
				}
			case '*':
				System.out.println("Multiply the numerators and divide by the product of the denominators. Try again:");
				break;
			case ':':
				System.out.println("Notice that " + exp + " = " + exp.hint());
				break;
			case '>':
				System.out.println("Notice that if " + exp.hint() + " > 0, the answer must be true. Try again:");
				break;
		}
	}
}
