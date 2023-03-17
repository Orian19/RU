// Demonstrates the Collatz conjecture. */
public class Collatz {
	public static void main(String args[]) {
	    int seed = Integer.parseInt(args[0]);
	    String mode = args[1];
	    int curNum = 0;

	    for (int i = 1; i <= seed; i++) {
	    	int stepsNum = 1; 
	    	curNum = i;
	    	if (mode.equals("v")) {
	    		System.out.print(curNum);
	    	}

	    	do {
	    		if (curNum % 2 == 0) {
	    			curNum /= 2;
	    		} else {
	    			curNum = (curNum * 3) + 1;
	    		}
	    		stepsNum++;
	    		if (mode.equals("v")) {
					System.out.print(" " + curNum);
	    		}
	    	} while (curNum != 1);

	    	if (mode.equals("v")) {
	    		System.out.print(" (" + stepsNum + ")");
	    		System.out.println();
	    	}
	    }
	    System.out.println("The first " + seed + " hailstone sequences reached 1.");
	}
}
