// Computes an approximation of PI.
public class CalcPi {
    public static void main(String args[]) { 
		int termsNum = Integer.parseInt(args[0]);
		double sum = 1.0;
		double minPlus = -1.0;
		double denom = 3.0;

		for (int i = 1; i < termsNum; i++) {
			sum = sum + (minPlus * (1 / denom));
			minPlus *= -1;
			denom += 2;
		}		
		System.out.println("pi according to Java: " + Math.PI);
		System.out.println("pi, approximated:     " + 4.0 * sum);
	}
}