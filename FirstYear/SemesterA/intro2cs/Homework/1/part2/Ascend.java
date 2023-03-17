// Generates three integer random numbers in a given range,
// and prints them in increasing order.
public class Ascend {
	public static void main(String[] args) {
		// get user input
		int lim = Integer.parseInt(args[0]);

		// generate 3 random integers in range [0, lim)
		int a = (int) (Math.random() * lim);
		int b = (int) (Math.random() * lim);
		int c = (int) (Math.random() * lim);

		// find order of a,b,c
		int minValue = 0;
		int maxValue = 0;
		int midValue = 0;

		// find min
		minValue = Math.min(a, b);
		minValue = Math.min(minValue, c);

		// find max
		maxValue = Math.max(a, b);
		maxValue = Math.max(maxValue, c);

		// find mid
		midValue = a + b + c - maxValue - minValue;

		// print ascending order
		System.out.println(a + " " + b + " " + c);
		System.out.println(minValue + " " + midValue + " " + maxValue);
	}
}
