// Computes the future value of a saving investment.
public class FVCalc {
	public static void main(String[] args){
		// get user input
		int currentValue = Integer.parseInt(args[0]);
		double rate = Double.parseDouble(args[1]);
		int yearsNum = Integer.parseInt(args[2]);

		// calculate future value
		double yearlyRate = Math.pow((1 + (rate / 100)), yearsNum);
		int futureValue = (int) (currentValue * yearlyRate);

		// print future value
		System.out.println("After " + yearsNum + " years, a $" + currentValue + 
			" saved at " + rate + "% will yield $" + futureValue);
	}
}