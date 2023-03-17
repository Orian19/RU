/* Tests the "quality" of Java's Math.random function.
*/
public  class  TestRandom {
	static int a = 0;
	public static void main(String[]  args) {
		int nTimes = Integer.parseInt(args[0]);

		double ratio = 0.0;
		int moreCount = 0;
		int lessCount = 0;

		for (int i = 0; i < nTimes; i++) {
			double curNum = Math.random();
			if (curNum > 0.5) {
				moreCount++;
			} else {
				lessCount++;
			}
		}
		ratio = (double) moreCount / lessCount;
		
		System.out.println("> 0.5:  " + moreCount + " times.");
		System.out.println("<= 0.5: " + lessCount + " times.");
		System.out.println("Ratio:  " + ratio);

		//why the hell is there a a=0 before the main function?????? 
	}
}
