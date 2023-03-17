/** Tests the Fraction and MyMath methods described in Homework 8.
 */
public class FractionTest {

    // Some representative Fraction objects, used in the various tests written in this class.
    static Fraction half = new Fraction(1,2);
    static Fraction minusHalf = new Fraction(-1,2);
    static Fraction quarter = new Fraction(1,4);
    static Fraction minusQuarter = new Fraction(-1,4);
    static Fraction third = new Fraction(1,3);
    static Fraction twoThirds = new Fraction(2,3);
    static Fraction zero = new Fraction(0,1);

    public static void main(String args[]) {
        // testFraction();
        testAbs();
        testSignum();
        testConvert();
        testSubtract();
        testCompareTo();
        testEquals();
        testPow();
        testRandomFraction();
        testCommonDenominator();
        testMax();
    }

    private static void testFraction() {
        // Use this function for writing code that creates and manipulates Fraction objects.
        // You can use the static Fraction objects declared above, but make sure to practice  
        // creating some Fraction objects of your own.
    }

    private static void testAbs() {
        System.out.println("Testing the abs function:");
        System.out.println("abs(" + half + ") = " + half.abs());
        // Complete the testing code here.
        System.out.println("abs(" + minusHalf + ") = " + minusHalf.abs());
        System.out.println("abs(" + zero + ") = " + zero.abs());
        System.out.println();
    }

    private static void testSignum() {
        System.out.println("Testing the signum function:");
        System.out.println("signum(" + half + ") = " + half.signum());
        System.out.println("signum(" + minusHalf + ") = " + minusHalf.signum());
        System.out.println("signum(" + zero + ") = " + zero.signum());
        System.out.println();
    }

    private static void testConvert() {
        System.out.println("Testing the convert function:");
        System.out.println(half + " converted is " + half.convert());
        System.out.println(minusHalf + " converted is " + minusHalf.convert());
        System.out.println(zero + " converted is " + zero.convert());
        System.out.println();
    }

    private static void testSubtract() {
        System.out.println("Testing the substract function:");
        System.out.println(half + " - " + half + " = " + half.subtract(half));
        System.out.println(half + " - " + quarter + " = " + half.subtract(quarter));
        System.out.println(quarter + " - " + half + " = " + quarter.subtract(half));
        System.out.println(half + " - " + minusHalf + " = " + half.subtract(minusHalf));
        System.out.println(minusHalf + " - " + half + " = " + minusHalf.subtract(half));
        System.out.println();
    }

    private static void testCompareTo() {
        System.out.println("Testing the compareTo function:");
        System.out.println(half + " compared to " + quarter + " returns " +  "" + half.compareTo(quarter));
        System.out.println(quarter + " compared to " + half + " returns " +  "" + quarter.compareTo(half));
        System.out.println(half + " compared to " + half + " returns " +  "" + half.compareTo(half));
        System.out.println();
    }

    private static void testEquals() {
        System.out.println("Testing the equals function:");
        System.out.println(half + " == " + half + "? " +  half.equals(half));
        System.out.println(half + " == " + quarter + "? " +  half.equals(quarter));
        System.out.println();
    }

    private static void testPow() {
        System.out.println("Testing the power function:");
        System.out.println(twoThirds + " to the power of " + half + " = " + twoThirds.pow(3));
        System.out.println();
    }

    private static void testRandomFraction() {
        System.out.println("Generating 10 random fractions with limit = 7:");
        for (int i = 0; i < 10; i++) {
            System.out.println(new Fraction(7));
        }
        System.out.println();
    }

    private static void testCommonDenominator() {
        System.out.println("Testing the commonDenminator function:");
        System.out.println("The common denominator of " + half + " and " + quarter + " is " + MyMath.commonDenominator(half, quarter));
        System.out.println("The common denominator of " + quarter + " and " + half + " is " + MyMath.commonDenominator(quarter, half));
        System.out.println("The common denominator of " + third + " and " + third + " is " + MyMath.commonDenominator(third, third));
        System.out.println("The common denominator of " + third + " and " + half + " is " + MyMath.commonDenominator(third, half));
        System.out.println();
    }

    private static void testMax() {
        System.out.println("Testing the max function:");
        System.out.println("max(" + half + "," + quarter + ") = " + MyMath.max(half, quarter));
        System.out.println("max(" + quarter + "," + half + ") = " + MyMath.max(quarter, half));
        System.out.println("max(" + half + "," + half + ") = " + MyMath.max(half, half));
        System.out.println();
    }    
}