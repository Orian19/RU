/** A library of commonly used mathematical operations.
 */
public class MyMath {

    // For each one of the three method names listed below, 
    // write the method signature, document the method,
    // and write its implementation.
    // The documentation should be done in JavaDoc.
    // Use the same documentation style we used in Fraction.java. 

    /** gcd */
        /** Returns the greatest common divisor of two positive numbers.
     * Uses Euclid's algorithm.
     */
    public static int gcd(int x, int y) {

        while (y != 0) {
            int rem = x % y;
            x = y;
            y = rem;     
        }
        return x;
    }

    /** commonDenominator */
    public static int  commonDenominator(Fraction f1, Fraction f2) {
        int d1 = f1.getDenominator();
        int d2 = f2.getDenominator();

        if (d1 % d2 == 0) {
            return d1;
        } else if (d2 % d1 == 0) {
            return d2;
        } else {
            return d1 * d2;
        }
    }

    /** max */
    public static Fraction max(Fraction f1, Fraction f2) {
        int compRes = f1.compareTo(f2);

        if (compRes == 1) {
            return f1;
        } else if (compRes == -1) {
            return f2;
        } else {
            return f2;
        }
    }

}