/** Represents an expression
 */
public class Expression {

	private static final char[] ops = {'+', '-', '*', ':', '>'};

    // The operands and operator of this expression
    private Fraction op1;
    private Fraction op2;
    private char op;
  
    /** Constructs an expression.
     *  @param numerator    can be signed
     *  @param denominator  can be signed
     */
    public Expression (Fraction f1, Fraction f2, char op) {
        op1 = f1;
        op2 = f2;
        this.op = op;
    }

    /** Constructs a random expression.
     *  A random expression consists of two random fractions, each with the given 
     *  limit, and a randomly selected operation which is either +, -, *, /, or >.
     *  @param limit must be at least 2
     */
    public Expression (int limit) {
       this.op1 = new Fraction(limit);
       this.op2 = new Fraction(limit);
       this.op = ops[(int) (Math.random() * ops.length)];  
    }

    /** Returns the first operand of this expression.
     *  @return the first operand of this expression (a fraction)
     */
    public Fraction getOp1() {
        return op1;
    }

    /** Returns the second operand of this expression.
     *  @return the second operand of this expression (a fraction)
     */
    public Fraction getOp2() {
        return op2;
    }

    /** Returns the operator of this expression.
     *  @return the second opetand of this expression (a fraction)
     */
    public char getOp() {
        return op;
    }

    /** Returns the value of this expression, as a fraction.
     *  If the operator (op) is '>', returns op1 minus op2, as a fraction.  
     *  @return the value of this expression, as fraction.
     */
    public Fraction value() {
        switch(this.op) {
            case '+':
                return this.op1.add(this.op2);
            case '-':
                return this.op1.subtract(this.op2);
            case '*':
                return this.op1.multiply(this.op2);
            case ':':
                return this.op1.divide(this.op2);
            case '>':
                return this.op1.subtract(this.op2);
        }
        return null;
    }

    /** Returns an expression that serves as a hint for this expression.
     *  Note: Multiplication expressions have no hints.
     *  @return an expression that has the same value as this expression
     */
    public Expression hint() {
        if (this.op1.getDenominator() == this.op2.getDenominator()) {
            return new Expression(this.op1, this.op2, this.op);
        }
        switch(this.op) {
            case '+':
                return addHint();
            case '-':
                return subHint();
            case ':':
                return divHint();
            case '>':
                return compHint();
        }
        return null;
    }

    /** Returns an expression that serves as a hint for solving this addition expression. 
     *  For example, the hint for 1/2 + 2/3 is 3/6 + 4/6.
     *  @return an expression that has the same value as this expression
     */
    public Expression addHint() {
        int comDenom = MyMath.commonDenominator(op1, op2);
        int f1Num = this.op1.getNumerator()*(comDenom / this.op1.getDenominator());
        int f2Num = this.op2.getNumerator()*(comDenom / this.op2.getDenominator());
        Fraction op1H = new Fraction(f1Num, comDenom);
        Fraction op2H = new Fraction(f2Num, comDenom);
        Expression addH = new Expression(op1H, op2H, this.op);
        
        return new Expression(addH.op1, addH.op2, this.op);
    }

    /** Returns an expression that serves as a hint for solving this subtraction expression. 
     *  For example, the hint for 1/2 - 2/3 is 3/6 - 4/6.
     *  @return an expression that has the same value as this expression
     */
    public Expression subHint() {
        int comDenom = MyMath.commonDenominator(op1, op2);
        int f1Num = this.op1.getNumerator()*(comDenom / this.op1.getDenominator());
        int f2Num = this.op2.getNumerator()*(comDenom / this.op2.getDenominator());
        Fraction op1H = new Fraction(f1Num, comDenom);
        Fraction op2H = new Fraction(f2Num, comDenom);
        Expression addH = new Expression(op1H, op2H, this.op);
        
        return new Expression(addH.op1, addH.op2, this.op);
    }

    /** Returns an expression that serves as a hint for solving this division expression. 
     *  For example, the hint for 1/2 : 2/3 is 1/2 * 3/2.
     *  @return a multiplication expression that has the same value as this expression
     */
    public Expression divHint() {
        return new Expression(this.op1, this.op2.invert(), '*');
    }

    /** Returns an expression that serves as a hint for solving this comparison expression. 
     *  For example, the hint for 1/2 > 2/3 is 1/2 - 2/3.
     *  @return a subtraction expression
     */
    public Expression compHint() {
        return new Expression(this.op1, this.op2, '-');
    }

    /** Returns a string representation of this expression.
     *  If the operand is negative, encloses the operand with parentheses.
     *  For example, if op1 = 1/5, op2 = -1/3, and op = "-", returns "1/5 - (-1/3)".
     *  @return a string representation of this expression.
     */
    public String toString () {
        if (this.op2.signum() == -1) {
            return this.op1 + " " + this.op + " (" + this.op2 + ")";
        } else if (this.op1.signum() == -1) {
            return "(" + this.op1 + ") " + this.op + " " + this.op2;
        }  else if (this.op1.signum() == -1 && this.op2.signum() == -1) {
            return "(" + this.op1 + ") " + this.op + " (" + this.op2 + ")";
        } else {
            return this.op1 + " " + this.op + " " + this.op2;
        }
    }
}