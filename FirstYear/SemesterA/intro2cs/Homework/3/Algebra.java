// Represents algebraic operations as functions.
public class Algebra {
	public static void main(String args[]) {
	    // Some tests of the class functions
	    System.out.println(plus(2,3));           // 2 + 3
	    System.out.println(plus(8,7));           
	    System.out.println(plus(0,0));           
	    System.out.println(plus(1,0));           
	    System.out.println(plus(0,2));           
	    System.out.println(minus(7,2));          // 7 - 2    
	    System.out.println(minus(3,0));              
	    System.out.println(minus(0,0));              
 		System.out.println(times(3,4));          // 3 * 4
 		System.out.println(times(0,4));          
 		System.out.println(times(4,0));          
 		System.out.println(times(4,1));          
 		System.out.println(times(1,1));          
 		System.out.println(div(5,3));            // 5 / 3    
 		System.out.println(div(12,3));               
 		System.out.println(div(0,3));                
   		System.out.println(div(3,5));            // 3 / 5
   		System.out.println(div(4,4));            // 4 / 4
   		System.out.println(div(14,7));           // 14 / 7
   		System.out.println(mod(5,3));            // 5 % 3    
   		System.out.println(mod(20,10));             
   		System.out.println(mod(28,8));             
   		System.out.println(pow(5,4));            // 5 ^ 4
   		System.out.println(pow(2,3));         
   		System.out.println(pow(5,0));            
   		System.out.println(pow(0,0));            
   		System.out.println(pow(0,2));            
   		System.out.println(sqrt(36));            // sqrt(36)
   		System.out.println(sqrt(9));            
   		System.out.println(sqrt(5));                       
   		System.out.println(sqrt(1));            
   		System.out.println(sqrt(100));            
   		System.out.println(sqrt(2));            
   		System.out.println(sqrt(3));            
   		System.out.println(sqrt(81));            
   		System.out.println(sqrt(76123));         // sqrt(76123)
   		System.out.println(times(2,plus(4,3)));  // 2 * (4 + 3)
   		int b = 5, c = 3;
   		System.out.println(minus(pow(b,2),times(4,c))); // b * b - 4 * c
	}  

	// Returns x1 + x2.
	// Assumption: x1 and x2 are nonnegative.
	public static int plus(int x1, int x2) {
		int addition = x1;
		for (int i = 0; i < x2; i++) {
			addition++;
		}
		return addition;
	}

	// Returns x1 - x2.
	// Assumption: x1 and x2 are nonnegative, and x1 >= x2.
	public static int minus(int x1, int x2) {
		int substraction = x1;
		for (int i = 0; i < x2; i++) {
			substraction--;
		}
		return substraction;
	}

	// Returns x1 * x2.
	// Assumption: x1 and x2 are nonnegative.
	public static int times(int x1, int x2) {
        int multiplication = 0;
        for (int i = 0; i < x2; i++) {
        	multiplication =  plus(x1, multiplication);
        }
        return multiplication;
	}

	// Returns x^n.
	// Assumption: x and n are nonnegative.
	public static int pow(int x, int n) {
		int power = 1;
		for (int i = 0; i < n; i++) {
			power = times(power, x);
		}
		return power;
	}

	// Returns x1 / x2 (integer division). 
	// Assumption: x1 is nonnegative, x2 is positive.
	public static int div(int x1, int x2) {
       int division = 0;
       while (x1 >= x2) {
       	x1 = minus(x1, x2);
       	division++;
       }
       return division;
	}

	// Returns x1 % x2
	// Assumption: x1 is nonnegative, x2 is positive.
	public static int mod(int x1, int x2) {
        int modulo = x1 - times(div(x1, x2), x2);
        return modulo;
	}	

	// Returns the integer part of sqrt(x) 
	// Assumption: x >= 1.
	public static int sqrt(int x) {
        int intSquart = 1;
        while (pow(intSquart, 2) < x - 1) {
        	intSquart++;
        }
        return intSquart;
	}	  	  
}
