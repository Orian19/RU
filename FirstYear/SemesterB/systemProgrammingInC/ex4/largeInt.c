#include <stdio.h>

/*************************************************
*  largeInt.c
*  ----------
*  A program containing functions for implementing
*  a new large int data type.
*************************************************/

/************************************************/
/* IMPORTANT NOTE:                              */
/* !! MODIFY CODE ONLY IN THE MARKED PLACES !!  */
/************************************************/


/********************************/
/**    SYMBOLIC CONSTANTS      **/
/********************************/
#define NUM_EXP_BITS 5
#define NUM_MAN_BITS (sizeof(int)*8 - NUM_EXP_BITS)

/********************************/
/**    FUNCTION DECLARATIONS   **/
/********************************/

/*********************************
* Problem 1 functions 
* - see documentation in bitOps.c
*********************************/
int getBit(int, int);
int getRight(int, int);
int getLeft(int, int);

/*********************************
* return the value of a given large int
* (in double type)
* THIS FUNCTION IS IMPLEMENTED IN
* THE BOTTOM OF THIS FILE
*********************************/
double largeIntToDouble(int largeInt);

/*********************************
* Problem 2.1
* returns the value of the mantissa
* of a large int
*********************************/
int largeIntMan(int largeInt);

/*********************************
* Problem 2.2
* returns the value of the exponent
* of a large int
*********************************/
int largeIntExp(int largeInt);

/*********************************
* Problem 2.3
* return the large int associated
* with given values of the 
* exponent and mantissa.
*********************************/
int constructLargeInt(int mantissa, int exponent);

/*********************************
* Problem 2.4
* return the large int associated
* with the sum of two large ints
*********************************/
int largeIntSum(int largeInt1, int largeInt2);


/********************************/
/**    FUNCTION DEFINITIONS    **/
/********************************/

/*********************************
* Problem 2.1
* function largeIntMan
* params:
* largeInt (int)
* Return the value of the mantissa associated with the
* large int (largeInt).
* This should be the non-negative integer associated
* with the right-most NUM_MAN_BITS bits in largeInt
*********************************/
int largeIntMan(int largeInt) {
   return getRight(largeInt,NUM_MAN_BITS);      /* returning the mantisa value using getRight function  //** <--- replace the 364 with an appropriate expression (no function calls)     ***/
}

/*********************************
* Problem 2.2
* function largeIntExp
* params:
* largeInt (int)
* Return the value of the exponent associated with the
* large int (largeInt).
* This should be the non-negative integer associated
* with the left-most NUM_EXP_BITS bits in largeInt
*********************************/
int largeIntExp(int largeInt) {
   return getLeft(largeInt, NUM_EXP_BITS);     /* returning the exponent value using getLeft function  / /** <--- replace the 364 with an appropriate expression (no function calls)     ***/
}

/*********************************
* Problem 2.3
* function constructLargeInt
* params:
* mantissa (int)
* exponent (int)
* Return the large int associated with the
* given values of the exponent and mantissa.
* Make the necessary adjustments to standardize the representation.
* If the value is out of bounds, then return the maximum new float
*********************************/
int constructLargeInt(int mantissa, int exponent) {
   /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
   int rangeM = (1 << (NUM_MAN_BITS))-1; /* calculating the range of the mantissa value (0-2^N-1) */
   int rangeE = (1 << (NUM_EXP_BITS))-1; /* calculating the range of the exponent value */
   if (mantissa <= rangeM && exponent > 0) /* checking if the mantissa takes up less than  NUM_MAN_NITS  bits and exponent is positive */
        {
                while (getBit(mantissa, NUM_MAN_BITS-1) != 1 && exponent > 0) /*  adjusting the value of the mantissa and exponent */
                {
                        exponent--;
                        mantissa = mantissa << 1;

                }

        }

   else if (mantissa > rangeM)  /* checking if the range of the mantissa value is larger than 2^n-1 */
        {
                while (mantissa > rangeM) /*  adjusting the value of the mantissa and exponent */
                {
                        exponent++;
                        mantissa = mantissa >> 1;
                }
        }

   if (exponent > rangeE)
        {
                return 0xFFFFFFFF;
        }

   return (exponent << NUM_MAN_BITS) + mantissa;
   /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}

/*********************************
* Problem 2.4
* function largeIntSum
* params:  
* largeInt1 (int)
* largeInt2 (int)
* Return the large int associated with the sum of the two
* large ints given as parameters. If one of the two inputs
* is not a valid large int, or their sum is out of bounds,
* the function returns the maximum large int.
*********************************/
int largeIntSum(int largeInt1, int largeInt2) {
   /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
   int exp1 = largeIntExp(largeInt1);
   int exp2 = largeIntExp(largeInt2);
   int man1 = largeIntMan(largeInt1);
   int man2 = largeIntMan(largeInt2);

   int man = 0;
   int exp = 0;
   if (exp1 >= exp2)
        {
                man = man1 + (man2/(1 << (exp1 - exp2)));
                exp = exp1;
        }
   else {
                man = man2 + (man1/(1 << (exp2 - exp1)));
                exp = exp2;
        }

   int sum = constructLargeInt(man, exp);
   int maxInt = -1;

   if (sum == maxInt)
        {
                return maxInt;
        }

   if (((getBit(man1, NUM_MAN_BITS-1) != 1) && (exp1 > 0)) || ((getBit(man2, NUM_MAN_BITS-1) != 1) && (exp2 > 0)))
        {
                return maxInt;
        }

   return sum;
   /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}

/*********************************
* function largeIntToDouble
* params:  
* largeInt (int)
* Return the value of the input large int as double
*
* !! DO NOT MODIFY THIS FUNCTION DEFINITION (IMPLEMENTATION) !!
* THIS IMPLEMENTATION DEPENDS ON YOUR CORRECT 
-* IMPLEMENTATION OF largeIntMan AND largeIntExp
* (FROM PROBLEMS 2.1 AND 2.2)
* NOTE: FUNCTION DOES NOT CHECK VALIDITY OF THE INPUT
*********************************/
double largeIntToDouble(int largeInt) {

    int mantissa = largeIntMan(largeInt),
            exponent = largeIntExp(largeInt);

    /*** This loop computes 2^exponent efficiently
         by using at most 2 x log_2(exponent) multiplications
         After the i'th iteration of this while loop
         - temp holds 2^(2^i) (2.0 , 4.0 , 16.0 , 256.0 , ... )
         - num holds 2^x, where x is the integer represented
           by the right-most i bits of exponent
                                                        ***/
    double temp = 2.0, num = 1.0;
    while (exponent) {
        if (exponent & 1) num *= temp;
        temp *= temp;
        exponent = exponent >> 1;
    } // end of while(exponent)

    /*** return 2^exp x mantissa  ***/

    return num * mantissa;
}

int main () {
    printf("%d\n",largeIntSum(0x36000000, -3));
    printf("%d", largeIntSum(0x340ABCDEF, -1597));
}