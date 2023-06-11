/**********************************************************
*  flexNumAdder.c                                         *
*  --------------                                         *
*  A main source file for a program for HW #5.            *
*  Program sums a list of numbers it receives as input    *
*  arguments.                                             *
*  Uses functions from the flexNum source module          *
*  and the virtualHeap module (for dynamic mem alloc)     *
**********************************************************/

/**********************************************************
*  DO NOT ADD ANY MORE INCLUDE DIRECTIVES TO THE PROGRAM  *
**********************************************************/
/*** standard libraries iand virtual heap              ***/
#include <stdio.h>
#include <string.h>
#include "/share/ex_data/ex5/virtualHeap.h"

/*********************************************/
/**    FUNCTION DECLARATIONS FOR PROBLEM 1  **/
/*********************************************/
int flexNumDecPos(const char* flexNum);
int flexNumTrail0s(const char* flexNum);
int flexNumLead0s(const char* flexNum);
int flexNumIsInt(const char* flexNum);
double flexNumToDouble(const char* flexNum);
int flexNumStandardize(char* flexNum);
int flexNumSum(const char* flexNum1, const char* flexNum2, char* flexNumSum, int buffLen);



/**********************************************************
*  IF YOU ADD "HELPER FUNCTIONS", DECLARE THEM HERE AND   *
*  DEFINE THEM BELOW MAIN()                               *
**********************************************************/


/**********************************************************
* main function
* - read input arguments one by one as flexible numbers
* - if argument is not valid flexible number, ignore it
* - standardize each input flexible number
* - sum the flexible numbers and standardize the intermediate
*   result
* - print the sum and return the number of invalid input
*   arguments
* - uses dynamic memory allocation to store the intermediate
*   result. Reallocates exact space in each step
**********************************************************/
int main(int argc, char** argv) {
    int i;
    int invalidCount = 0;
    char* sum = ourMalloc(sizeof(char)); /*  creating a dynamic array that will hold the sum of all inputs */
    *sum = '0';  /* intitalizing the sum to be zero */
    for (i = 1; i < argc; i++) {
        if(flexNumDecPos(argv[i]) == -1) { /* checking if the input is a valid flex num and counting the invalids */
            invalidCount++;
            continue; /* we simply countinue to the next iteration if the number is invalid */
        } else {
            flexNumStandardize(argv[i]); /* we standardize the input */
            int len = -(flexNumSum(argv[i], sum, sum, 0)); /* we calculate the length of the input using flexNumSum with buffer=0  */
            char* curSum = ourMalloc(len*sizeof(char)); /* initailizing a dynamic array to hold the current sum of this iteration (sum + curSum) */
            if(!curSum) /* checking if the memory allocation was successful */
            {
                printf("Out of memory\n");
                return 13;
            }
            flexNumSum(argv[i], sum, curSum, len); /* calculating the sum of the current sum togther with the current input value */
            flexNumStandardize(curSum); /* standadrdizing the result */
            ourFree(sum); /* freeing the memeory of sum so it can be allocated again */
            sum = curSum; /* updating to sum to hold the current sum  */
        }
    }
    printf("The sum of the %d input numbers is %s\n", argc-invalidCount-1, sum); /* printing the result  */
    ourFree(sum); /*  freeing the sum (also points to curSum) */
    return invalidCount; /* returning the inValid count */
}
/***   end of main()   ***/

/*******************************************************/
/**                   END  OF  FILE                   **/
/*******************************************************/