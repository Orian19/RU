#include <string.h>
/**********************************************************
*  flexNum.c                                              *
*  --------------                                         *
*  A code module containing functions for implementing    *
*  the flexible number data type                          *
**********************************************************/

/*********************************************************/
/*     IMPORTANT NOTE:                                   */
/*     !! MODIFY CODE ONLY IN THE MARKED PLACES !!       */
/*********************************************************/

/********************************/
/**    FUNCTION DECLARATIONS   **/
/********************************/
int flexNumDecPos(const char* flexNum);
int flexNumTrail0s(const char* flexNum);
int flexNumLead0s(const char* flexNum);
int flexNumIsInt(const char* flexNum);
double flexNumToDouble(const char* flexNum);
int flexNumStandardize(char* flexNum);
int flexNumSum(const char* flexNum1, const char* flexNum2, char* flexNumSum, int buffLen);


/********************************/
/**    FUNCTION DEFINITIONS    **/
/********************************/


/**********************************************************
* Problem 1.1
* function flexNumDecPos
* params:
* flexNum  (const char*) - string representing flexible number
* return the position of a decimal point withing a given
* flexible number.
* - If input is not a valid flexible number, return -1
* - If the input flexible number does not contain a decimal
*   point, return its length
**********************************************************/
int flexNumDecPos(const char* flexNum) {
    /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
    int i = 0, countP = 0, pos = -1; /*  declaring count for decimal points and for the position*/

    if((flexNum[0] < 48) || (flexNum[0] > 57)) return -1; /* checking if the first char is not a number */

    while(flexNum[i]) { /* looping over flexNum  */
        if(countP > 1) return -1;  /* we return -1 if we encounter more than one decimal point */
        if(flexNum[i] == 46){ /* updating decimal point count and postion */
            countP++;
            pos=i;
        }
        else if ((flexNum[i] < 48) || (flexNum[i] > 57)) return -1; /* making sure we do not encounter any chars that are not numbers */
        i++;
    }
    if (countP == 0) return strlen(flexNum); /* returning the length of the string if we did not encounter any decimal points */

    return pos;
    /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}
/***   end of flexNumDecPos()   ***/


/**********************************************************
* Problem 1.2
* function flexNumTrail0s
* params:
* flexNum  (const char*) - string representing flexible number
* return the number of trailing zeros in a given flexible
* number.
* - If input is not a valid flexible number, return -1
**********************************************************/
int flexNumTrail0s(const char* flexNum) {
    /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
    int countTrailing = 0; /* intalizing counter */
    int i = strlen(flexNum) - 1; /* intailizing i to start from the end of the string  */
    int isValid = flexNumDecPos(flexNum);
    if (isValid == -1) return -1; /* checking if the string is a flex number */
    if (isValid == strlen(flexNum)) return 0; /* checking if there is no decimal point */
    while (flexNum[i]) {
        if (i <= isValid) return countTrailing;  /* checking if we reached the decimal point  */
        if (flexNum[i] == 48) countTrailing++; /* updating the count if we encounter a zero */
        else break; /*  if we encountred a number that is not zero we break */
        i--;
    }
    return countTrailing;
    /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}
/***   end of flexNumTrail0s()   ***/


/**********************************************************
* Problem 1.3
* function flexNumLead0s
* params:
* flexNum  (const char*) - string representing flexible number
* return the number of leading zeros in a given flexible
* number.
* - If input is not a valid flexible number, return -1
**********************************************************/
int flexNumLead0s(const char* flexNum) {
    /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
    int countLeading = 0, i = 0; /* intalizing counter and i to be the index  */
    int isValid = flexNumDecPos(flexNum);
    if (isValid == -1) return -1; /* checking if the string is a flex number */
    while (flexNum[i]) {
        if (i >= isValid) return countLeading; /* checking if we reached the decimal point  */
        if((i + 1 == isValid) && (flexNum[i] == 48)) return countLeading; /* if zero is at the last index (before a decimal point/the number) dont count it  */
        if (flexNum[i] == 48) countLeading++; /* updating the count if we encounter a zero */
        else break; /*  if we encountred a number that is not zero we break */
        i++;
    }
    return countLeading;
    /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}
/***   end of flexNumLead0s   ***/


/**********************************************************
* Problem 1.4
* function flexNumIsInt
* params:
* flexNum  (const char*) - string representing flexible number
* return 1 if a given flexible number represents an integer
* value and 0 otherwise.
**********************************************************/
int flexNumIsInt(const char* flexNum) {
    /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
    if( (flexNumDecPos(flexNum) == strlen(flexNum)) || (flexNumTrail0s(flexNum) == (strlen(flexNum) - flexNumDecPos(flexNum) -1)) ) return 1; /* checkimg if the dot position is at length, or the number of the trailing zeros is equal to the number of the digits after the dot. if so, we know we got an integer */
    return 0;
    /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}
/***   end of flexNumIsInt()   ***/


/**********************************************************
* Problem 1.5
* function flexNumToDouble
* params:
* flexNum  (const char*) - string representing flexible number
* return the double-percision value corresponding to a
* given flexible number.
* - If input is not a valid flexible number, return -1.0
**********************************************************/
double flexNumToDouble(const char* flexNum) {
    /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
    double numPos = 0.0, numNeg = 0.0; /* declaring vars to sum up, numPos - to sum the numbers before the dot, numNeg - after the dot */
    int isValid = flexNumDecPos(flexNum);
    int i = 0;
    if (isValid == -1) return -1.0;      /* cheking if the number is valid */
    while (i < isValid) {
        numPos = numPos * 10 + (flexNum[i] - '0');  /* sum up the digits before the dot according to their value to the power of 10 */
        i++;
    }

    int j = strlen(flexNum) - 1;
    while (j > isValid) {
        numNeg = numNeg / 10 + (flexNum[j] - '0'); /* sum up the digits after the dot according to their value to the minus power of 10 */
        j--;
    }
    return numPos + numNeg/10; /* the sum of the digits before the dot is the final number */
    /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}
/***   end of flexNumToDouble()    ***/


/**********************************************************
* Problem 1.6
* function flexNumStandardize
* params:
* flexNum  (const char*) - string representing flexible number
* converts given flexible number to standard form by removing
* all leading and trailing zeros.
* writes over the given flexible number.
* return 0 if not a valid flexible number
*  (and does not modify string)
* return 1 if already standard flexible number
*  (and does not modify string)
* return 2 if a non-standard flexible number*  (and modifies string appropriately)
**********************************************************/
int flexNumStandardize(char* flexNum) {
    /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
    int isValid = flexNumDecPos(flexNum);
    int lead = flexNumLead0s(flexNum), trail = flexNumTrail0s(flexNum); /* declaring vars, lead - number of leading zeros, trail - number of trailing zeros */
    if (isValid == -1) return 0; /* cheking if the number is valid */
    if ((lead == 0)  && (trail == 0)) return 1; /* if the number of trailing and leading zeros is 0 then we alredy have a standard number */
    int i = 0;
    while(lead < strlen(flexNum) - trail) { /* going through the number from the pos after the leading zeros */
        if (flexNum[lead] == '.' && lead == strlen(flexNum) - trail - 1) break; /* if we got to the position of the dot and if there rest of the numbers (after the dot) are trailing zeros then break (without adding the dot) */
        flexNum[i] = flexNum[lead]; /* changing our number at location i (starts at 0) to the number at location lead (after the leading zeros)*/
        lead++;
        i++;
    }
    flexNum[i] = '\0'; /* bound the array at position i, as we increase i before we finish the while loop */
    return 2;
    /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}
/***   end of flexNumStandardize()   ***/


/**********************************************************
* Problem 1.7
* function flexNumSum
* params:
* flexNum1    (const char*) - string representing flexible number
* flexNum2    (const char*) - string representing flexible number
* flexNumSum  (char*)       - buffer (character array) used for writing the result
* buffLen     (int)         - maximum number of character
*                            (including '\0') that function
*                            is allowed to write to buffer
* writes the flexible number corresponding to the sum of two
* given flexible numbers into a specified buffer.
* If one of the two flexible numbers is invalid, or space is
* insufficient to write the sum, then does not write anything
* and returns minus the length of required to write the sum.
* Otherwise, writes sum and returns 1.
* Does not standardize the output.
**********************************************************/
int flexNumSum(const char* flexNum1, const char* flexNum2, char* flexNumSum, int buffLen) {
    /***      Apply all changes to the code below this line. DO NOT DELETE THIS COMMENT   ***/
    int isValid1 = flexNumDecPos(flexNum1), isValid2 = flexNumDecPos(flexNum2);
    int len1 = strlen(flexNum1), len2 = strlen(flexNum2);
    int c=0,flagR = 1,flagL = 1, flagD = -1, flagF = -1; /* c is the carry-over (values=0/1), flagL,R help detemine if num1 or num2 has more digits, flagD ot detrimine if there is a decimal point for either number, flagF to determine if we need to overflow digit */
    if ((isValid1 == -1) || (isValid2 == -1)) return 0; /* checking if the nums are valid flex mums */

    int r1 = len1-isValid1, r2 = len2-isValid2; /* r1,r2 is the number of digigts to the right of the decimal point*/
    if (r1 != 0) r1--; /* removing 1 as in these two cases we have a decimal point which is included as part of the length */
    if (r2 != 0) r2--;
    int length  = r1; /* intializing expected length of the final sum */
    if (r2>r1){ /* checking which number has more digits to the right of the decimal point - to help calculate the expected length */
        length  = r2;
        flagR = 2; /* in this case flexNum2 has more digits to the right of the dec point */
    }
    if (length != 0) length++; /* adding 1 for the case we have a decimal point  */

    if (isValid1 > isValid2) { /* case 1: where num1 has more digits to the left of the decimal point */
        length += isValid1;
        if (flexNum1[0] == '9'){ /* checking if the msb is 9 to add overflow digit (0) */
            length++;
            flagF = 0; /* updating flagF as we have an overflow digit  */
        }

    } else if((isValid1 == isValid2) && ((flexNum1[0]-'0')+(flexNum2[0]-'0') >= 9)) { /* case 2: where the number of digits to the left of the dec point of num1 is the same num2, and the sum of the two msb's is greater than 9 */
        length += isValid1 + 1; /* we take the number of digits before the decimal point and adding 1 as we have an overflow diigt */
        flagF = 0; /* updating flag accordingly */

    }else{ /* symmetric to case 1 (isValid2 >= isValid1) */
        length += isValid2;
        flagL=2;
        if (flexNum2[0] == '9'){
            length++;
            flagF = 0;
        }
    }
    length++; /* add 1 for '\0' null terminator */
    if(length > buffLen) return -length; /* chekcing if the expected length of the sum is greater than buffLen */

    int j= length - 2; /* initailizing index to be the last poisiton of the string (-2 as we have also '\0' */
    while (j >= 0) {
        if((r1 == r2) && (r1!= 0) && (r2 != 0))
            flagD = 0; /* updating flagD to be 0 as we have a decimal point */
        if ((flagF == 0) && (j == 0)){ /* checking if we have an overflow digit and j is at the position of the msb */
            flexNumSum[j] = c + '0';
        }else if(flagR == 1 && r1 > r2){ /* checking if we have right overhang caused by num1 */
            flexNumSum[j] = flexNum1[isValid1+r1]; /* adding the current digit to the final sum ([isValid1 + r1] = position of the overhanged digit */
            r1--; /* advancing(--) r1 to the left */
            flagD = 0; /* in this case we have a decimal point */
        }else if (flagR == 2 && r2>r1){ /* symmetric to the case above */
            flexNumSum[j] = flexNum2[isValid2+r2];
            r2--;
            flagD = 0;
        }else if ((flagD == 0) && (r1 != 0)) { /* in this case we are left with overlaping digits to the right of the decimal point (r1/r2 == 0 only when we reach the decimal point) */
            flexNumSum[j] = (((flexNum1[isValid1+r1] - '0') + (flexNum2[isValid2+r2] - '0') + c) % 10) + '0'; /* calculating the sum of the digits (from num1,num2) and adding c as the carry over (can be 0). also we do mod 10 to thake the remainder (we do + '0' to convert back to a char)  */
            if (((flexNum1[isValid1+r1] - '0') + (flexNum2[isValid2+r2] - '0') + c) >= 10) /* updating the carry over value (c) according to the sum of current digits */
                c=1;
            else
                c=0;
            /* advancing r1, r2 (--, advancing to the left) */
            r1--;
            r2--;
        }else if (flagD == 0){ /* adding the decimal point at the correct position (r1 = r2 = 0) */
            flexNumSum[j] = '.';
            flagD = -1; /* updating flagD to be -1 as it is not revelant for the rest of calculation  */
        }else if (isValid2 > 0 && isValid1 > 0){ /* now, we are calculating the sum of the digits to the left of the decimal point */
            flexNumSum[j] = (((flexNum1[isValid1 - 1] - '0') + (flexNum2[isValid2 - 1] - '0') + c) % 10) + '0';
            if ((flexNum1[isValid1-1] - '0')+(flexNum2[isValid2-1] - '0')+c >= 10)
                c=1;
            else
                c=0;
            isValid1--;
            isValid2--;
        }else if ((flagL == 1) && (isValid1 > 0)) { /* checking if we have left overhang caused by num1 */
            flexNumSum[j] = (((flexNum1[isValid1-1] - '0') + c) % 10) + '0';
            if ((flexNum1[isValid1-1] - '0') + c >= 10)
                c = 1;
            else
                c = 0;
            isValid1--;
        }else if ((flagL == 2) && (isValid2 > 0)) { /* symmetric to the case above */
            flexNumSum[j] = (((flexNum2[isValid2 - 1] - '0') + c) % 10) + '0';
            if((flexNum2[isValid2-1] - '0') + c >= 10)
                c = 1;
            else
                c = 0;
            isValid2--;
        }
        j--;
    }
    flexNumSum[length-1] = '\0'; /* adding the null indicator */
    return 1;
    /***      Apply all changes to the code above this line. DO NOT DELETE THIS COMMENT   ***/
}
/***   end of flexNumSum()   ***/

/*******************************************************/
/**                   END  OF  FILE                   **/
/*******************************************************/