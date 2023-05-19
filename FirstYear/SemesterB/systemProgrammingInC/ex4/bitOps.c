#include <stdio.h>

/*************************************************
*  bitOps.c
*  ----------
*  A program containing functions for implementing
* basic bit operations on integers
*************************************************/

/************************************************/
/* IMPORTANT NOTE:                              */
/* !! MODIFY CODE ONLY IN THE MARKED PLACES !!  */
/************************************************/


/********************************/
/**    FUNCTION DECLARATIONS   **/
/********************************/

/*********************************
* Problem 1.1
* computes the value (0/1) of
* the k'th bit of a given int n
*********************************/
int getBit(int n, int k);

/*********************************
* Problem 1.2
* computes the int represented by
* k right bits of a given int n
*********************************/
int getRight(int n, int k);

/*********************************
* Problem 1.3
* computes the int represented by
* k left bits of a given int n
*********************************/
int getLeft(int n, int k);


/********************************/
/**    FUNCTION DEFINITIONS    **/
/********************************/

/*********************************
* Problem 1.1
* function getBit
* params:
* n (int)
* k (int)
* return the value (0/1) of the
*  k'th bit of n
*********************************/
int getBit(int n, int k) {

   /*** check for valid value of k ***/
   if(k>=(sizeof(int)8) || k<0)            /** <--- replace the 1 with an appropriate condition on k    ***/
      return 0;

   return (((unsigned)n)& (1<<k)) >> k;      /*** <--- replace the 749 with an appropriate expression      ***/
}

/*********************************
* Problem 1.2
* function getRight
* params:
* n (int)
* k (int)
* return the int represented by the
*  k right bits of n
*********************************/
   int getRight(int n, int k) {

   /*** base of recusrion ***/
   if(k<=0)             /*** <--- replace the 1 with an appropriate condition on k    ***/
      return 0;

   if(k> ((sizeof(int)8)-1))             /** <--- replace the 1 with an appropriate condition on k    ***/
      return n;

   return ((1 << (k-1)) * (getBit(n,k-1))) + getRight(n,k-1);       /*** <--- replace the 749 with an appropriate expression involving a recursive call to rightBits     ***/

}

/*********************************
* Problem 1.3
* function getLeft
* params:
* n (int)
* k (int)
* return the int represented by the
*  k left bits of n
*********************************/
int getLeft(int n, int k) {

   if(k<=0 )             /*** <--- replace the 1 with an appropriate condition on k    ***/
      return 0;

   if(k> ((sizeof(int)8)-1))             /** <--- replace the 1 with an appropriate condition on k    ***/
      return n;

  
   return (((unsigned)n) >> ((sizeof(int)8)-k));       /** <--- replace the 749 with an appropriate expression (no function calls)     ***/
}