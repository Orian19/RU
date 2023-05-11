import java.lang.reflect.Array;

public class Sort <T extends Comparable<T>> {
    private int threshold;

    public Sort() {
        this.threshold = 0;//mergeRecursive = 50000; quickClass = 50;
    }

    /**
     * This is the standard, unmodified version of Quicksort. This version of Quicksort should use the
     * partition routine you saw in the lecture.
     * @param array to sort
     */
    public void quickSortClass(T[] array) {
        quickSortClass(array, 0, array.length);
    }

    /**
     * helper function to run quickSortClass
     * @param array to sort
     * @param start pointer
     * @param end pointer
     */
    private void quickSortClass(T[] array, int start, int end) {
        if (end - start > this.threshold) {
            int parti = partitionClass(array, start, end-1);
            quickSortClass(array, start, parti-1);
            quickSortClass(array, parti+1, end);
        } else {
            bubbleSort(array, start, end-1);
        }
    }

    /**
     * helper function to do partition as seen in class
     * @param array to sort
     * @param start pointer
     * @param end pointer
     * @return partition index
     */
    private int partitionClass(T[] array, int start, int end) {
        T pivot = array[end]; // last element
        int lessIdx = end, moreIdx = start - 1;

        while (true) {
            do {
                lessIdx--;
            } while (array[lessIdx].compareTo(pivot) > 0 && lessIdx > start);

            do {
                moreIdx++;
            } while (array[moreIdx].compareTo(pivot) > 0 || moreIdx > end);

            if (moreIdx < lessIdx) {
                swapValues(array, moreIdx, lessIdx);
            } else {
                swapValues(array, lessIdx+1, moreIdx);
                return lessIdx+1;
            }
        }
    }

    /**
     * This is the standard, unmodified version of Quicksort. This version of Quicksort should use the
     * partition routine you saw in the recitation.
     * @param array to sort
     */
    public void quickSortRecitation(T[] array) {
        quickSortRecitation(array, 0, array.length);
    }

    /**
     * helper function to run quickSortRecitation
     * @param array to sorct
     * @param start pointer
     * @param end pointer
     */
    private void quickSortRecitation(T[] array, int start, int end) {
        if (end - start > this.threshold) {
            int parti = partitionRecitation(array, start, end-1);
            quickSortClass(array, start, parti-1);
            quickSortClass(array, parti+1, end);
        } else {
            bubbleSort(array, start, end-1);
        }
    }

    /**
     * helper function to do partition as seen in the recitation
     * @param array to sort
     * @param start pointer
     * @param end pointer
     * @return partition index
     */
    private int partitionRecitation(T[] array, int start, int end) {
        T pivot = array[end]; // last element
        int lessIdx = start-1;

        for (int moreIdx = start; moreIdx < end-1; moreIdx++) {
            if (array[moreIdx].compareTo(pivot) <= 0) {
                lessIdx++;
                swapValues(array, lessIdx, moreIdx);
            }
        }
        swapValues(array, lessIdx+1, end-1);
        return lessIdx + 1;
    }

    /**
     * This function sorts an array using recursive stable merge sort.
     * @param array to sort
     */
    public void mergeSortRecursive(T[] array) {
        mergeSortRecursive(array, 0, array.length-1);
    }

    /**
     * helper function to run mergeSort recursively
     * @param array to sort
     * @param start pointer for start point of sub-array
     * @param end pointer for end point of sub-array
     */
    private void mergeSortRecursive(T[] array, int start, int end) {
        if (end - start > this.threshold) {
            int mid = (start + end) / 2;
            mergeSortRecursive(array, start, mid);
            mergeSortRecursive(array, mid+1, end);
            merge(array, start, mid, end);
        } else {
            bubbleSort(array, start, end);
        }
    }

    /**
     * helper function that implements the merge process
     * @param array to sort
     * @param start pointer
     * @param mid pointer
     * @param end pointer
     */
//    private void merge(T[] array, int start, int mid, int end) { // TODO: something wrong. I get duplicate values
//        int n = end - start + 1;
//        T[] tempArr = (T[]) Array.newInstance(Comparable.class, n);
//        int i = 0;
//        int i1 = start, i2 = mid + 1;
//
//        while (i < n) {
//            if(array[i1].compareTo(array[i2]) < 0) { // checking if A[i1] < A[i2]
//                tempArr[i++] = array[i1++];
//            } else {
//                tempArr[i++] = array[i2++];
//            }
//        }
//
//        // copying the merged array into the input array
//        for (int j = 0; j < n-1; j++) {
//            array[start+j] = tempArr[j]; // TODO: maybe issue is this is too slow and not memory efficient?
//        }
////        System.arraycopy(array, start, tempArr, start, n-start); // copying elements to the new array
//    }

    private void merge(T[] array, int start, int mid, int end) { // TODO: something wrong. I get duplicate values
        int n1 = mid - start + 1, n2 = end - mid;
        T[] left = (T[]) Array.newInstance(Comparable.class, n1);
        T[] right = (T[]) Array.newInstance(Comparable.class, n2);

        for (int i = 0; i < n1; i++) {
            left[i] = array[start+i];
        }

        // TODO: copy helper function
        for (int i = 0; i < n2; i++) {
            right[i] = array[mid+1+i];
        }

        int i = 0, j = 0, k = start;
        while (i < left.length && j < right.length) {
            if (left[i].compareTo(right[j]) <= 0) { // checking if L[i] < R[J]
                array[k] = left[i];
                i++;
            } else {
                array[k] = right[j];
                j++;
            }
            k++;
        }

        // copying left overs to final array
        while (i < left.length) {
            array[k] = left[i];
            k++;
            i++;
        }
        while (j < right.length) {
            array[k] = right[j];
            k++;
            j++;
        }
    }

    /**
     * helper function that implemnt bubble sort.
     * to be used on small inputs to make the sorting more efficient
     * @param array to sort
     * @param start pointer
     * @param end pointer
     */
    private void bubbleSort(T[] array, int start, int end) {
        for (int i = 0; i < end; i++) {
            int swap = 0; // binary indicator for detecting early stops
            for (int j = 1; j < end-i+1; j++) {
                if (array[j-1].compareTo(array[j]) > 0) { // if A[j-1] bigger than A[j]
                    swap = 1;
                    swapValues(array, j-1, j);
                }
            }
            if (swap == 0) { // no swaps were made --> array is already sorted and can stop
                return;
            }
        }
    }

    /**
     * helper function to swap elements in array
     * @param array to swap in
     * @param i index
     * @param j index
     */
    private void swapValues(T[] array, int i, int j) {
        T temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }

    /**
     * This function sorts an array using non-recursive stable merge sort.
     * @param array to sort
     */
    public void mergeSortIterative(T[] array) { //TODO: does not work? separate merge?
        int n = array.length;
//        T[] tempArr = (T[]) Array.newInstance(Comparable.class, n);

        for (int subSsize = 1; subSsize < n; subSsize *= 2) { // represents the size of the sub-arrays
            for (int start = 0; start < n - subSsize; start += 2 * subSsize) { // indices of the arrays
                if (subSsize > this.threshold) {
                    int mid = start + subSsize - 1;
                    int end = Math.min(start + (2 * subSsize - 1), n - 1);
                    merge(array, start, mid, end);
                } else {
                    bubbleSort(array, start, n - 1);
                }
            }
        }
    }

    /**
     * This is the standard, unmodified version of the radix sort which gets as an input the base to be
     * used by the algorithm. uses count sort
     * @param array to sort
     * @param base to be used
     */
    //TODO: fixxxxx, does not work + I dont fully understand some parts
    public static void radixSort(int[] array, int base) {
        int maxNumDigits = (int) Math.log(base); //TODO: probably issue here! // calculating the max num of digits for a specific base
        // TODO: do we also need to deal with letters? for ex. hexadecimal
        convertBase(array, 10, base);
        int sizeDigits = getMaxSize(array, base);

        for (int i = 0; i < sizeDigits; i += maxNumDigits) {
            int[] sortedBySubNum =  new int[array.length]; // starting from LSB to MSB
            countSort(array, sortedBySubNum, sizeDigits, i, base);
            System.arraycopy(sortedBySubNum, 0, array, 0, array.length);
        }
        convertBase(array, base, 10);
    }

    /**
     * finding the bound for radix sort
     * @param array given
     * @param base given
     * @return max size of digits
     */
    private static int getMaxSize(int[] array, int base) {
        // TODO: not sure how this works or if needed
        int maxElement = array[0];
        for (int i = 1; i < array.length; i++) {
            if (array[i] > maxElement) {
                maxElement = array[i];
            }
        }
        return  (int) Math.ceil(Math.log(maxElement + 1) / Math.log(base));
    }

    /**
     * helper function that implements countSort for a specific base
     * @param array to sort
     * @param digitIdx amount of digits representing one number in the given base
     * @param base working with (all elements in the array are from this base)
     * @param sorted result of the sorted array will be here
     * @param sizeBound the maximum number of digits in the given array (our k)
     */
    private static void countSort(int [] array, int [] sorted, int sizeBound, int digitIdx, int base) {
        // TODO: need to somehow get the max num of digits
        // TODO: need somehow integrate digitIdx and base!!!
        // initializing the prefix sum array (C in the lecture)
        int[] prefixSum = new int[base]; //TODO: not sure about the size (sizeBoound?)

        // building C to hold the number of appearance of each element s.t it corresponds to index of C
        for (int i = 0; i < array.length; i++) { // TODO: should start from 0/1?
            int curDigit= getCurDigit(array[i], digitIdx, base);
            prefixSum[curDigit]++;
        }

        // now C[i] contains the number of elements equal to i
        // we now create C prime (modifying C to have prefix sum)
        for (int i = 1; i < base; i++) {
            prefixSum[i] += prefixSum[i-1];
        }

        // now C prime = prefix sum contains the number of elements less than or equal to i
        // we now fill the sorted(B) with the correct values by prefixSum to get the sorted array
        for (int i = array.length-1; i >= 0; i--) { //TODO: make sure for is good
            int curDigit = getCurDigit(array[i], digitIdx, base);
            sorted[prefixSum[curDigit] - 1] = array[i]; //TODO: should there be -1 here?
            prefixSum[curDigit]--;
        }
    }

    private static int getCurDigit(int curValue, int digIdx, int base) {
        // TODO: need to convert. is this what I need?
        return (curValue / (int)Math.pow(base, digIdx)) % base;
    }

    /**
     * converting a number from one base to another
     * @param array given
     * @param fromBase from which base to convert
     * @param toBase to which base to convert
     */
    private static void convertBase(int[] array, int fromBase, int toBase) {
        for (int i = 0; i < array.length; i++) {
            array[i] = convertNumber(array[i], fromBase, toBase);
        }
    }

    private static int convertNumber(int number, int fromBase, int toBase) {
        return convertFromDecimal(convertToDecimal(number, fromBase), toBase);
    }

    private static int convertToDecimal(int number, int base) {
        int decimal = 0;
        int power = 0;
        while (number > 0) {
            decimal += (number % 10) * Math.pow(base, power++);
            number /= 10;
        }
        return decimal;
    }

    private static int convertFromDecimal(int decimal, int base) {
        int number = 0;
        int power = 0;
        while (decimal > 0) {
            number += (decimal % base) * (int) Math.pow(10, power++);
            decimal /= base;
        }
        return number;
    }

    /**
     * This function updates the value of a private data member that controls the size of inputs below
     * which a naïve sorting algorithm will be applied (i.e., the base case for all the sorting algorithms
     * supported by the class). This method allows the user to set the threshold size during runtime,
     * offering better flexibility in controlling the sorting behavior.
     * @param threshold for base case sorting
     */
    public void setNaiveSortThreshold(int threshold) {
        this.threshold = threshold;
    }
}
