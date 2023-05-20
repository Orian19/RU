import java.lang.reflect.Array;

public class Sort <T extends Comparable<T>> {
    private int threshold;

    public Sort() {
        this.threshold = 50;
    }

    /**
     * This is the standard, unmodified version of Quicksort. This version of Quicksort should use the
     * partition routine you saw in the lecture.
     * @param array to sort
     */
    public void quickSortClass(T[] array) {
        quickSortClass(array, 0, array.length-1);
    }

    /**
     * helper function to run quickSortClass
     * @param array to sort
     * @param start pointer
     * @param end pointer
     */
    private void quickSortClass(T[] array, int start, int end) {
        if (end - start > this.threshold) {
            int parti = partitionClass(array, start, end);
            quickSortClass(array, start, parti-1);
            quickSortClass(array, parti+1, end);
        } else {
            simpleSort(array, start, end);
//            bubbleSort(array, start, end);
        }
    }

    /** TODO: which sort is this? why my bubble does not work and this does?
     * gal's simple sort
     * @param array to sort
     * @param i start index
     * @param j end index
     */
    public void simpleSort(T[] array, int i,int j){
        int length = j - i + 1;
        if (length > 1) {
            for (int k = i; k < j; k++) {
                for (int l = i; l < j; l++) {
                    if (array[l].compareTo(array[l+1]) > 0) {
                        swapValues(array,l,l+1);
                    }
                }
            }
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
        int moreIdx = start - 1, lessIdx = end;

        while (moreIdx < lessIdx) {
            lessIdx--; moreIdx++;
            while (lessIdx >= start && array[lessIdx].compareTo(pivot) > 0) {
                lessIdx--;
            }

            while (moreIdx <= end && array[moreIdx].compareTo(pivot) <= 0) {
                moreIdx++;
            }

            if (moreIdx < lessIdx) {
                swapValues(array, moreIdx, lessIdx);
            }
        }
        swapValues(array, lessIdx+1, end);
        return lessIdx+1;
    }

    /**
     * This is the standard, unmodified version of Quicksort. This version of Quicksort should use the
     * partition routine you saw in the recitation.
     * @param array to sort
     */
    public void quickSortRecitation(T[] array) {
        quickSortRecitation(array, 0, array.length-1);
    }

    /**
     * helper function to run quickSortRecitation
     * @param array to sorct
     * @param start pointer
     * @param end pointer
     */
    private void quickSortRecitation(T[] array, int start, int end) {
        if (end - start > this.threshold) {
            int parti = partitionRecitation(array, start, end);
            quickSortRecitation(array, start, parti-1);
            quickSortRecitation(array, parti+1, end);
        } else {
              simpleSort(array, start, end);
//            bubbleSort(array, start, end-1);
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

        for (int moreIdx = start; moreIdx <= end-1; moreIdx++) {
            if (array[moreIdx].compareTo(pivot) <= 0) {
                lessIdx++;
                swapValues(array, lessIdx, moreIdx);
            }
        }
        swapValues(array, lessIdx+1, end);
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

    private void merge(T[] array, int start, int mid, int end) { // TODO: something wrong. I get duplicate values
        int n1 = mid - start + 1, n2 = end - mid;
        T[] left = (T[]) Array.newInstance(Comparable.class, n1);
        T[] right = (T[]) Array.newInstance(Comparable.class, n2);

        copyArray(array, left, start, mid);
        copyArray(array, right, mid+1, end);

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

        // copying remaining elements to the final array
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
        for (int i = start; i < end; i++) {
            boolean swapped = false; // binary indicator for detecting early stops
            for (int j = start; j < end - i; j++) {
                if (array[j].compareTo(array[j+1]) > 0) { // if A[j-1] bigger than A[j]
                    swapped = true;
                    swapValues(array, j, j+1);
                }
            }
            if (!swapped) { // no swaps were made --> array is already sorted and can stop
                break;
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

        for (int subSsize = 1; subSsize < n; subSsize *= 2) { // represents the size of the sub-arrays
            for (int start = 0; start < n - subSsize; start += 2 * subSsize) { // indices of the array
                int rightSubEnd = Math.min(2 * subSsize + start - 1, n - 1);
                if (subSsize > this.threshold) {
                    int mid = start + subSsize - 1;
                    merge(array, start, mid, rightSubEnd);
                } else {
                    bubbleSort(array, start, rightSubEnd);
                }
            }
        }
    }

    /**
     * helper function to copy elements from one array to another
     * @param from array to copy from
     * @param to array to copy to
     * @param start index
     * @param end index
     */
    private void copyArray(T[] from, T[] to, int start, int end) {
        int cur = 0;
        for (int i = start; i <= end; i++) {
            to[cur] = from[i];
            cur++;
        }
    }

    /**
     * This is the standard, unmodified version of the radix sort which gets as an input the base to be
     * used by the algorithm. uses count sort
     * @param array to sort
     * @param base to be used
     */
    public static void radixSort(int[] array, int base) {
        int sizeDigits = getMaxSize(array);

        // running over the digits in the base given
        for (int i = 1; sizeDigits / i > 0; i *= base) { //TODO: understand 'for loop'
            int[] sortedBySubNum =  new int[array.length]; // starting from LSB to MSB
            countSort(array, sortedBySubNum, i, base);
            System.arraycopy(sortedBySubNum, 0, array, 0, array.length);
        }
    }

    /**
     * finding the bound for radix sort
     * @param array given
     * @return max size of digits
     */
    private static int getMaxSize(int[] array) {
        int maxElement = Integer.MIN_VALUE;
        for (int i = 0; i < array.length; i++) {
            if (array[i] > maxElement) {
                maxElement = array[i];
            }
        }
        return  maxElement;
    }

    /**
     * helper function that implements countSort for a specific base
     * @param array to sort
     * @param digitIdx amount of digits representing one number in the given base
     * @param base working with (all elements in the array are from this base)
     * @param sorted result of the sorted array will be here
     */
    private static void countSort(int [] array, int [] sorted, int digitIdx, int base) {
        // initializing the prefix sum array (C in the lecture)
        int[] prefixSum = new int[base];

        // building C to hold the number of appearance of each element s.t it corresponds to index of C
        for (int i = 0; i < array.length; i++) {
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
        for (int i = array.length-1; i >= 0; i--) {
            int curDigit = getCurDigit(array[i], digitIdx, base);
            sorted[prefixSum[curDigit]-1] = array[i];
            prefixSum[curDigit]--;
        }
    }

    private static int getCurDigit(int num, int digIdx, int base) {
        return (num / digIdx) % base;  //TODO:understand
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
