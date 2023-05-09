import java.lang.reflect.Array;

public class Sort <T extends Comparable<T>> {
    private int threshold;
    private T[] array; // TODO: figure out if needed other fields

    public Sort() {
        this.threshold = 50;//mergeRecursive = 50000; quickClass = 50;
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
     * This is the standard, unmodified version of the radix sort which gets as an input the base to be
     * used by the algorithm.
     * @param array to sort
     * @param base to be used
     */
    public static void radixSort(int[] array, int base){

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
        int n = end - start + 1;
        T[] tempArr = (T[]) Array.newInstance(Comparable.class, n);
        int i = 0;
        int i1 = start, i2 = mid + 1;

        while (i < n) {
            if(array[i1].compareTo(array[i2]) < 0) { // checking if A[i1] < A[i2]
                tempArr[i++] = array[i1++];
            } else {
                tempArr[i++] = array[i2++];
            }
        }

        // copying the merged array into the input array
        for (int j = 0; j < n-1; j++) {
            array[start+j] = tempArr[j]; // TODO: maybe issue is this is too slow and not memory efficient?
        }
//        System.arraycopy(array, start, tempArr, start, n-start); // copying elements to the new array
    }

//    private void merge(T[] array, int start, int mid, int end) { // TODO: something wrong. I get duplicate values
//        int n1 = mid - start + 1, n2 = end - mid;
//        T[] left = (T[]) Array.newInstance(Comparable.class, n1);
//        T[] right = (T[]) Array.newInstance(Comparable.class, n2);
//
//        for (int i = 0; i < n1; i++) {
//            left[i] = array[start+i];
//        }
//
//        for (int i = 0; i < n2; i++) {
//            right[i] = array[mid+1+i];
//        }
//
//        int i = 0, j = 0;
//        for (int k = start; k < end; k++) {
//            if (left[i].compareTo(right[j]) < 0) { // checking if L[i] < R[J]
//                array[k] = left[i];
//                i++;
//            } else {
//                array[k] = right[j];
//                j++;
//            }
//        }
//    }

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
