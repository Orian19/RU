import java.util.Arrays;
import java.util.Random;

import java.awt.*;
import javax.swing.*;
import java.awt.geom.*;
public class Main <T extends Comparable<T>> {
    static int NUMITER = 100;
    static String[] sortingAlgorithms = {"MergeSortClass", "MergeSortIterative",
            "quickSortClass", "quickSortRecitation", "radixSort", "JavaArraySort"};
    public static void main(String[] args) {
        System.out.println("Starting tests...\n");

        // calling all the tests

        // radix sort tests
        int[] bases = {1, 5, 10, 15}; // unable to do , 20, 25, 30};
        runRadixTests(bases);

        // sorting algorithms comparison tests (using base 2^15 for radix which was best)
        int[] inputSizes = {10000, 50000, 100000, 500000, 1000000};
        runComparisonTests(inputSizes);

        System.out.println("\n\n\nAll Done :)\n");
    }

    /**
     * running radix sort with all bases given
     * @param bases for sort
     */
    public static void runRadixTests(int[] bases) {
        for (int i = 0; i < bases.length; i++) {
            System.out.println("\nradixSort base: 2^" + bases[i] + "\n");
            radixSortTest((int)Math.pow(2,bases[i]));
        }
    }

    /**
     * run all comparison tests (randomInput, increasingInput, DecreasingInput) on all input sizes given
     * @param inputSizes to run on
     */
    public static void runComparisonTests(int[] inputSizes) {
        double[][] rndAvgs = new double[inputSizes.length][sortingAlgorithms.length];
        double[][] incAvgs = new double[inputSizes.length][sortingAlgorithms.length];
        double[][] decAvgs = new double[inputSizes.length][sortingAlgorithms.length];

        for (int i = 0; i < inputSizes.length; i++) {
            System.out.println("Sorting comparison tests for input: " + inputSizes[i] + "\n");
            rndAvgs[i] = randomInputsTester(inputSizes[i]);
            incAvgs[i] = increasingInputsTester(inputSizes[i]);
            decAvgs[i] = decreasingInputsTester(inputSizes[i]);
        }

//        // showing comparison plots
//        ResultsGraph.showGraph(rndAvgs); // Show graph for random input averages
//        ResultsGraph.showGraph(incAvgs); // Show graph for increasing input averages
//        ResultsGraph.showGraph(decAvgs); // Show graph for decreasing input averages
    }

    /**
     * run randomInputTests
     * @param size input size
     */
    public static double[] randomInputsTester(int size) {
        Sort s = new Sort();
        long startTime =  0, endTime;
        long[][] durationList = new long [6][NUMITER];

        for (int i = 0; i < NUMITER; i++) {
            int [] array = new int [size];
            randomizeArray(array ,Integer.MAX_VALUE);

            for (int j = 0; j < 6; j++){
                switch (j) {
                    case 0:
                        Integer [] Sort1 = new Integer [size];    // In each case creating a new array to be sorted
                        copying(array, Sort1,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.mergeSortRecursive(Sort1);
                        break;
                    case 1:
                        Integer [] Sort2 = new Integer [size];
                        copying(array,Sort2,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.mergeSortIterative(Sort2);
                        break;
                    case 2:
                        Integer [] Sort3 = new Integer [size];
                        copying(array, Sort3,0,array.length-1);
                        startTime = System.currentTimeMillis();
//                        s.quickSortClass(Sort3);
                        break;
                    case 3:
                        Integer [] Sort4 = new Integer [size];
                        copying( array, Sort4,0,array.length-1);
                        startTime = System.currentTimeMillis();
//                        s.quickSortRecitation(Sort4);
                        break;
                    case 4:
                        int [] radix = new int [size];
                        radix = copying(array,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.radixSort(radix,2^15); // we found that the optimal base is 2^15
                        break;
                    case 5:
                        Integer [] Sort5 = new Integer [size];
                        copying(array,Sort5,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        Arrays.sort(Sort5); // Java's arrays sort
                        break;
                }
                endTime = System.currentTimeMillis();
                durationList[j][i] = endTime - startTime;
            }
        }
        System.out.println("Results for randomInputs:\n");
        double[] averages = getAverages(durationList, 5);
        double [] standardDeviations = getStandardDeviations(durationList, 5);
        printResults(averages, standardDeviations);

        return averages;
    }


    /**
     * run decreasing input tests
     * @param size to sort
     */
    public static double[] decreasingInputsTester(int size) {
        Sort s = new Sort();
        long startTime =  0, endTime =  0;
        long [][] durationList = new long [6][NUMITER];

        for (int i = 0; i < NUMITER; i++) {
            int [] array = new int [size];
            randomizeArray(array,Integer.MAX_VALUE);

            // creating decreasing order
            s.radixSort(array,2^15); // sorting the array
            array = reverseSortArray(array);

            for (int j = 0; j < 6; j++){
                switch (j) {
                    case 0:
                        Integer [] Sort1 = new Integer [size];       // In each case creating a new array to be sorted
                        copying(array, Sort1,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.mergeSortRecursive(Sort1);
                        break;
                    case 1:
                        Integer [] Sort2 = new Integer [size];
                        copying(array,Sort2,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.mergeSortIterative(Sort2);
                        break;
                    case 2:
                        Integer [] Sort3 = new Integer [size];
                        copying(array, Sort3,0,array.length-1);
                        startTime = System.currentTimeMillis();
//                        s.quickSortClass(Sort3);
                        break;
                    case 3:
                        Integer [] Sort4 = new Integer [size];
                        copying( array, Sort4,0,array.length-1);
                        startTime = System.currentTimeMillis();
//                        s.quickSortRecitation(Sort4);
                        break;
                    case 4:
                        int [] radix = new int [size];
                        radix = copying(array,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.radixSort(radix,2^15); // we found that the optimal base is 2^15
                        break;
                    case 5:
                        Integer [] Sort5 = new Integer [size];
                        copying(array,Sort5,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        Arrays.sort(Sort5); // Java's arrays sort
                        break;
                }
                endTime = System.currentTimeMillis();
                durationList[j][i] = endTime - startTime;
            }
        }
        System.out.println("Results for decreasingInputs:\n");
        double[] averages = getAverages(durationList, 5);
        double [] standardDeviations = getStandardDeviations(durationList, 5);
        printResults(averages, standardDeviations);

        return averages;
    }

    /**
     * run increasing input tests
     * @param size to sort
     */
    public static double[] increasingInputsTester(int size) {
        Sort s = new Sort();
        long startTime =  0, endTime =  0;
        long [][] durationList = new long [6][NUMITER];

        for (int i = 0; i < NUMITER; i++) {
            int [] array = new int [size];
            randomizeArray(array,Integer.MAX_VALUE);

            s.radixSort(array,2^15); // sorting the array

            for (int j = 0; j < 6; j++){
                switch (j) {
                    case 0:
                        Integer [] Sort1 = new Integer [size];       // In each case creating a new array to be sorted
                        copying(array, Sort1,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.mergeSortRecursive(Sort1);
                        break;
                    case 1:
                        Integer [] Sort2 = new Integer [size];
                        copying(array,Sort2,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.mergeSortIterative(Sort2);
                        break;
                    case 2:
                        Integer [] Sort3 = new Integer [size];
                        copying(array, Sort3,0,array.length-1);
                        startTime = System.currentTimeMillis();
//                        s.quickSortClass(Sort3);
                        break;
                    case 3:
                        Integer [] Sort4 = new Integer [size];
                        copying( array, Sort4,0,array.length-1);
                        startTime = System.currentTimeMillis();
//                        s.quickSortRecitation(Sort4);
                        break;
                    case 4:
                        int [] radix = new int [size];
                        radix = copying(array,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        s.radixSort(radix,2^15); // we found that the optimal base is 2^15
                        break;
                    case 5:
                        Integer [] Sort5 = new Integer [size];
                        copying(array,Sort5,0,array.length-1);
                        startTime = System.currentTimeMillis();
                        Arrays.sort(Sort5); // Java's arrays sort
                        break;
                }
                endTime = System.currentTimeMillis();
                durationList[j][i] = endTime - startTime;
            }
        }
        System.out.println("Results for increasingInputs:\n");
        double[] averages = getAverages(durationList, 5);
        double [] standardDeviations = getStandardDeviations(durationList, 5);
        printResults(averages, standardDeviations);

        return averages;
    }

    /**
     * tests for radix sorts. finding best base to work with bases on range of values for input
     * @param base given
     */
    public static void radixSortTest(int base) {
        Sort s = new Sort();
        long startTime = 0, endTime = 0;
        long[][] durationList = new long[3][NUMITER]; // 3 - for each base

        for (int i = 0; i < durationList[0].length; i++) {
            for (int j = 0; j < durationList.length; j++) {
                switch (j) { // each case represent a different range
                    case 0:
                        int r1 = (int) Math.pow(2, 10);
                        int[] arr1 = new int[500000];
                        randomizeArray(arr1, r1);
                        int[] radix1;
                        radix1 = copying(arr1, 0, arr1.length - 1);
                        startTime = System.currentTimeMillis();
                        s.radixSort(radix1, base);
                        break;
                    case 1:
                        int r2 = (int) Math.pow(2, 20);
                        int[] arr2 = new int[500000];
                        randomizeArray(arr2, r2);
                        int[] radix2;
                        radix2 = copying(arr2, 0, arr2.length - 1);
                        startTime = System.currentTimeMillis();
                        s.radixSort(radix2, base);
                        break;
                    case 2:
                        int r3 = (int) Math.pow(2, 30);
                        int[] arr3 = new int[500000];
                        randomizeArray(arr3, r3);
                        int[] radix3;
                        radix3=copying( arr3, 0, arr3.length - 1);
                        startTime = System.currentTimeMillis();
                        s.radixSort(radix3, base);
                        break;
                }
                endTime = System.currentTimeMillis();
                durationList[j][i] = endTime - startTime; // calculating the time
            }
        }
        System.out.println("Results for radixSort by range:\n");

        double[] averages = getAverages(durationList, 2);
        double [] standardDeviations = getStandardDeviations(durationList, 2);

        System.out.println("Range 2^10:   Average: " + averages[0] + " SD: " + standardDeviations[0]);
        System.out.println("Range 2^20:   Average: " + averages[1] + " SD: " + standardDeviations[1]);
        System.out.println("Range 2^30:   Average: " + averages[2] + " SD: " + standardDeviations[2]);
    }

    /**
     * calculating the average running time on all iterations
     * @param durationList list of all times
     * @param row sorting algo
     * @return average of running times
     */
    public static double calculateAverage(long [][] durationList, int row) {
        double sum = 0.0;
        for (int i = 0; i < durationList[row].length; i++){
            sum += durationList[row][i];
        }
        return sum / durationList[row].length;
    }

    /**
     * calculating standard deviation of all running times over all iterations
     * @param durationList list of all times
     * @param row sorting algo
     * @return standard deviation
     */
    public static double calculateStandardDeviation(long [] [] durationList,int row){
        double sumOfSquaredDiffs = 0.0;
        double cur;

        for (int i = 0; i < durationList[row].length; i++) {
            cur = calculateAverage(durationList, row);
            sumOfSquaredDiffs += Math.pow((cur - durationList[row][i]), 2);
        }
        return Math.sqrt(sumOfSquaredDiffs / durationList[row].length);
    }

    /**
     * calculate averages for all sorting algorithms
     * @param durationList times
     * @param sortingAlgo num of sorting algorithms
     * @return array of averages
     */
    public static double[] getAverages(long[][] durationList, int sortingAlgo) {
        double[] averages = new double[sortingAlgo+1];
        for (int i = 0; i <= sortingAlgo; i++) {
            averages[i] = calculateAverage(durationList, i);
        }
        return averages;
    }

    /**
     * calculate standard deviations for all sorting algorithms
     * @param durationList times
     * @param sortingAlgo number
     * @return array of standard deviations
     */
    public static double[] getStandardDeviations(long[][] durationList, int sortingAlgo) {
        double[] stds = new double[sortingAlgo+1];
        for (int i = 0; i <= sortingAlgo; i++) {
            stds[i] = calculateStandardDeviation(durationList, i);
        }
        return stds;
    }

    /**
     * print comparison results
     * @param averages list
     * @param stds list
     */
    public static void printResults(double[] averages, double[] stds) {
        System.out.println();
        for (int i = 0; i < sortingAlgorithms.length; i++) {
            System.out.println(sortingAlgorithms[i] + "        Average: " + averages[i] + " SD: " + stds[i]);
        }
        System.out.println();
    }

    /**
     * copying from one array to another, for generic integer array
     * @param from array
     * @param to array
     * @param start index
     * @param end index
     */
    public static void copying(int [] from, Integer[] to, int start, int end) {
        int cur = 0;
        for(int i = start; i <= end; i++){
            to[cur] = from[i];
            cur++;
        }
    }

    /**
     * copying from one array to another
     * @param from array
     * @param start index
     * @param end index
     */
    public static int [] copying(int[] from, int start, int end){
        int cur = 0;
        int[] to = new int [from.length];
        for(int i = start; i <= end; i++){
            to[cur] = from[i];
            cur++;
        }
        return to;
    }

    /**
     * creating a randomized array
     * @param array to fill
     * @param range of random values
     */
    public static void randomizeArray(int[] array, int range) {
        Random r = new Random();
        for (int i = 0 ; i < array.length; i++) {
            array[i] = r.nextInt(range);
        }
    }

    /**
     * creating an array sorted in decreasing order
     * @param array to sort in reverse
     * @return the revresed sorted array
     */
    public static int[] reverseSortArray(int[] array){
        int [] output = new int[array.length];
        int k = 0;
        for (int i = array.length-1; i >= 0; i--) {
            output[k] = array[i];
            k++;
        }
        return output;
    }
}
