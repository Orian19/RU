import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class checkFastestTest {
    Sort s = new Sort();
    testHelper helper = new testHelper();
    int size = 3000000;

    @Test
    void testFastestRandom() {
        long startTime = System.nanoTime();
        Integer[]a = new Integer[size];

        helper.randomizeArray(a);
        s.mergeSortIterative(a);
        assertEquals(true, helper.isSortedArray(a));
        long elapsedTime = System.nanoTime() - startTime;
        System.out.println("merge sort iterative total execution time in millis: "
                + elapsedTime/1000000);

        /* ========================================================== */

        startTime = System.nanoTime();
        a = new Integer[size];
        helper.randomizeArray(a);
        s.mergeSortRecursive(a);
        assertEquals(true, helper.isSortedArray(a));
        elapsedTime = System.nanoTime() - startTime;
        System.out.println("merge sort recursive total execution time in millis: "
                + elapsedTime/1000000);

        /* ========================================================== */

        startTime = System.nanoTime();
        a = new Integer[size];
        helper.randomizeArray(a);
        s.quickSortClass(a);
        assertEquals(true, helper.isSortedArray(a));
        elapsedTime = System.nanoTime() - startTime;
        System.out.println("quick sort class total execution time in millis: "
                + elapsedTime/1000000);

        /* ========================================================== */

        startTime = System.nanoTime();
        a = new Integer[size];
        helper.randomizeArray(a);
        s.quickSortRecitation(a);
        assertEquals(true, helper.isSortedArray(a));
        elapsedTime = System.nanoTime() - startTime;
        System.out.println("quick sort recitation total execution time in millis: "
                + elapsedTime/1000000);

        /* ========================================================== */

        int[]b = new int[size];
        helper.randomizeArray(b);
        s.radixSort(b, 10);
        assertEquals(true, helper.isSortedArray(b));

        elapsedTime = System.nanoTime() - startTime;
        System.out.println("radix sort total execution time in millis: "
                + elapsedTime/1000000);

        /* ========================================================== */

        b = new int[size];
        helper.randomizeArray(b);
        Arrays.sort(b);
        assertEquals(true, helper.isSortedArray(b));

        elapsedTime = System.nanoTime() - startTime;
        System.out.println("java sort total execution time in millis: "
                + elapsedTime/1000000);
    }

}


