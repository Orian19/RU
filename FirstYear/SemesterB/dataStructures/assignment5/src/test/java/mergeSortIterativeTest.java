import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class mergeSortIterativeTest {
    Sort s = new Sort();
    testHelper helper = new testHelper();
    int size = 8;//100000;

    @Test
    void testRandom() {
        s.setNaiveSortThreshold(6);
        Integer[]a = new Integer[size];
        helper.randomizeArray(a);
        s.mergeSortIterative(a);
        assertEquals(true, helper.isSortedArray(a));
    }

    @Test
    void testSorted() {
        Integer[]a = new Integer[size];
        helper.sortArray(a);
        s.mergeSortIterative(a);
        assertEquals(true, helper.isSortedArray(a));
    }

    @Test
    void testSortedOpposite() {
        Integer[]a = new Integer[size];
        helper.sortOpposite(a);
        s.mergeSortIterative(a);
        assertEquals(true, helper.isSortedArray(a));
    }
}


