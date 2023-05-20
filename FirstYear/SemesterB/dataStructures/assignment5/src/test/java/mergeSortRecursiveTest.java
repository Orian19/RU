import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class mergeSortRecursiveTest {
    Sort s = new Sort();
    testHelper helper = new testHelper();
    int size = 100000;

    @Test
    void testRandom() {
        Integer[]a = new Integer[size];
        helper.randomizeArray(a);
        s.mergeSortRecursive(a);
        assertEquals(true, helper.isSortedArray(a));
    }

    @Test
    void testSorted() {
        Integer[]a = new Integer[size];
        helper.sortArray(a);
        s.mergeSortRecursive(a);
        assertEquals(true, helper.isSortedArray(a));
    }

    @Test
    void testSortedOpposite() {
        Integer[]a = new Integer[size];
        helper.sortOpposite(a);
        s.mergeSortRecursive(a);
        assertEquals(true, helper.isSortedArray(a));
    }
}


