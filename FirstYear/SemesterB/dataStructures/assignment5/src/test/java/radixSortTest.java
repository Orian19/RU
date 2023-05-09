import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class radixSortTest {
    testHelper helper = new testHelper();
    int size = 100000;

    @Test
    void testRandom() {
        int[]a = new int[size];
        for(int i = 3; i < 20; i+= 3){
            helper.randomizeArray(a);
            Sort.radixSort(a, i);
            assertEquals(true, helper.isSortedArray(a));
        }
    }

    @Test
    void testSorted() {
        int[]a = new int[size];

        for(int i = 3; i < 12; i+= 3) {
            helper.sortArray(a);
            Sort.radixSort(a, 6);
            assertEquals(true, helper.isSortedArray(a));
        }
    }

    @Test
    void testSortedOpposite() {
        int[]a = new int[size];

        for(int i = 3; i < 12; i+= 3) {
            helper.sortOpposite(a);
            Sort.radixSort(a, 6);
            assertEquals(true, helper.isSortedArray(a));
        }
    }

}
