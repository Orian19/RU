import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class HeapTest {

    @Test
    void generalTestHeap() {
        Heap<Integer> heap = new Heap<>();
        heap.add(2);
        heap.add(3);
        heap.add(1);

        assertEquals(3, heap.get());
        assertEquals(2, heap.get());
        assertEquals(1, heap.get());
    }

    @Test
    void testEmptyHeap() {
        Heap<Integer> heap = new Heap<>();
        assertEquals(null, heap.get());
    }

    @Test
    void generalTestHeap2() {
        Heap<Integer> heap = new Heap<>();
        heap.add(2);
        assertEquals(2, heap.get());
        heap.remove(2);
        heap.remove(3);
        assertEquals(null, heap.get());
    }

}