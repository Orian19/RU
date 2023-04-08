import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class QueueTest {

    @Test
    void testQueueInteger() {
        Queue<Integer> queue = new Queue<>();

        queue.add(1);
        queue.add(2);
        queue.add(3);

        assertEquals(1, queue.get());
        assertEquals(2, queue.get());
        assertEquals(3, queue.get());
        assertEquals(null, queue.get());
    }

    @Test
    void testQueueString() {
        Queue<String> queue = new Queue<>();

        queue.add("4");
        queue.add("d");

        assertEquals("4", queue.get());
        assertEquals("d", queue.get());
        assertEquals(null, queue.get());
    }

    @Test
    void testEmptyQueue() {
        Queue<Integer> queue = new Queue<>();
        assertEquals(null, queue.get());
    }
}