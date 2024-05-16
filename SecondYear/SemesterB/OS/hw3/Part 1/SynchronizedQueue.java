/**
 * A synchronized bounded-size queue for multithreaded producer-consumer applications.
 * producers = threads(workers), create some data/ do some operation
 * consumers = needs to use some data
 *
 * @param <T> Type of data items
 */


public class SynchronizedQueue<T> {

    private T[] buffer; // queue of jobs
    private int producers;
    // TODO: Add more private members here as necessary
    private int IN;  // index of the end of the queue (enqueue here)
    private int OUT; // index of next job/item (dequeue here)
    private int COUNT;  // number of available items

    /**
     * Constructor. Allocates a buffer (an array) with the given capacity and
     * resets pointers and counters.
     *
     * @param capacity Buffer capacity
     */
    @SuppressWarnings("unchecked")
    public SynchronizedQueue(int capacity) {
        this.buffer = (T[]) (new Object[capacity]);
        this.producers = 0;
        // TODO: Add more logic here as necessary
        this.IN = 0;
        this.OUT = 0;
        this.COUNT = 0;
    }

    /**
     * Dequeues the first item from the queue and returns it.
     * If the queue is empty but producers are still registered to this queue,
     * this method blocks until some item is available.
     * If the queue is empty and no more items are planned to be added to this
     * queue (because no producers are registered), this method returns null.
     * <p>
     * using monitors for sync
     *
     * @return The first item, or null if there are no more items
     * @see #registerProducer()
     * @see #unregisterProducer()
     */
    public synchronized T dequeue() throws InterruptedException {
        // waiting for a producer to produce an item, so we can consume it (queue/buffer ie empty)
        while (this.producers > 0 && this.COUNT == 0) {
            this.wait();
        }
        // Entering Critical Section
        // the queue/buffer ie empty and no more items are planned to be added (no producers registered)
        if (this.producers == 0 && this.COUNT == 0) {
            return null;
        }
        T item = this.buffer[this.OUT]; // getting the value of the next item
        this.OUT = (this.OUT + 1) % this.getCapacity();  // advancing OUT (cyclic queue/buffer)
        this.COUNT--;
        this.notifyAll(); // notify all producers there is an empty spot in the buffer (scheduling determines which producer/thread will produce next)
        return item;
    }

    /**
     * Enqueues an item to the end of this queue. If the queue is full, this
     * method blocks until some space becomes available.
     *
     * @param item Item to enqueue
     */
    public synchronized void enqueue(T item) throws InterruptedException {
        // waiting for a consumer to consume an item, so we can produce new items
        while (this.COUNT == this.getCapacity()) {
            this.wait();
        }
        // Entering Critical Section
        this.buffer[this.IN] = item; // inserting the item to the next spot in the buffer/queue
        this.IN = (this.IN + 1) % this.getCapacity(); // advancing IN (cyclic queue/buffer)
        this.COUNT++;
        this.notifyAll();  // notify all consumers there is an available item to consume
    }

    /**
     * Returns the capacity of this queue
     *
     * @return queue capacity
     */
    public int getCapacity() {
        return this.buffer.length;
    }

    /**
     * Returns the current size of the queue (number of elements in it)
     *
     * @return queue size
     */
    public int getSize() {
        return this.COUNT;
    }

    /**
     * Registers a producer to this queue. This method actually increases the
     * internal producers counter of this queue by 1. This counter is used to
     * determine whether the queue is still active and to avoid blocking of
     * consumer threads that try to dequeue elements from an empty queue, when
     * no producer is expected to add any more items.
     * Every producer of this queue must call this method before starting to
     * enqueue items, and must also call <see>{@link #unregisterProducer()}</see> when
     * finishes to enqueue all items.
     *
     * @see #dequeue()
     * @see #unregisterProducer()
     */
    public void registerProducer() {
        // TODO: This should be in a critical section
        this.producers++;
    }

    /**
     * Unregisters a producer from this queue. See <see>{@link #registerProducer()}</see>.
     *
     * @see #dequeue()
     * @see #registerProducer()
     */
    public synchronized void unregisterProducer() {
        // TODO: This should be in a critical section
        this.producers--;
    }
}
