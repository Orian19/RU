public class Manager <T extends Comparable<T>> {
    private Queue<T> queue;
    private Heap<T> heap;
    private int size;

    public int getSize() {
        return this.size;
    }

    public Manager() {
        this.queue = new Queue<>();
        this.heap = new Heap<>();
        this.size = 0;
    }
    /**
     * adds element t to the system.
     *
     * @param t the element
     */
    public void add(T t) {
        this.queue.add(t);
        this.heap.add(t);
        this.size++;
    }

    /**
     * returns the element with the earliest creation time and
     * removes it from the system.
     *
     * @return element with the earliest creation time
     */
    public T getByCreationTime() {
        this.size--;
        T t = this.queue.get();
        this.heap.remove(t);
        return t;
    }

    /**
     *  returns the element with the highest priority and removes it from
     *     the system.
     *
     * @return element with the highest priority
     */
    public T getByPriority() {
        this.size--;
        T t = this.heap.get();
        this.queue.remove(t);
        return t;
    }
}
