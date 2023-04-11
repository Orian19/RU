public class Queue <T> { // implementation using DoublyLinkedList
    private Node<T> first; // points to the first node
    private Node<T> last; // points to the last node
    private int size; // the number of elements in the list

    /**
     * constructor - creates a list with 0 elements
     */
    public Queue() {
        // starts with a dummy node, to avoid creating an empty list
        Node<T> dummy = new Node<>(null);
        this.first = dummy;
        this.last = first;
        this.size = 0;
    }

    /**
     * Adds t to the data structure
     *
     * @param t the element
     */
    public void add(T t) {
        Node<T> newNode = new Node<>(t);
        if (this.size == 0) {
            this.last = newNode;
            this.first.next = this.last;
        } else {
            newNode.next = this.first.next;
            this.first.next.prev = newNode;
            this.first.next = newNode;
        }
        newNode.prev = this.first;
        this.size++;
    }

    /**
     *  Returns and removes the first / highest priority element.
     *
     * @return the element that has been removed = last
     */
    public T get() {
        T highestP = this.last.t; // get the highest priority (FIFO --> last element)

        if (this.first == null || this.size == 0) {
            return null;
        } else {
            this.last = this.last.prev;
            this.last.next = null;
            this.size--;
            return highestP;
        }
    }

    /**
     * Removes an element equal to t from the data structure, if exists. if
     * multiple elements exist, remove an arbitrary one.
     *
     * @param t the element
     */
    public void remove(T t) {
        if (this.first == null || this.size == 0) {
            return;
        }

        Node<T> cur = this.first.next;

        while (cur != null && cur != this.first) {
            if (cur.t.equals(t)) { // TODO: use compareTo or equals?
                if (cur == this.first.next) {
                    this.first.next = cur.next;
                    if (cur.next != null) {
                        cur.next.prev = null;
                    } else {
                        this.last = this.first;
                    }
                } else if (cur == this.last) {
                    this.last = cur.prev;
                    this.last.next = null;
                } else {
                    cur.prev.next = cur.next;
                    cur.next.prev = cur.prev;
                }
                this.size--;
                return;
            }
            cur = cur.next;
        }
    }
}
