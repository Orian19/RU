public class Node<T> {
    T t; // the T object that this node refers to
    Node<T> next;
    Node<T> prev;

    /**
     * constructs a generic node that refers to the given element
     * @param t the element
     */
    public Node(T t) {
        this.t = t;
        this.next = null;
        this.prev = null;
    }
}
