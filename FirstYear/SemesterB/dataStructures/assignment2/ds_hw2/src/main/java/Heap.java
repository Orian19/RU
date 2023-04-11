import java.lang.reflect.Array;
import java.util.Arrays;

// TODO: think about edge cases!!! + is it necessary to do circular? (for overflow)
public class Heap<T extends Comparable<T>> { // implementation max heap using circular arrays
    private T[] heap;
    private int maxSize = 50;
    private int size;
    private int start;

    /**
     * constructor - creates an empty array
     */
    public Heap() {
        this.heap = (T[]) Array.newInstance(Comparable.class, this.maxSize); // TODO: do I need to handle this warning?
        this.size = 0;
        this.start = 0;
    }
    /**
     * Adds t to the data structure
     * using percolate up method
     *
     * @param t the element
     */
    public void add(T t) {
        if (this.maxSize == this.size) {
            T[] tempHeap = (T[]) Array.newInstance(Comparable.class, this.maxSize * 2); // doubling array size the handle overflow
            System.arraycopy(this.heap, 0, tempHeap, 0, this.size); // copying elements to the new array
            this.maxSize = this.maxSize * 2;
            this.heap = tempHeap;
        }

        if (this.size == 0) {
            this.heap[0] = t;
            this.size++;
        } else {
            // inserting element t as the last element
            this.heap[size] = t;
            this.size++;

            // percolate up to the correct position
            percolateUp(size-1);
        }
    }

    /**
     *  Returns and removes the first / highest priority element.
     *
     * @return the element that has been removed = first
     */
    public T get() {
        if (this.size == 0) {
            return null;
        } else {
            // moving the last element to the root
            T max = this.heap[0];
            this.heap[0] = this.heap[size-1];
            this.size--;

            // percolate down to the correct position
            percolateDown(0);

            // return the root - which has the maximal value (max heap)
            return max;
        }
    }

    /**
     * Removes an element equal to t from the data structure, if exists. if
     * multiple elements exist, remove an arbitrary one.
     *
     * using percolate down method
     *
     * @param t the element
     */
    public void remove(T t) {
        if (this.size == 0) {
            return;
        }
        int eIdx = indexOf(t); // TODO: is there a better way to implement this method without using indexOf function?

        if (eIdx != -1) {
            // swapping the last element with t
            this.heap[eIdx] = this.heap[size-1];
            this.heap[size-1] = t;
            this.size--;

            // percolate down to the correct position
            percolateDown(eIdx);
        }
    }

    /**
     * percolate down to the correct position
     * @param idx start index for percolating + 1
     */
    private void percolateDown(int idx) {
        int parent = idx;
        int child1 = (2 * parent) + 1;
        int child2 = (2 * parent + 1) + 1;

        // while parent is smaller than one of its children
        while ((child1 < this.size && this.heap[parent].compareTo(this.heap[child1]) < 0) ||
                (child2 < this.size && this.heap[parent].compareTo(this.heap[child2]) < 0)) {

            int maxC = 0; // index of maximal child
            if (this.heap[child1].compareTo(this.heap[child2]) > 0) {
                maxC = child1;
            } else {
                maxC = child2;
            }
            T temp = this.heap[parent];
            this.heap[parent] = this.heap[maxC];
            this.heap[maxC] = temp;

            parent = maxC;
            child1 = 2 * parent;
            child2 = 2 * parent + 1;
        }
    }

    /**
     * percolate up to the correct position
     *
     * @param idx start index for percolating
     */
    private void percolateUp(int idx) {
        int parent = idx / 2;

        // while parent is bigger than one of its children
        while (this.heap[idx].compareTo(this.heap[parent]) > 0) {
            T temp = this.heap[idx];
            this.heap[idx] = this.heap[parent];
            this.heap[parent] = temp;

            idx = parent;
            parent = idx / 2;
        }
    }

    /**
     * returns the index of the given element
     *
     * @param t the element
     * @return index of the element in the array. returns -1 if not found
     */
    private int indexOf(T t) {
        for (int i = 0; i < size; i++) {
            if (this.heap[i].equals(t)) {
                return i;
            }
        }
        return -1;
    }
}
