import java.util.NoSuchElementException;

/**
 * Represents a generic linked list, and list-oriented operations.
 * The list can hold objects of any type.
 */
public class LinkedList<E> {
	
	// The following constant is used for localizing the line separator character,
	// which may be different in different host platforms.

	private Node<E> first;  // Points to the first node in the list (just after the dummy node)
	private Node<E> last;   // Points to the last node in the list
	private int size;       // Number of list elements
	
	/** 
	 * Creates a list with 0 elements.
	 */
	public LinkedList() {
		// Starts with a dummy node, to avoid handling empty lists.
		Node<E> dummy = new Node<E>(null);
		this.first =  dummy;
		this.last = first;
		this.size = 0;
	}
	
	/** 
	 * Adds the given element to the end of this list.
	 * @param e  the node to add
	 */
	public void add(E e) {
	    Node<E> newNode = new Node<E>(e);
		this.last.next = newNode;
		this.last = newNode;
		if (this.size == 0) {
			this.first.next = newNode;
		}
        this.size++;
	}

	/** 
	 * Adds the given element to the beginning of this list.
	 * @param e  the element to add to the list.
	 */
	public void addFirst(E e) {
		// Put your code here
		Node<E> newNode = new Node<E>(e);
		newNode.next = first.next;
		this.first.next = newNode;
        this.size++;
	}
	
	/**
	 * Adds the given element at the n'th location of this list.
	 * The index of the list's first element is 0.
	 * The index of the list's last element is the list's size.
	 * @param  e the element to add
	 * @param  index the index
	 * @throws IllegalArgumentException if index is negative or larger than the list's size
	 */
	public void add(E e, int index) {
		// Put your code here
		Node<E> newNode = new Node<E>(e);
		Node<E> prev = this.first;
		Node<E> cur = this.first.next;
		int curIdx = 0;

		try {
			while (curIdx < index) {
				prev = cur;
				cur = cur.next;
				curIdx++;
			}
			newNode.next = cur;
			prev.next = newNode;
			if (index == size) {
				this.last = newNode;
			}
			this.size++;
		} catch (IllegalArgumentException excep) {
			throw new IllegalArgumentException();
		}
	}
	
	/** 
	 * Returns the index of the given element in this list, or -1 if not found.
	 * @param  e the returned index will be the index of e.
	 * @return the index of the given element in this list
	 */
	public int indexOf (E e) {
	    // Replace the following statement with your code
		if (size == 0) {
			return -1;
		}

		Node<E> cur = this.first.next;
		int curIdx = 0;

		while (curIdx < size) {
			if (cur.e == e) {
				return curIdx;
			}
			cur = cur.next;
			curIdx++;
		}
	    return -1;
	}

	/** 
	 * If the given element exists in this list, removes it and returns true.
	 *  Otherwise, returns false.
	 * @param  e the element to remove.
	 * @return if the given element exists in this list, removes it and
	 * returns true. Otherwise, returns false.
	 */
	 public boolean remove(E e) {
		// Replace the following statement with your code
		int idx = indexOf(e);
	    if (idx == -1) {
			return false;
		}
		Node<E> prev = this.first;
		Node<E> cur = first.next;
		while (cur.e != e) {
			prev = cur;
			cur = cur.next;
		}
		prev.next = cur.next;
		cur = null;
		if (idx == size - 1) {
			this.last = prev;
		}
		size--;
		return true;
	}

	/** 
	 * Returns the first element in this list.
	 * @return the first element in this list.
	 * @throws NoSuchElementException if the list is empty
	 */
	// public E getFirst() {
	// 	// Replace the following statement with your code
	// 	try {
	// 		return this.first.next.e;
	// 	} catch (NoSuchElementException e) {
	// 		throw new NoSuchElementException();
	// 	}		
	// }

	public E getFirst() {
		// Replace the following statement with your code
		if(this.first.next == null){
			throw new NoSuchElementException();
		}
		return this.first.next.e;
	}
	
	/** 
	 * Returns the last element in this list.
	 * @return the last element in this list.
	 * @throws NoSuchElementException if the list is empty
	 */
	public E getLast() {
		// Replace the following statement with your code
		try {
			return this.last.e;
		} catch (NoSuchElementException e) {
			throw new NoSuchElementException();
		}
	}
	
	/** 
	 * Returns the size of this list.
	 * @return the size of this list.
	 */
	public int size() {
	    return this.size;
	}
	
	/** 
	 * Returns an itertaor on this list.
	 * @return an itertaor on this list.
	 */
	public ListIterator<E> iterator() {
	    return new ListIterator<E>(this.first.next);
	}
	
	/**
	 * A textual representation of the elements of this list.
	 * Each element is displayed in a separate line.
	 */
	public String toString() {
	    if (this.size == 0) {
			return "()";
		}
	    StringBuilder sb = new StringBuilder();
	    ListIterator<E> itr = this.iterator();
	    while (itr.hasNext()) {
	        sb.append(itr.next().toString() +  "\n");
	    }
	    return sb.toString();
	}
	
	// The main method of this class can be used for testing the
	// LinkedList methods. Clients of the class will normally not use it.
	public static void main(String[] args) {
		// Creates a list of Integer objects, add some elements, and prints the list.
		LinkedList<Integer> list = new LinkedList<Integer>();
		list.add(3);
		list.add(7);
		list.add(9);
		System.out.println(list);
		
		// As you implement the LinkedList class methods, write your testing 
		// code below. If you want, you can use private testing methods.
		// list.add(20, 3);
		// System.out.println(list);

		System.out.println(list.getLast());
		System.out.println();

		list.addFirst(1);
		System.out.println(list);

		System.out.println(list.getLast());
		System.out.println();

		list.addFirst(2);
		System.out.println(list);

		list.add(5, 0);
		System.out.println(list);

		System.out.println(list.getLast());
		System.out.println();

		list.add(10, 3);
		System.out.println(list);

		System.out.println(list.indexOf(1));
		System.out.println(list.indexOf(2));
		System.out.println(list.indexOf(11));
		System.out.println();

		list.remove(2);
		System.out.println(list);

		System.out.println(list.getLast());
		System.out.println();

		list.remove(9);
		System.out.println(list);

		System.out.println(list.getLast());
		System.out.println();

		list.remove(1);
		System.out.println(list);

		System.out.println(list.getFirst());
		System.out.println();
		System.out.println(list);
		System.out.println();
		System.out.println(list.getLast());

		
		testExceptions();
	}
	
	// Exception testing method.
	private static void testExceptions() {
	    // Creates a list of Integer objects
		LinkedList<Integer> list = new LinkedList<Integer>();
			
		// After you'll implement the getFirst() method, the statement below
		// should cause the program to crash.
		// To prevent it, wrap the method call with try/catch code. 
	    list.getFirst(); // Tries to get an element from the list, which is empty
				
	    // Adds three elements to the list, and prints it
		list.add(3);
		list.add(7);
		list.add(9);
		System.out.println(list);
			
		// After you'll implement the add(e,index) method, the statement below
		// should cause the program to crash.
		// To prevent it, wrap the method call with try/catch code. 
		list.add(8,-2);	// Tries to insert an element in index -2.
					
		// After you'll implement the add(e,index) method, the statement below
		// should cause the program to crash.
		// To prevent it, wrap the method call with try/catch code. 
		list.add(8,10); // Tries to insert an element in index 10.
	}
}
