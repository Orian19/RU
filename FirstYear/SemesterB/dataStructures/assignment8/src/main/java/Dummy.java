import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

public class Dummy {
    LinkedList<String> s = new LinkedList<String>();

    public static void main(String[] args) {
        Dummy s = new Dummy("1234");
        s.translocate(0, 1, 2);
        System.out.println(s.toString());
    }

    public Dummy(int n) {
        for (int i = 0; i < n; i++) {
            int c = (int) (Math.random() * 4);
            switch (c) {
                case 0:
                    s.add("A");
                    break;
                case 1:
                    s.add("T");
                    break;
                case 2:
                    s.add("C");
                    break;
                case 3:
                    s.add("G");
                    break;
                default:
                    break;
            }
        }
    }

    public Dummy(List<String> dummy) {
        s = new LinkedList<String>(dummy);
    }

    public Dummy(String dummy) {
        for (char c : dummy.toCharArray()) {
            s.add(String.valueOf(c));
        }
    }

    public Dummy(Dummy s1, Dummy s2) {
        s1.s.addAll(s2.s);
        s = s1.s;
    }

    public int size() {
        return s.size();
    }

    public String select(int k) {
        return s.get(k);
    }

    public Dummy[] split(int k) {
        Dummy d1 = new Dummy(s.subList(0, k));
        Dummy d2 = new Dummy(s.subList(k, s.size()));
        Dummy[] res = {d1, d2};
        return res;
    }

    public void substitute(int i, char c) {
        s.set(i, "" + c);
    }

    public void insert(int i, char c) {
        if (i < 0 || i > s.size()) {
            throw new IllegalArgumentException("Invalid index: " + i);
        }
        s.add(i, "" + c);
    }

    public void delete(int i) {
        if (i < 0 || i >= s.size()) {
            throw new IllegalArgumentException("Invalid index: " + i);
        }
        s.remove(i);
    }

    public void translocate(int i, int j, int k) {
        if (i < 0 || j < i || j >= s.size() || k < 0 || k > s.size()) {
            throw new IllegalArgumentException("Invalid index: " + k);
        }
        if (k >= i && k <= j) {
            throw new IllegalArgumentException("Invalid index: " + k);
        }

        LinkedList<String> sublist = new LinkedList<String>(s.subList(i, j + 1));

        // Remove the sublist from the original linked list
        s.subList(i, j + 1).clear();

        // Get the iterator at position k (relative to the sequence prior to the operation)
        k = k > j ? k - sublist.size() : k;
        ListIterator<String> iterator = s.listIterator(k);

        // Add the sublist elements at position k
        while (!sublist.isEmpty()) {
            iterator.add(sublist.removeFirst());
        }
    }

    public void invert(int i, int j) {
        if (i < 0 || j < i || j >= s.size()) {
            throw new IllegalArgumentException("Invalid indeces: " + i + ", " + j);
        }

        ListIterator<String> startIterator = s.listIterator(i);
        ListIterator<String> endIterator = s.listIterator(j + 1);

        while (startIterator.nextIndex() < endIterator.previousIndex()) {
            String temp = startIterator.next();
            startIterator.set(endIterator.previous());
            endIterator.set(temp);
        }
    }

    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        for (String c : s) {
            stringBuilder.append(c);
        }
        return stringBuilder.toString();
    }
}
