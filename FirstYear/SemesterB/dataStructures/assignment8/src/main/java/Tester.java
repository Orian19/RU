public class Tester {
    public static void main(String[] args) {

    }

    public static SplayTree randomTest(Dummy d, SplayTree t) {
        int op = (int) (Math.random() * 6);
        return opTest(d, t, op);
    }

    public static SplayTree opTest(Dummy d, SplayTree t, int n) {
        switch (n) {
            case 0:
                return testDelete(d, t);
            case 1:
                return testInsert(d, t);
            case 2:
                return testInvert(d, t);
            case 3:
                return testJoin(d, t);
            case 4:
                return testSubstitute(d, t);
            case 5:
                return testTranslocate(d, t);
            default:
                return t;
        }
    }

    public static boolean compareStrings(Dummy d, SplayTree t) {
        return d.toString().equals(t.toString()) && ((t.root == null && d.size() == 0) || d.size() == t.root.size);
    }

    public static boolean compareStrings(String n, SplayTree.Node node, int length) {
        return n.equals("" + node.character) && node.size == length;
    }

    public static SplayTree testJoin(Dummy d, SplayTree t) {
        Dummy d2 = new Dummy((int) (Math.random() * 10));
        SplayTree t2 = new SplayTree(d2.toString());
        Dummy jointDummy = new Dummy(d, d2);
        SplayTree jointTree = new SplayTree(t, t2);
        t = jointTree;
        d = jointDummy;
        return t;
    }

    public static boolean testSelect(Dummy d, SplayTree t) {
        int n = randomIndex(d);
        return compareStrings(d.select(n), t.select(n), d.size());
    }

    public static SplayTree testSubstitute(Dummy d, SplayTree t) {
        int i = randomIndex(d);
        char c = randomChar();
        d.substitute(i, c);
        t.substitute(i, c);
        return t;
    }

    public static SplayTree testInsert(Dummy d, SplayTree t) {
        int i = randomIndex(d);
        char c = randomChar();
        d.insert(i, c);
        t.insert(i, c);
        return t;
    }

    public static SplayTree testDelete(Dummy d, SplayTree t) {
            int i = randomIndex(d);
            d.delete(i);
            t.delete(i);
        return t;
    }

    public static SplayTree testTranslocate(Dummy d, SplayTree t) {
        int i = randomIndex(d);
        int j = randomIndex(d, i);
        int k = randomIndex(d, i, j);
        if (k > -1) {
            d.translocate(i, j, k);
            t.translocate(i, j, k);
        }
        return t;
    }

    public static SplayTree testInvert(Dummy d, SplayTree t) {
        int i = randomIndex(d);
        int j = randomIndex(d, i);
        d.invert(i, j);
        t.invert(i, j);
        return t;
    }

    private static char randomChar() {
        int c = (int) Math.random() * 4;
        switch (c) {
            case 0:
                return 'A';
            case 1:
                return 'T';
            case 2:
                return 'C';
            default:
                return 'G';
        }
    }

    public static int randomIndex(Dummy d) {
        return (int) (Math.random() * d.toString().length());

    }

    private static int randomIndex(Dummy d, int i) {
        return i + (int) (Math.random() * (d.toString().length() - i));

    }

    private static int randomIndex(Dummy d, int i, int j) {
        int k1 = i == 0 ? -1 : (int) (Math.random() * i);
        int k2 = j == d.size() - 1 ? -1 : randomIndex(d, j + 1);
        if (k1 == -1 && k2 == -1) {
            return -1;
        }
        if (k1 == -1) {
            return k2;
        }
        if (k2 == -1) {
            return k1;
        }
        return Math.random() < 0.5 ? k1 : k2;
    }
}
