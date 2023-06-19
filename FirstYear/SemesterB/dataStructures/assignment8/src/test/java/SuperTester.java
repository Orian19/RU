import org.junit.Test;
import static org.junit.Assert.*;

public class SuperTester {

    public void generalTest(int numiter, int len, int op) {
        for (int i = 0; i < numiter; i++) {
            int n = (int) (Math.random() * len);
            Dummy d = new Dummy(n);
            SplayTree t = new SplayTree(d.toString());
            for (int j = 0; j < n; j++) {
                t = Tester.opTest(d, t, op);
                assertTrue(Tester.compareStrings(d, t));
            }
        }
    }

    @Test
    public void randomTest() {
        for (int i = 0; i < 1000; i++) {
            int n = (int) (Math.random() * 100);
            Dummy d = new Dummy(n);
            SplayTree t = new SplayTree(d.toString());
            for (int j = 0; j < n; j++) {
                t = Tester.randomTest(d, t);
                assertTrue(Tester.compareStrings(d, t));
                if (Math.random() < (1 / 7)) {
                    SplayTree[] tt = { null, null };
                    Dummy[] dd = new Dummy[2];
                    while (t.root != null) {
                        dd = d.split(n);
                        tt = t.split(t.select(Tester.randomIndex(d)));
                        assertTrue(Tester.compareStrings(dd[0], tt[0]) && Tester.compareStrings(dd[1], tt[1]));
                    }
                }
            }
        }
    }

    @Test
    public void testDelete() {
        generalTest(100, 100, 0);
    }

    @Test
    public void testInsert() {
        generalTest(100, 100, 1);
    }

    @Test
    public void testInvert() {
        generalTest(100, 100, 2);
    }

    @Test
    public void testJoin() {
        generalTest(100, 100, 3);
    }

    @Test
    public void testSelect() {
        for (int i = 0; i < 100; i++) {
            int n = (int) (Math.random() * 100);
            Dummy d = new Dummy(n);
            SplayTree t = new SplayTree(d.toString());
            for (int j = 0; j < n; j++) {
                assertTrue(Tester.testSelect(d, t));
            }
        }
    }

    @Test
    public void testSplit() {
        for (int i = 0; i < 100; i++) {
            Dummy d = new Dummy(10000);
            SplayTree t = new SplayTree(d.toString());
            SplayTree[] tt = { null, null };
            Dummy[] dd = new Dummy[2];
            while (t.root != null) {
                int n = Tester.randomIndex(d);
                dd = d.split(n);
                tt = t.split(t.select(n));
                assertTrue(Tester.compareStrings(dd[0], tt[0]) && Tester.compareStrings(dd[1], tt[1]));
            }
        }
    }

    @Test
    public void testSubstitute() {
        generalTest(100, 100, 4);
    }

    @Test
    public void testTranslocate() {
        generalTest(100, 100, 5);
    }
}
