package Q1;

public class TestQ1 {
    public static void doItAll(ActualDoer doer) {
        I obj1 = new A();  // doThis does do1() first
        I obj2 = new B();  // doThis does do2() first

        obj1.doThat(doer); // do2(), do1()
        obj2.doThis(doer); // do2()
        obj1.doThis(doer); // do1()
        obj2.doThat(doer); // do2(), do1()
    }
}
