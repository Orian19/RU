package Q1;

public abstract class S implements I {
    public void doThat(ActualDoer d) {
        d.do2();
        d.do1();
    }
}
