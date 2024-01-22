public class TestQ1 {
    static Foo x = new Bar();

    public static void main(String[] args) {
        Foo y = new Foo();

        // current values: Vars.a=3, Vars.b=3, Vars.c=2
        y = new Foo();
        x = new Bar();

        // current values: Vars.a=3, Vars.b=2, Vars.c=5
        y = new Foo();
        y = new Foo();
        x = new Bar();

        // current values: Vars.a=2, Vars.b=5, Vars.c=6
        y = new Foo();
        y = new Foo();

        // current values: Vars.a=4, Vars.b=5, Vars.c=6
    }
}