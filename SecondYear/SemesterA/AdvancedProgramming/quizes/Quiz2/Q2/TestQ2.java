package Q2;

public class TestQ2 {
    static int calcMod(int a, int b) {
        Modder modder = (x, y) -> x % y;

        return modder.mod(a,b);
    }
}
