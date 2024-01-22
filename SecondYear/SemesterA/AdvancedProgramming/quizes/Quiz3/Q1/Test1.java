package Q1;

class Test {
    public static <T> T identity(T t) {
        return t;
    }

    public static void main(String[] args) {
        String a1 = "test1";
        Integer b1 = 7;

        String a2 = identity(a1);
        Integer b2 = identity(b1);
    }
}
