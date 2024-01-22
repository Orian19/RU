class TestQ2 {
    public static void main(String[] args) {
        A[] arr = {new A(), new B(), new B(), new C(), new B(), new B()}; // new B() = 2, new C() = 3+2, new C() = 5 * 2

        for (int i = 1; i < arr.length; ++i) {
            arr[i].set(arr[i-1].get());
        }

        System.out.println(arr[arr.length - 1].get());
    }
}