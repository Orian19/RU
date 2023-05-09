import java.util.Random;

public class testHelper <T extends Comparable<T>> {
    Random r;
    public testHelper(){
        r = new Random();
    }

    boolean isSortedArray(int[] array){
        int n = array.length;
        if(n == 1 || n == 0) return true;
        for(int i = 1; i < n; i++){
            if(array[i] < array[i-1] ) return false;
        }
        return true;
    }

    boolean isSortedArray(T[] array){
        int n = array.length;
        if(n == 1 || n == 0) return true;
        for(int i = 1; i < n; i++){
            if(array[i].compareTo(array[i-1]) < 0 ) return false;
        }
        return true;
    }

    void randomizeArray(T[] array){
        for(int i = 0 ; i < array.length; i++){
            array[i] = (T) Integer.valueOf(r.nextInt(Integer.MAX_VALUE));
        }
    }

    void randomizeArray(int[] array){
        for(int i = 0 ; i < array.length; i++){
            array[i] = r.nextInt(Integer.MAX_VALUE);
        }
    }

    public void sortArray(T[] a) {
        for(int i = 0 ; i < a.length; i++){
            a[i] = (T) Integer.valueOf(i);
        }
    }

    public void sortOpposite(T[] a) {
        for(int i = 0 ; i < a.length; i++){
            a[i] = (T) Integer.valueOf(a.length - i);
        }
    }

    public void sortArray(int[] a) {
        for(int i = 0 ; i < a.length; i++){
            a[i] = i;
        }
    }

    public void sortOpposite(int[] a) {
        for(int i = 0 ; i < a.length; i++){
            a[i] = a.length - i;
        }
    }
}
