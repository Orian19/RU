package Q2;

import java.util.*;

public class Test2 {
    static List<Integer> sort(List<Integer> input) {
        List<Integer> output = new ArrayList<>();

        // list always sorted and duplicates removed
        Set<Integer> set = new TreeSet<>(input);

        for (Integer value: set)
            output.add(value);

        return output;
    }

    public static void main(String[] args) {
        List<Integer> integerList = List.of(1, 3, 5, 2, 4);
        sort(integerList);
    }
}


