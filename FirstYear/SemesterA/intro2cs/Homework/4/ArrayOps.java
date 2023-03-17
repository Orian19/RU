public class ArrayOps {
    public static void main(String[] args) {
        int [] arr1 = {1,2,3};
        int [] arr2 = {1,-8,10};
        int [] arr3 = {1,-2,3};
        int [] arr4 = {1,1,3};
        int [] arr5 = {8, 2, 3, 4, 5};
        int [] arr6 = {1, 7, 3, 4, 5};
        int [] arr7 = {1, 2, 3, 4, 5};
        int [] arr8 = {0,1, 2, 3, 4, 6};
        int [] arr9 = {2, 3, 1};
        int [] arr10 = {0, 1};
        int [] arr11 = {1, 3, 5};
        int [] arr12 = {2, 4, 6};
        int [] arr13 = {1};
        int [] arr14 = {1, 2, 3};
        int [] arr15 = {2, 8, 3, 7, 8};
        int [] arr16 = {2, 3, 1, 4};
        int [] arr17 = {2};
        int [] arr18 = {0};
        int [] arr19 = {11, 9, 8, 9, 10};
        // System.out.println(sumArray(arr1)); // 6
        // System.out.println(sumArray(arr2)); // 3
        // System.out.println(isSorted(arr1)); //true
        // System.out.println(isSorted(arr3)); // false
        // System.out.println(isSorted(arr4)); // true
        // System.out.println(bruteForce(arr5, 8)); // 0
        // System.out.println(bruteForce(arr6, 5)); // 4
        // System.out.println(bruteForce(arr7, 6)); // -1
        // System.out.println(findMissingInt(arr8)); // 5
        // System.out.println(findMissingInt(arr9)); // 0
        // System.out.println(findMissingInt(arr10)); // 1
        // System.out.println(findMissingInt(arr16)); // 1
        // System.out.println(findMissingInt(arr17)); // 1
        // System.out.println(findMissingInt(arr18)); // 1
        System.out.println(secondMaxValue(arr19));
        // printArray(merge(arr11, arr12)); // {1, 2, 3, 4, 5, 6}
        // System.out.println();
        // printArray(merge(arr13, arr14)); // {1, 1, 2, 3}
    }

    public static void printArray(int [] array) {
        for (int i = 0; i < array.length; i++) {
            System.out.print(array[i] + ", ");
        }
    }

    public static int sumArray(int [] array) {
        int sum = 0;
        for (int i = 0; i < array.length; i++) {
            sum += array[i];
        }
        return sum;
    }

    public static boolean isSorted(int [] array) {
        boolean sorted = true;
        int curNum = 0;
        int nextNum = 0;
        for (int i = 0; i < array.length; i++) {
            if (i == array.length - 1) {
                break;
            }
            curNum = array[i];
            nextNum = array[i + 1];
            if (curNum > nextNum) {
                sorted = false;
            }
        }
        return sorted;
    }

    public static int bruteForce (int [] array, int value){
        for (int i = 0; i < array.length; i++) {
            if (array[i] == value) {
                return i;
            } 
        }
        return -1;
    }
    
    public static int binarySerach (int [] array, int value){
        // Write your code here:
        return -1;

    }

    public static int secondMaxValue(int [] array) {
        int temp = 0;

        for (int i = 0; i < array.length; i++) {
            for (int j = i + 1; j < array.length; j++) {
                if (array[i] > array[j]) {
                    temp = array[i];
                    array[i] = array[j];
                    array[j] = temp;
                }
            } 
        }
        // int secMax = array[0];
        // int max = array[0];

        // for (int i = 0; i < array.length; i++) {
        //     if (array[i] > max) {
        //         secMax = max;
        //         max = array[i];
        //     } else if (array[i] > secMax) {
        //         secMax = array[i];
        //     } 
        // }
        return array[array.length - 2];
    }

    public static int findMissingInt (int [] array){
       int missingNum = 0;
       boolean exists = false;

       for (int i = 1; i <= array.length; i++) {
        for (int j = 0; j < array.length; j++) {
            System.out.println(i + ", " + array[j]);
            if (array[j] == i) {
                exists = true;
                break;
            } else {
                exists = false;
            }
        }
        System.out.println(i + ", " + exists);
        if (!exists) {
            missingNum = i;
        }
       }
       return missingNum;
    }

    public static int[] merge(int [] array1,int [] array2) {
        int [] merged = new int [array1.length + array2.length];
        int idx1 = 0;
        int idx2 = 0;
        int mergedIdx = 0;

        while(idx1 < array1.length && idx2 < array2.length) {
            if (array1[idx1] < array2[idx2]) {
                merged[mergedIdx] = array1[idx1];
                idx1++;
                mergedIdx++;
            } else {
                merged[mergedIdx] = array2[idx2];
                idx2++;
                mergedIdx++;
            }
        }

        while (idx1 < array1.length) {
            merged[mergedIdx] = array1[idx1];
            idx1++;
            mergedIdx++;
        }

        while (idx2 < array2.length) {
            merged[mergedIdx] = array2[idx2];
            idx2++;
            mergedIdx++;
        }
        return merged;
    }


}


