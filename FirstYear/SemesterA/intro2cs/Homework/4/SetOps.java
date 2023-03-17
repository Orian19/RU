public class SetOps {
    public static void main(String[] args) {
        int [] arr1 = {1,2,3}; 
        int [] arr2 = {1,2,3,1}; 
        int [] arr3 = {1,1,1,1}; 
        int [] arr4 = {1,2,3,4}; 
        int [] arr5 = {1,2,2,1,1,4}; 
        int [] arr6 = {1,2,1,2,3}; 
        int [] arr7 = {4,3,2,7}; 
        int [] arr8 = {1,2,2,1,1,4}; 
        int [] arr9 = {3,2,1}; 
        int [] arr10 = {2,3}; 
        int [] arr11 = {1,2,3,4,5}; 
        int [] arr12 = {6,2,3,8,11}; 
        int [] arr13 = {1,2,3,5}; 
        int [] arr14 = {2,4,3,6,7}; 
        int [] arr15 = {1,2,3,5}; 
        int [] arr16 = {2,4,3,6,7}; 
        // System.out.println(contains(arr1,1)); //true
        // System.out.println(contains(arr1,4)); //false
        // System.out.println(contains(arr2,1,0)); //false
        // System.out.println(contains(arr2,1,4)); //true
        // System.out.println(contains(arr1,4,2)); //false
        // System.out.println(uniqueElements(arr2)); //3
        // System.out.println(uniqueElements(arr3)); //1
        // System.out.println(uniqueElements(arr4)); //4
        // printArray(buildSet(arr2)); //{1,2,3}
        // System.out.println();
        // printArray(buildSet(arr5)); //{1,2,4}
        // printArray(unionSets(arr6,arr7)); // {1,2,3,4,7}
        // System.out.println();
        // printArray(unionSets(arr7,arr6)); // {1,2,3,4,7}
        // System.out.println();
        // printArray(unionSets(arr1,arr9)); // {1,2,3}
        // printArray(intersectionSets(arr1,arr10)); // {2,3}
        // System.out.println();
        // printArray(intersectionSets(arr11,arr12)); // {2,3}
        // System.out.println();
        // printArray(intersectionSets(arr12,arr11)); // {2,3}
        // printArray(diffSets(arr1,arr10)); // {1}
        // System.out.println();
        // printArray(diffSets(arr13,arr14)); // {1,5}
        // System.out.println();
        // printArray(diffSets(arr14,arr13)); // {4,6,7}
        printArray(symDiffSets(arr15,arr16)); // {1,5,4,6,7}
        printArray(symDiffSets(arr16,arr15)); // {1,5,4,6,7}
    }

    public static void printArray(int [] array) {
        for (int i = 0; i < array.length; i++) {
            System.out.print(array[i] + ", ");
        }
    }

    public static boolean contains(int [] array, int value) {
        for (int i = 0; i < array.length; i++) {
            if (array[i] == value) {
                return true;
            }
        }
        return false;
    }

    public static boolean contains(int [] array, int value, int index) {
        for (int i = 0; i < index; i++) {
            if (array[i] == value) {
                return true;
            }
        }
        return false;
    }

    public static int uniqueElements (int [] array){
        int [] uniqueArr = new int [array.length];
        boolean isUnique = false;
        int uniqueCounter = 0;
        for (int i = 0; i < array.length; i++) {
            for (int j = 0; j < uniqueArr.length; j++) {
                if (array[i] == uniqueArr[j]) {
                    isUnique = false;
                    break;
                } else {
                    isUnique = true;
                }
            }
            if (isUnique) {
                uniqueArr[i] = array[i];
                uniqueCounter++;
            }
        }
        return uniqueCounter;
    }

    public static int [] buildSet(int [] array) {
        int [] uniqueArr = new int [uniqueElements(array)];
        boolean isUnique = false;
        int uniqueArrIdx = 0;
        for (int i = 0; i < array.length; i++) {
            for (int j = 0; j < uniqueArr.length; j++) {
                if (array[i] == uniqueArr[j]) {
                    isUnique = false;
                    break;
                } else {
                    isUnique = true;
                }
            }
            if (isUnique) {
                uniqueArr[uniqueArrIdx] = array[i];
                uniqueArrIdx++;
            }
        }
        return uniqueArr;
    }

    public static int [] unionSets(int [] array1, int[] array2) {
        int [] unionSet = new int [array1.length + array2.length];
        int [] uniqueSet1 = buildSet(array1);
        int [] uniqueSet2 = buildSet(array2);
        for (int i = 0; i < uniqueSet1.length; i++) {
            unionSet[i] = uniqueSet1[i];
        }
        for (int i = 0; i < uniqueSet2.length; i++) {
            unionSet[unionSet.length - uniqueSet2.length + i] = uniqueSet2[i];
        }
        return buildSet(unionSet);
    }

    public static int [] intersectionSets(int [] array1, int[] array2) {
        int intersectionArrSize = 0;
        int uniqueSize1 =  uniqueElements(array1);
        int uniqueSize2 =  uniqueElements(array2);
        if (uniqueSize1 > uniqueSize2) {
            intersectionArrSize = uniqueSize2;
        } else {
            intersectionArrSize = uniqueSize1;
        }

        int [] tempIntersectionSet = new int [intersectionArrSize];
        int [] uniqueSet1 = buildSet(array1);
        int [] uniqueSet2 = buildSet(array2);
        int intersectionCounter = 0;
        int intersectionIdx = 0;

        for (int i = 0; i < uniqueSet1.length; i++) {
            if (contains(array2, array1[i])) {
                tempIntersectionSet[intersectionIdx] = array1[i];
                intersectionIdx++;
                intersectionCounter++;
            }
        }

        int [] intersectionSet = new int [intersectionCounter];
        for (int i = 0; i < intersectionSet.length; i++) {
            intersectionSet[i] = tempIntersectionSet[i];
        }
        return buildSet(intersectionSet);
    }

    public static int [] diffSets(int [] array1, int[] array2) {
        int diffArrSize = 0;
        int uniqueSize1 =  uniqueElements(array1);
        int uniqueSize2 =  uniqueElements(array2);
        if (uniqueSize1 > uniqueSize2) {
            diffArrSize = uniqueSize2;
        } else {
            diffArrSize = uniqueSize1;
        }

        int [] tempDiffSet = new int [diffArrSize];
        int [] uniqueSet1 = buildSet(array1);
        int [] uniqueSet2 = buildSet(array2);
        int diffCounter = 0;
        int diffIdx = 0;

        for (int i = 0; i < uniqueSet1.length; i++) {
            if (!contains(array2, array1[i])) {
                tempDiffSet[diffIdx] = array1[i];
                diffIdx++;
                diffCounter++;
            }
        }

        int [] diffSet = new int [diffCounter];
        for (int i = 0; i < diffSet.length; i++) {
            diffSet[i] = tempDiffSet[i];
        }
        return buildSet(diffSet);
    }

    public static int [] symDiffSets(int [] array1, int[] array2) {
        int [] interArr = intersectionSets(array1, array2);
        int [] symDiff1 = diffSets(array1, interArr); 
        int [] symDiff2 = diffSets(array2, interArr);
        int [] symDiffArr = unionSets(symDiff1, symDiff2); 

        return symDiffArr;
    }

}


