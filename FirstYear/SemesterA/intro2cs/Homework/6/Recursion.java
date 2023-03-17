public class Recursion {
    // For Q8
    public static int steps = 0;

    public static void main(String[] args) {
        System.out.println(gcd(56, 42));
        System.out.println();
        System.out.println(removeVowels("hEllO worLD"));
        System.out.println(removeVowels("h3ll0 worLD"));
        System.out.println(removeVowels("h3ll0 worLD@!@352"));
        System.out.println();
        System.out.println(convert(26, 2));
        System.out.println(convert(234, 3));
        System.out.println(convert(767, 10));
        System.out.println();
        System.out.println(camelcase("Hello World"));
        System.out.println(camelcase("HELLO world"));
        System.out.println(camelcase("world"));
        System.out.println(camelcase(" Intro to coMPUter sCIEncE "));
        System.out.println();
        int [] arr1 = {1, 2, 3, 4};
        int [] arr2 = {1, 2, 3};
        int [] arr3 = {};
        int [] arr4 = {0};
        int [] arr5 = {1, 2, 3, 4, 5};
        int [] arr6 = {1, 2, 3, 5, 6};
        System.out.println(sumArr(arr1));
        System.out.println(sumArr(arr2));
        System.out.println(sumArr(arr3));
        System.out.println(sumArr(arr4));
        System.out.println(sumArr(arr5));
        System.out.println();
        System.out.println(binarySearch(arr5, 1));
        System.out.println(binarySearch(arr5, 5));
        System.out.println(binarySearch(arr5, 6));
        System.out.println(binarySearch(arr6, 4));
        System.out.println();
        int [] arr7 = {4, 2, 8};
        int [] arr8 = {4, 2, 8, 7, -1};
        int [] arr9 = {2, 6, 4, 2, 8};
        int [] arr10 = {4, 2, 3, 2, 1};
        int [] arr11 = {4, 2, 3, 2, 8};
        int [] arr12 = {4, 2, 3, 4, 1, 8};
        int [] arr13 = {2, 3, 4, 1, 8};
        int [] arr14 = {4, 4, 2};
        int [] arr15 = {4, 3, 4, 2};
        int [] arr16 = {4, 2, 4, 4};
        int [] arr17 = {4, 2, 4 ,6, 4};
        System.out.println(isSubArr(arr7, arr8));
        System.out.println(isSubArr(arr7, arr9));
        System.out.println(isSubArr(arr7, arr10));
        System.out.println(isSubArr(arr7, arr11));
        System.out.println(isSubArr(arr7, arr12));
        System.out.println(isSubArr(arr7, arr13));
        System.out.println(isSubArr(arr14, arr15));
        System.out.println(isSubArr(arr14, arr16));
        System.out.println(isSubArr(arr14, arr17));
        System.out.println();
        mainQuestion8();
    }

    public static int gcd (int a, int b){
        if (a == b) {
            return a;
        } else if (a > b) {
            return gcd(a - b, b);
        } else {
            return gcd(a, b - a);
        }
    }

    public static String removeVowels (String str){
        return removeVowels("", str);
    }

    private static String removeVowels (String vStr, String str){
        String vowels = "aeiouAEIOU";

        if (str.length() == 0) {
            return vStr;
        } else if (vowels.indexOf(str.charAt(0)) == -1) {
            vStr += str.charAt(0);
            return removeVowels(vStr, str.substring(1));
        } else {
            return removeVowels(vStr, str.substring(1));
        }
    }

    public static String convert (int num, int base){
        return convert("", num, base);     
    }

    private static String convert (String converted, int num, int base) {
        if (num == 0) {
            return reverseString(converted);
        } 
        converted += "" + num % base;
        return convert(converted , num / base, base);     
    }

    public static String reverseString (String str) {
        if (str.length() <= 1) {
            return str;
        } 
        return reverseString(str.substring(1)) + str.charAt(0);
    }

    public static String camelcase (String sentence){
        String captialized = captializeWord(sentence);
        String noSpaceCapped = removeSpaces(captialized);
        return letterToLower(noSpaceCapped.charAt(0)) + noSpaceCapped.substring(1);
    }
    
    public static String removeSpaces(String str){
        return removeSpaces("", str);
    }

    public static String removeSpaces(String cleanS, String str){
        if (str.length() == 0) {
            return cleanS;
        }
        if (str.charAt(0) != ' ') {
            cleanS += str.charAt(0);
            return removeSpaces(cleanS, str.substring(1));
        } else {
            return removeSpaces(cleanS, str.substring(1));
        }
    }

    public static String captializeWord(String str){
        return captializeWord(isLetter(str.charAt(0)), "", str);
    }

    public static String captializeWord(boolean flag, String cap, String str){
        if (str.length() == 0) {
            return cap;
        }
        char curChar = str.charAt(0); 
        if (isLetter(curChar)) {
            cap += !flag ? letterToUpper(curChar) : letterToLower(curChar);
            return captializeWord(isLetter(str.charAt(0)), cap, str.substring(1));
        } else {
            cap += curChar;
            return captializeWord(isLetter(str.charAt(0)), cap, str.substring(1));
        }
    }
    
    public static char letterToUpper(char chr){
        return ('a' <= chr && chr <= 'z') ? (char) (chr - ' ') : chr;
    }

    public static char letterToLower(char chr){
        return ('A' <= chr && chr <= 'Z') ? (char) (chr + ' ') : chr;
    }
    public static boolean isLetter(char chr){
        return ('a' <= chr && chr <= 'z') || ('A' <= chr && chr <= 'Z');
    }   

    public static int sumArr (int [] arr){
        if (arr.length == 0) {
            return 0;
        }
        return sumArr(arr, arr[arr.length - 1]);
    }
    public static int sumArr (int [] arr, int n){
        if (arr.length == 0) {
            return 0;
        }
        int lastValue = arr[arr.length - 1];
        return sumArr(removeLast(arr), lastValue) + lastValue;
    }

    public static int [] removeLast (int [] arr) {
        int [] removed = new int [arr.length - 1];
        return removeLast(0, removed, arr);
    }

    public static int [] removeLast (int idx, int [] removed, int [] arr) {
        if (idx == arr.length - 1) {
            return removed;
        }
        removed[idx] = arr[idx];
        return removeLast(idx + 1, removed, arr);
    }

    public static int binarySearch (int [] sortedArr, int value){
        if (sortedArr.length == 0) {
            return 0;
        }
        int sign = 1;
        if (sortedArr[sortedArr.length / 2] > value) {
            sign *= -1;
        } 
        return binarySearch(sign, sortedArr.length / 2, sortedArr, value);
    }

    public static int binarySearch (int sign, int idx, int [] sortedArr, int value){
        if (idx == sortedArr.length || idx == -1) {
            return -1;
        }

        if (sortedArr[idx] == value) {
            return idx;
        }
        return binarySearch(sign, idx + sign, sortedArr, value);
    }

    public static boolean isSubArr (int [] smaller, int [] bigger){
        return isSubArr(0, 0, smaller, bigger);
    }

    public static boolean isSubArr (int idxS, int idxB, int [] smaller, int [] bigger){
        if (idxS == smaller.length) {
            return true;
        }
        if (idxB == bigger.length) {
            return false;
        }

        if (smaller[idxS] == bigger[idxB]) {
            return isSubArr(idxS + 1, idxB + 1, smaller, bigger);
        }
        return isSubArr(idxS, idxB + 1, smaller, bigger);
    }

    public static void mainQuestion8() {
        // Put your testing code here. For example:
        int[][] a = { {3, 4, 5},
                      {2, 2, 0},
                      {1, 0, 1} };
        System.out.println(" Value of maximal path = " + maxVal(a)); 
        System.out.println(" Number of recursive steps = " + steps);
        steps = 0;                       
        System.out.println(" Value of maximal path = " + effMaxVal(a)); 
        System.out.println(" Number of recursive steps = " + steps);
    }

    // Returns the value of the maximal path in the given 2D array
    public static int maxVal(int[][] arr) {
        return maxVal(arr, 0, 0);
    }
    
    // Returns the value of the maximal path in the given 2D array, starting at location (i,j)
    private static int maxVal(int[][] arr, int i, int j) {
        steps++;

        if (i == arr.length || j == arr[0].length) {
            return 0;
        }
        
        int rightPos = maxVal(arr, i, j + 1);
        int downPos = maxVal(arr, i + 1, j);

        if (rightPos > downPos) {
            return arr[i][j] + rightPos;
        } else {
            return arr[i][j] + downPos;
        }
    }
    
    // Returns the value of the maximal path in the given 2D array, efficiently.
    public static int effMaxVal(int[][] arr) {
        // Creates a 2D array named memo, of the same dimensions as arr.
        // This array will be used for memorizing maxVal computations.
        // After creating the memo array, initializes all its elements to -1.
        // Next, initializes the value of the bottom-right cell of memo to the 
        // value of the bottom-right (destination) cell of arr.

        // Put the array creation and initialization code here:
        int [][] memo = new int [arr.length][arr[0].length];
        memo = initArr(memo, 0, 0);  
        memo[arr.length - 1][arr[0].length - 1] = arr[arr.length - 1][arr[0].length - 1];

        // Computes the value of the maximal path, using the memo array: 
        effMaxVal(arr, 0, 0, memo);

        // The maximal value is now stored in memo[0][0]:
        return memo[0][0];
    }

    public static int [][] initArr(int [][] arr, int i, int j) {
        if (i == arr.length || j == arr[i].length) {
            return arr;
        }
        arr[i][j] = -1;

        initArr(arr, i + 1, j);
        initArr(arr, i, j + 1);  

        return arr;
    }
    
    // Returns the value of the maximal path in the given 2D array, starting
    // at location (i,j), efficiently. By "efficiently" we mean as follows:
    // If the value was already computed, returns the value using memo.
    // Otherwise, computes the value, stores the value in memo,
    // and returns the value.
    // SIDE EFFECT: This function changes the contents of the given memo array.
    private static int effMaxVal(int[][] arr, int i, int j, int[][] memo) {
        steps++;
        
        if (i == arr.length || j == arr[0].length) {
            return 0;
        }

        if (memo[i][j] != -1) {
            return memo[i][j];
        }

        int rightPos = effMaxVal(arr, i, j + 1, memo);
        int downPos = effMaxVal(arr, i + 1, j, memo);

        if (rightPos > downPos) {
            memo[i][j] = arr[i][j] + rightPos;
            return arr[i][j] + rightPos;
        } else {
            memo[i][j] = arr[i][j] + downPos;
            return arr[i][j] + downPos;
        }
    }
}
