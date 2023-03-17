/*** you may only use the following String functions
1.  str.charAt(int i);
2.  str.length();
3.  str.substring(int start);
4.  str.substring(int start,int end);
*/
public class StringOps {
    public static void main(String[] args) {
        // System.out.println(trim("aaaaaahappy birthdayaaa",'a'));
        // System.out.println(trim("aaaaaaaaa",'a'));
        // String [] arr1 = tokenize(" x + Math.sqrt(x) - rate");
        // String [] arr2 = tokenize("WORD");
        // printStringArray(arr1); // {“x”, “+”, “Math.sqrt(x)”,”-“,”rate”}
        // printStringArray(arr2); // WORD
        // System.out.println();
        // System.out.println(arr.length); // 5
        // System.out.println(lastIndexOf("abccb",'b'));
        // System.out.println(lastIndexOf("abccb",'a'));
        // System.out.println(lastIndexOf("abccb",'v'));
        System.out.println(toBinary(6));
    }
    public static void printStringArray(String [] array) {
        for (int i = 0; i < array.length; i++) {
            System.out.print(array[i] + ", ");
        }
    }
    public static String trim(String str,char ch) {
        boolean isSameCh = false;
        int startTrimIdx = 0;
        int endTrimIdx = 0;
        int strLen = str.length();
        for (int i = 0; i < strLen; i++) {
            if (str.charAt(i) == ch) {
                isSameCh =  true;
            } else {
                isSameCh = false;
                startTrimIdx = i;
                break;
            }
        }

        for (int i = 0; i < strLen; i++) {
            if (str.charAt(strLen - i - 1) == ch) {
                isSameCh = true;
            } else {
                isSameCh = false;
                endTrimIdx = str.length() - i;
                break;
            }
        }
        return str.substring(startTrimIdx, endTrimIdx);
    }

    public static int lastIndexOf(String str, char ch) {
        int last = -1;
         for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) == ch) {
                last = i;
            }
         }
         return last;
    }

    public static String toBinary (int x){
        String tempBin = "";
        int remainder = 0;

        while (x != 0) {
            remainder = x % 2;
            x = x / 2;
            tempBin += "" + remainder;
        }
        System.out.println

        String bin = "";

        for (int i = 0; i < tempBin.length(); i++) {
            bin += tempBin.charAt(tempBin.length() - 1 - i);
        }
        return bin;
    }
    public static String [] tokenize(String str) {
        int wordCount = 0;
        String curWord = "";
        String [] tempArr = new String [str.length()];
        int arrIdx = 0;
        for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) != ' ') {
                curWord += "" + str.charAt(i);
                if (i == str.length() - 1) {
                    tempArr[arrIdx] = curWord;
                    wordCount++;
                    arrIdx++;
                }
            } else if (i != 0 && str.charAt(i - 1) != ' ') {
                tempArr[arrIdx] = curWord; 
                wordCount++;
                curWord = "";
                arrIdx++;
            }
        }
        String [] wordsArr = new String [wordCount];
        for (int i = 0; i < wordsArr.length; i++) {
            wordsArr[i] = tempArr[i];
        }
        return wordsArr;
    }

}


