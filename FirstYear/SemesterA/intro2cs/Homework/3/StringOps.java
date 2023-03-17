public class StringOps {
    public static void main(String[] args) {
        System.out.println(charRunCount("Hello World", 'l'));
        System.out.println(charRunCount("Hello WorLd", 'L'));
        System.out.println(charRunCount("Hello WorLd", 'm'));
        System.out.println(lowercase("Hello World !!"));
        System.out.println(lowercase("ABCDE"));
        System.out.println(camelcase("Hello World"));
        System.out.println(camelcase("HELLO world"));
        System.out.println(camelcase("world"));
        System.out.println(camelcase(" Intro to coMPUter sCIEncE "));
        System.out.println(camelcase("             PHASE THREE  "));
        System.out.println(camelcase("WAKANDA             FOREVER"));
        System.out.println(camelcase("      Hello MY     NAme IS KorG aNd     I aM made of ROCKSSSSS "));
        System.out.println(camelcase("  OHHHH SnaPP"));
        System.out.println(stringToInt("-234"));
        System.out.println(stringToInt("+234"));
        System.out.println(stringToInt("234"));
        System.out.println(expand("G2T11C4A5T1G8C2"));
        System.out.println(compareTo("abc","abc"));
        System.out.println(compareTo("abc","ABC"));
        System.out.println(compareTo("abc","abcd"));
        System.out.println(compareTo("abc","abd"));
        System.out.println(compareTo("abc","abac"));
        System.out.println(compareTo("abc","ab"));
    }

    public static int charRunCount(String str, char c) {
        int count = 0;
        for (int i = 0; i < str.length(); i++) {
            if ((int) str.charAt(i) == c) {
                count++;
            }
        }
        return count;
    }

    public static String lowercase(String str) {
        int changesLower = 32; // from ascii table
        String lowCaseStr = "";
        for (int i = 0; i < str.length(); i++) {
            char curChar = str.charAt(i);
            if (curChar >= 65 && curChar <= 90) {
                curChar += changesLower;
            }
            lowCaseStr += curChar;
        }
        return lowCaseStr;
    }

    public static String uppercase(String str) {
        int changesUpper = -32; // from ascii table
        String uppCaseStr = "";
        for (int i = 0; i < str.length(); i++) {
            char curChar = str.charAt(i);
            if (curChar >= 97 && curChar <= 122) {
                curChar += changesUpper;
            }
            uppCaseStr += curChar;
        }
        return uppCaseStr;
    }

    public static String camelcase(String str) {
        String camelCaseStr = "";
        for (int i = 0; i < str.length(); i++) {
            while (str.charAt(i) == ' ') {
                if (i == str.length() - 1) {
                        break;
                    }
                i++;
            }
            char curChar = str.charAt(i);
            if (i == 0) {
                camelCaseStr += lowercase("" + curChar); 
            } else if (str.charAt(i - 1) == ' ' && camelCaseStr != "") {
                camelCaseStr += uppercase("" + curChar);
            } else {
                camelCaseStr += lowercase("" + curChar);
            }
        }
        if (camelCaseStr.charAt(camelCaseStr.length() - 1) == ' ') {
            camelCaseStr = camelCaseStr.substring(0, camelCaseStr.length() - 1);
        }
        return camelCaseStr;
    }

    public static int compareTo(String str1, String str2) {
        String lowStr1 = lowercase(str1);
        String lowStr2 = lowercase(str2);
        String sShorter = "";
        if (lowStr1.length() < lowStr2.length()) {
            sShorter = lowStr1;
        } else {
            sShorter = lowStr2;
        }
        if (lowStr1.equals(lowStr2)) {
            return 0;
        }
        for (int i = 0; i < sShorter.length(); i++) {
            char curChar1 = lowStr1.charAt(i);
            char curChar2 = lowStr2.charAt(i);
            if (curChar2 > curChar1) {
                return -1;
            } else if (curChar2 < curChar1) {
                return 1;
            } else if (i == sShorter.length() - 1) {
                if (lowStr2 != sShorter) {
                    return -1;
                }
            }
        }
        return 1;
    }

    public static int stringToInt(String str) {
        int strToInt = 0;
        int toDec = -48;
        boolean isNeg = false;
        char firstChar = str.charAt(0);
        int i = 0;
        if (firstChar == 45) {
            isNeg = true;
            i++;
        } else if (firstChar == 43) {
            i++;
        }
        int curNum = 0;
        while (i < str.length()) {
            char curChar = str.charAt(i);
            if (isDigit(curChar)) {
                curNum = (curChar + toDec) * (int) Math.pow(10, str.length() - i - 1);
                strToInt += curNum;
            }
            i++;
        }
        if (isNeg) {
            strToInt *= -1;
        }
        return strToInt;
    }

    public static boolean isDigit(char chr) {
        return (chr >= 48 && chr <= 57);
    }

    public static String expand(String str) {
        String expandedDNA = "";
        int repNum = 1;
        for (int i = 0; i < str.length(); i++) {
            char curChar = str.charAt(i);
            int curIndex = i;
            while (isDigit(str.charAt(i + 1))) {
                i++;
                if (i == str.length() - 1) {
                    break;
                }
            } 
            repNum = stringToInt(str.substring(curIndex + 1, i + 1));
            for (int j = 0; j < repNum; j++) {
                expandedDNA += curChar;
            }
        }
        return expandedDNA;
    }
}


