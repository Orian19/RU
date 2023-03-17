// Represents some code operations as functions.
public class CodeOps {
	public static void main(String args[]) {
	    // printHistogram(frequencies("As Easy as A, B, C"));
	    // System.out.println(encode("defend the northern wall", 4)); // “hijirh xli rsvxlivr aepp”
	    System.out.println(decode("dibpt jtou b qju. dibpt jt b mbeefs ", 20)); // “defend the northern wall”
	}  

	public static int[] frequencies(String str) {
		int [] res = new int [26]; // Alphabet size. 
		int curLetter = 97;
	    String lowStr = lowercase(str);
	    for (int i = 0; i < res.length; i++) {
	    	for (int j = 0; j < str.length(); j++) {
	    		if (lowStr.charAt(j) == curLetter) {
	    			res[i]++;
	    		}
	    	}
	    	curLetter++;
	    }
	 	return res;
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

	public static void printHistogram(int[] letters) {
		for (int i = 0; i < letters.length; i++) {
			for (int j = 0; j < letters[i]; j++) {
				System.out.print("*");
			}
			System.out.println();
		}
	}

	// assumption: the key is bigger or equal to -26
	public static String encode(String str, int key) {
		String encodedStr = "";
		char newCurChar = ' ';
		for (int i = 0; i < str.length(); i++) {
			if (str.charAt(i) == 32) {
				encodedStr += str.charAt(i);
			} else {
				if ((str.charAt(i) + key) > 122) {
					newCurChar = (char) (str.charAt(i) + key - 26);
				} else if ((str.charAt(i) + key) < 97) {
					newCurChar = (char) (str.charAt(i) + key + 26);
				} else {
					newCurChar = (char) (str.charAt(i) + key);
				}
				encodedStr += newCurChar;
			}
		}
		return encodedStr;
	}

	public static String decode(String str, int key) {
		String decodedStr = "";
		char newCurChar = ' ';
		for (int i = 0; i < str.length(); i++) {
			char curChar = str.charAt(i);
			if (isLetter(curChar)) {
				if ((str.charAt(i) - key) > 122) {
					newCurChar = (char) (str.charAt(i) - key - 26);
				} else if ((str.charAt(i) - key) < 97) {
					newCurChar = (char) (str.charAt(i) - key + 26);
				} else {
					newCurChar = (char) (str.charAt(i) - key);
				}
				decodedStr += newCurChar;
			} else {
				decodedStr += str.charAt(i);
			}
		}
		return decodedStr;
	}

	public static boolean isLetter(char ch) {
		if (ch > 100 && ch < 133) {
			return true;
		} else if (ch > 60 && ch < 173) {
			return true;
		}
		return false;
	}

	public static String decode(String str) {
		// Replace the return statement with your code.
	    return "";
	}
}
