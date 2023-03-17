
// A collection of functions for handling anagrams.
public class Anagram {
	public static void main(String args[]) {
		  // Tests preProcess function.
		  System.out.println(preProcess("   What? No way!    F f ")); 
	      // Tests the isAnagram function.
   		  System.out.println(isAnagram("silent","listen"));  // true
	      System.out.println(isAnagram("William Shakespeare","I am a weakish speller")); // true
	      System.out.println(isAnagram("Madam Curie","Radium came")); // true
	      System.out.println(isAnagram("Madam Curie","Radium cam")); // false
	      // Tests the randomAnagram function.
	      System.out.println(randomAnagram("silent")); // Prints a random anagram
	      // Performs a stress test of randomAnagram
	      boolean pass = true;
	      String str = "this is a stress test";
	      for (int i = 0; i < 1000; i++) {
	          pass = pass && isAnagram(str, randomAnagram(str));	
	      }     
	      System.out.println(pass);	// true if all tests are positive
	   }  

	   // Returns true if the two given strings are anagrams, false otherwise.
	   public static boolean isAnagram(String str1, String str2) {
	   	   boolean isAnag = false;
		   String preStr1 = preProcess(str1);
		   String preStr2 = preProcess(str2);

		   if (preStr1.length() != preStr2.length()) {
		   	  return false;
		   }

		   for (int i = 0; i < preStr1.length(); i++) {
		   	  char curChar1 = preStr1.charAt(i);
		   	  for (int j = 0; j < preStr2.length(); j++) {
		   	  	char curChar2 = preStr2.charAt(j);
		   	  	if (curChar1 == curChar2) {
		   	  		isAnag = true;
		   	  	}
		   	  }
		   }
		   return isAnag;
	   }
	   
	   // Returns a preprocessed version of the given string: all the letter characters
	   // are converted to lower-case, and all the other characters are deleted. For example, 
	   // the string "What? No way!" becomes "whatnoway"
	   public static String preProcess(String str) {
	   	   String preStr = ""; 
		   String lowStr = lowercase(str); 
		   for (int i = 0; i < lowStr.length(); i++) {
		       char curChar = lowStr.charAt(i);
		       if (curChar != ' ') {
		        	preStr += curChar;
		       }    
		   }
		   return preStr;
	   }

	    public static String lowercase(String str) {
	        int changesLower = 32; // from ascii table
	        String lowCaseStr = "";
	        for (int i = 0; i < str.length(); i++) {
	            char curChar = str.charAt(i);
	            if (curChar >= 65 && curChar <= 90) {
	                curChar += changesLower;
	                lowCaseStr += curChar;
	            } else if (curChar >= 97 && curChar <= 122) {
	            	lowCaseStr += curChar;
	            }
	        }
	        return lowCaseStr;
	    }	   
	   
	   // Returns a random anagram of the given string. The random anagram consists of the same
	   // letter characters as the given string, arranged in a random order.
	   public static String randomAnagram(String str) {
		   String rndAnag = "";
		   int strLen = str.length();
		   int rndIndex = 0;
		   char curChar = ' ';

		   for (int i = 0; i < strLen; i++) {
			   rndIndex = (int) (Math.random() * (strLen - i));
			   curChar = str.charAt(rndIndex);
		   	   rndAnag += curChar;
		   	   str = str.substring(0, rndIndex ) + str.substring(rndIndex + 1);
		   }
		   return rndAnag;
	   }
}
