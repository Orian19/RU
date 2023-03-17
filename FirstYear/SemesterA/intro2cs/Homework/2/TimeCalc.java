
/** Performs time calculations. 
 */
public class TimeCalc {
	public static void main(String[] args) {
		String validInp = "0123456789:-";
		boolean inputFlag = true;
		int hours = 0;
		int minutes = 0;
		int minNum = 0;
		// checking user inserted full time and a minutes value
		if (args.length == 2) {
			if (args[0].length() == 5) {
				String colonPos = "" + args[0].charAt(2);
				if (!colonPos.equals(":")) {
					inputFlag = false;
				}
				for (int i = 0; i < 5; i++) {
					char curChar = args[0].charAt(i);
					if (validInp.indexOf(curChar) == -1) {
						inputFlag = false;
					}
				}
				for (int i = 0; i < args[1].length(); i++) {
					char curChar = args[1].charAt(i);
					if (validInp.indexOf(curChar) == -1) {
						inputFlag = false;
					}
				}	
			} else {
				inputFlag = false;
			}
		} else {
			inputFlag = false;
		}

		if (inputFlag) {
			hours = Integer.parseInt("" + args[0].charAt(0) + args[0].charAt(1));
			minutes = Integer.parseInt("" + args[0].charAt(3) + args[0].charAt(4));
			minNum = Integer.parseInt(args[1]);

			if (minNum <= -1440 || hours < 0 || hours > 24 || minutes < 0 || minutes > 59) {
				inputFlag = false;
			}
		}
		// if the flag is still true than all inputs are valid :)
		if (inputFlag) {
			// calculating the hour after addition of minNum
	        int hourCalc = 0, countHour = 0, addMin = 0;

	        if (minNum > 0) {
	        	addMin = minNum % 60;
		        while (minNum >= 60) {
		        	countHour++;
		        	minNum -= 60;
		        }
				hours += countHour;
		    	minutes += addMin;
		    	if (minutes >= 60) {
		    		hours += 1;
		    		minutes -= 60;
		   		}
		   		if (hours >= 24) {
					hours %= 24;
				}
		    } else {
		    	minNum *= -1;
		    	addMin = minNum % 60;
		        while (minNum >= 60) {
		        	countHour++;
		        	minNum -= 60;
		        }
				hours -= countHour;
		    	minutes -= addMin;
		    	if (minutes <= 0) {
		    		hours -= 1;
		    		minutes += 60;
		   		}
		   		if (minutes <= 0) {
		   			hours -= 1;
		   			minutes += 60;
		   		}
		   		while (hours <= 0){
		   			hours += 24;
		   		}
		    }
			// format time
			String fMinutes = "", ampm = "";
	        // 0 padding for minutes
	        if (minutes < 10) {
	        	fMinutes = "0" + minutes;
	        }
	        else {
	        	fMinutes = "" + minutes;
	        }
	        // converting hour if needed + assigning to correct AM/PM name
	        if (hours == 12) {
	        	ampm = "PM";
	        }
	        else if (hours > 12) {
	        	hours -= 12;
	        	ampm = "PM";
	        }
	        else {
	        	ampm = "AM";
	        } 
			// print time in AM/PM format
			System.out.println(hours + ":" + fMinutes + " "+ ampm);		
		} else {
			System.out.println("Invalid input");
		}
	}
}
