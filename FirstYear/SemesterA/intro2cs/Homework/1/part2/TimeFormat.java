
// Represents the hh:mm time format using an AM/PM format. 
public class TimeFormat {
	public static void main(String[] args) {
		// In Java, the command-line arguments args[0], args[1], ... are represented
		// each by a string. In this program, the single "hh:mm" input is represented
		// by the single command-line string argument args[0]. 
		//   
		// Concatenates the empty string "" with the left hour-digit, concatenates
		// the resulting string with the right hour-digit, and casts the resulting
		// string as an int.
		int hours = Integer.parseInt("" + args[0].charAt(0) + args[0].charAt(1));
		// Does the same with the minute digits.
		int minutes = Integer.parseInt("" + args[0].charAt(3) + args[0].charAt(4));
        
        System.out.println(args[0].length());

        // format time
		String ampm = "";
		String fMinutes = "";

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
	}
}
