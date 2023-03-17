import java.awt.Color;

/**
 * Demonstrates three Instush.java services: flipping an image horizontally, flipping an image 
 * vertically, and greyscaling an image.
 * 
 * The program recieves two command-line arguments: the name of the PPM file that represents 
 * the source image (a string), and one of the strings "fh", "fv", or "gs" (a string). The program
 * creates and displays a new image which is either the horizontally flipped version of the source
 * image ("fh"), or the vertically flipped version of the source image ("fv"), or the greyscaled 
 * version of the source image ("gs"). For example: 
 * java Editor1 thor.ppm gs
 */
public class Editor1 {

	public static void main (String[] args){
		// Write your code here	
		String file = args[0];
		String process = args[1];
		
		if ("gs".equals(process)) {
			Instush.show(Instush.greyscaled(Instush.read(file)));
		} 

		if ("fh".equals(process)) {
			Instush.show(Instush.flippedHorizontally(Instush.read(file)));
		} 

		if ("fv".equals(process)) {
			Instush.show(Instush.flippedVertically(Instush.read(file)));
		} 			
	}
}
