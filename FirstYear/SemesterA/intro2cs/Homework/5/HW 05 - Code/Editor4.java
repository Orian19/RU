import java.awt.Color;

/**
 * Demonstrates morphing an image into its greyscaled version
 * The program recieves two command-line arguments: the name of a PPM file
 * that represents the source image (a string) 
 * and the number of morphing steps (an int). 
 * For example:
 * java Editor4 thor.ppm 300
 * This action transforms the colored image of Thor into a black and white image of Thor
 */
public class Editor4 {

	public static void main (String[] args) {
		// Write your code here		
		String file = args[0];
		int steps = Integer.parseInt(args[1]);

		Instush.morph(Instush.read(file), Instush.greyscaled(Instush.read(file)), steps);
	}
}
