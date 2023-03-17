import java.awt.Color;

/**
 * Demonstrates the scaling function of Instush.java.
 * The program recieves two command-line arguments: the name of the PPM file
 * (a string) representing the image that should be scaled, and two integers
 * that specify the width and the height of the scaled image. For example:
 * java Editor2 ironman.ppm 100 900
 */
public class Editor2 {

	public static void main(String[] args) {
		// Write your code here
		String file = args[0];
		int width = Integer.parseInt(args[1]);
		int height = Integer.parseInt(args[2]);

		Instush.show(Instush.scaled(Instush.read(file), width, height));
	}
}
