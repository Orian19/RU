import java.awt.Color;

/**
 * A library of image processing functions.
 */
public class Instush {
	
	public static void main(String[] args) {
	    // println(read("tinypic.ppm"));			
	    // System.out.println();
		// println(flippedHorizontally(read("tinypic.ppm")));			
		// System.out.println();
		// println(flippedVertically(read("tinypic.ppm")));			
		// System.out.println();
		// //println(greyscaled(read("thor.ppm")));			
		// show(read("xmen.ppm"));
		// //show(greyscaled(read("thor.ppm")));
		// System.out.println();
		// println(scaled(read("tinypic.ppm"), 3, 5));	
		// System.out.println();
		// show((read("ironman.ppm")));
		// show(scaled(read("ironman.ppm"), 800, 300));	
		// morph(read("ironman.ppm"), read("xmen.ppm"), 100);		
	}

	/**
	 * Returns an image created from a given PPM file.
	 * SIDE EFFECT: Sets standard input to the given file.
	 * @return the image, as a 2D array of Color values
	 */
	public static Color[][] read(String filename) {
		StdIn.setInput(filename);
		// Reads the PPM file header (ignoring some items)
		StdIn.readString();
		int numCols = StdIn.readInt();
		int numRows = StdIn.readInt();
		StdIn.readInt();
		// Creates the image
		Color[][] image = new Color[numRows][numCols];
		// Reads the RGB values from the file, into the image. 
		// For each pixel (i,j), reads 3 values from the file,
		// creates from the 3 colors a new Color object, and 
		// makes pixel (i,j) refer to that object.		
		// Replace the following statement with your code
		int colorSize = 3;
	
		for (int i = 0; i < numRows; i++) {
			for (int j = 0; j < numCols; j++) {
				int [] curColor = new int[3];
				for (int k = 0; k < colorSize; k++) {
					curColor[k] = StdIn.readInt(); 
				}
				image[i][j] = new Color(curColor[0], curColor[1], curColor[2]);
			}
		}
		return image;
	}

	/**
	 * Prints the pixels of a given image.
	 * Each pixel is printed as a triplet of (r,g,b) values.
	 * For debugging purposes.
	 * @param image - the image to be printed
	 */
	public static void println(Color[][] image) {
	    // Write your code here
		for (int i = 0; i < image.length; i++) {
			for (int j = 0; j < image[0].length; j++) {
				System.out.print("(");
				System.out.printf("%3s,", image[i][j].getRed());    // Prints the red component
				System.out.printf("%4s,", image[i][j].getGreen());  // Prints the green component
				System.out.printf("%4s", image[i][j].getBlue());   // Prints the blue component
				System.out.print(") ");
			}
			System.out.println();
		}
	}
	
	/**
	 * Returns an image which is the horizontally flipped version of the given image. 
	 * @param image - the image to flip
	 * @return the horizontally flipped image
	 */
	public static Color[][] flippedHorizontally(Color[][] image) {
		// Replace the following statement with your code
		Color[][] flippedImage = new Color[image.length][image[0].length];
		for (int i = 0; i < flippedImage.length; i++) {
			for (int j = 0; j < flippedImage[0].length; j++) {
				flippedImage[i][j] = image[i][image.length - 1 - j];
			}
		}
		return flippedImage;
	}
	
	/**
	 * Returns an image which is the vertically flipped version of the given image. 
	 * @param image - the image to flip
	 * @return the vertically flipped image
	 */
	public static Color[][] flippedVertically(Color[][] image){
		// Replace the following statement with your code
		Color[][] flippedImage = new Color[image.length][image[0].length];
		for (int i = 0; i < flippedImage[0].length; i++) {
			for (int j = 0; j < flippedImage.length; j++) {
				flippedImage[j][i] = image[image.length - 1 - j][i];
			}
		}
		return flippedImage;
	}
	
	/**
	 * Returns the average of the RGB values of all the pixels in a given image.
	 * @param image - the image
	 * @return the average of all the RGB values of the image
	 */
	public static double average(Color[][] image) {
		// Replace the following statement with your code
		return 0.0;
	}

	/**
	 * Returns the luminance value of a given pixel. Luminance is a weighted average
	 * of the RGB values of the pixel, given by 0.299 * r + 0.587 * g + 0.114 * b.
	 * Used as a shade of grey, as part of the greyscaling process.
	 * @param pixel - the pixel
	 * @return the greyscale value of the pixel, as a Color object
	 *         (r = g = b = the greyscale value)
	 */
	public static Color luminance(Color pixel) {
		// Replace the following statement with your code
		int lum = (int) (pixel.getRed() * 0.299 + pixel.getGreen() * 0.587 + pixel.getBlue() * 0.114);
		Color greyScaleLum = new Color(lum, lum, lum);
		return greyScaleLum;
	}
	
	/**
	 * Returns an image which is the greyscaled version of the given image.
	 * @param image - the image
	 * @return rhe greyscaled version of the image
	 */
	public static Color[][] greyscaled(Color[][] image) {
		// Replace the following statement with your code
		Color[][] greyScaledImage = new Color[image.length][image[0].length];
		for (int i = 0; i < image.length; i++) {
			for (int j = 0; j < image[0].length; j++) {
				greyScaledImage[i][j] = luminance(image[i][j]);
			}
		}
		return greyScaledImage;
	}	
	
	/**
	 * Returns an umage which is the scaled version of the given image. 
	 * The image is scaled (resized) to be of the given width and height.
	 * @param image - the image
	 * @param width - the width of the scaled image
	 * @param height - the height of the scaled image
	 * @return - the scaled image
	 */
	public static Color[][] scaled(Color[][] image, int width, int height) {
		// Replace the following statement with your code
		Color[][] scaledImage = new Color[height][width];
		for (int i = 0; i < scaledImage.length; i++) {
			for (int j = 0; j < scaledImage[0].length; j++) {
				double hScale = (double) image.length / height;
				double wScale = (double) image[0].length / width;

				scaledImage[i][j] = image[(int) (i * hScale)][(int) (j * wScale)];
			}
		}
		return scaledImage;
	}
	
	/**
	 * Returns a blended color which is the linear combination of two colors.
	 * Each r, g, b, value v is calculated using v = (1 - alpha) * v1 + alpha * v2.
	 * 
	 * @param pixel1 - the first color
	 * @param pixel2 - the second color
	 * @param alpha - the linear combination parameter
	 * @return the blended color
	 */
	public static Color blend(Color c1, Color c2, double alpha) {
		// Replace the following statement with your code
		int rBlended = (int) ((1 - alpha) * c2.getRed() + alpha * c1.getRed());
		int gBlended = (int) ((1 - alpha) * c2.getGreen() + alpha * c1.getGreen());
		int bBlended = (int) ((1 - alpha) * c2.getBlue() + alpha * c1.getBlue());

		Color blended = new Color(rBlended, gBlended, bBlended);

		return blended;
	}
	
	/**
	 * Returns an image which is the blending of the two given images.
	 * The blending is the linear combination of (1 - alpha) parts the
	 * first image and (alpha) parts the second image.
	 * The two images must have the same dimensions. 
	 * @param image1 - the first image
	 * @param image2 - the second image
	 * @param alpha - the linear combination parameter
	 * @return - the blended image
	 */
	public static Color[][] blend(Color[][] image1, Color[][] image2, double alpha) {
		// Replace the following statement with your code
		Color[][] blendedImage = new Color[image1.length][image1[0].length];
		for (int i = 0; i < image1.length; i++) {
			for (int j = 0; j < image1[0].length; j++) {
				blendedImage[i][j] = blend(image1[i][j], image2[i][j], alpha);
			}
		}
		return blendedImage;
	}
	
	/**
	 * Morphs the source image into the target image, gradually, in n steps.
	 * Animates the morphing process by displaying the morphed image in each step.
	 * The target image is an image which is scaled to be a version of the target
	 * image, scaled to have the width and height of the source image.
	 * @param source - source image
	 * @param target - target image
	 * @param n - number of morphing steps
	 */
	public static void morph(Color[][] source, Color[][] target, int n) {
		// Write your code here
		if (source.length != target.length || source[0].length != target[0]. length) {
			target = scaled(target, source[0].length, source.length);
		}

		for (int g = 0; g < n; g++) {
			show(blend(source, target, ((double) n - g) / n));
		}
	}
	
     /**
	 * Renders (displays) an image on the screen, using StdDraw. 
	 * 
	 * @param image - the image to show
	 */
	public static void show(Color[][] image) {
		StdDraw.setCanvasSize(image[0].length, image.length);
		int width = image[0].length;
		int height = image.length;
		StdDraw.setXscale(0, width);
		StdDraw.setYscale(0, height);
		StdDraw.show(25); 
		for (int i = 0; i < height; i++) {
			for (int j = 0; j < width; j++) {
				// Sets the pen color to the color of the pixel
				StdDraw.setPenColor( image[i][j].getRed(),
					                 image[i][j].getGreen(),
					                 image[i][j].getBlue() );
				// Draws the pixel as a tiny filled square of size 1
				StdDraw.filledSquare(j + 0.5, height - i - 0.5, 0.5);
			}
		}
		StdDraw.show();
	}
}

