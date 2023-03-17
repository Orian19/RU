import java.util.LinkedList;

import java.awt.Color;

public class ItemsTester {
    // Note this is a tester for both Shirt / PoloShirt / Jeans
    public static void main(String[] args) {
        testShirt();
        testShirtColor();
        testDiscount();
        testInvalidDiscount();
        testJeans();
    }

    public static void testShirt() {
        System.out.println("Creating shirts...");
        LinkedList<Color> colors = new LinkedList<Color>();
        colors.add(Color.white);
        colors.add(Color.blue);
        Shirt s1 = new Shirt("zara", 1, "T-Shirt", 20, colors);
        LinkedList<Color> colors2 = new LinkedList<Color>();
        colors2.add(Color.orange);
        Shirt s2 = new Shirt("zara", 3, "V-Shirt", 70, colors2);
        Shirt s3 = new Shirt("gucci", 7, "T-Shirt", 100, colors);
        Shirt s4 = new Shirt("loui biton", 9, "T-Shirt", 60, colors);
        Shirt s5 = new Shirt("zara", 1, "T-Shirt", 20, colors);
        PoloShirt ps1 = new PoloShirt("zara", 2, "PoloShirt", 80, colors, true);
        PoloShirt ps2 = new PoloShirt("fox kids", 8, "PoloShirt", 80, colors, true);

        // All should be true
        System.out.println("\nEquals:");
        System.out.println(s1.equals(s5));
        System.out.println(!ps1.equals(ps2));
        System.out.println(s3.equals(s3));


        System.out.println("\ntoString:");
        String str1 = "class Shirt{name='T-Shirt', price=60.0}, brand='loui biton', colors=java.awt.Color[r=255,g=255,b=255] java.awt.Color[r=0,g=0,b=255] ";
        String str2 = "class Shirt{name='V-Shirt', price=70.0}, brand='zara', colors=java.awt.Color[r=255,g=200,b=0] ";
        String strPoloShirt1 = "class PoloShirt{name='PoloShirt', price=80.0}, brand='zara', colors=java.awt.Color[r=255,g=255,b=255] java.awt.Color[r=0,g=0,b=255] , Sleeves='Long'";
        System.out.println(s4.toString().equals(str1));
        System.out.println(s2.toString().equals(str2));
        System.out.println(ps1.toString().equals(strPoloShirt1));
    }

    public static void testShirtColor() {
        System.out.println("\naddColor & removeColor:");
        LinkedList<Color> colors1 = new LinkedList<Color>();
        colors1.add(Color.white);
        colors1.add(Color.blue);
        LinkedList<Color> colors2 = new LinkedList<Color>();
        colors2.add(Color.white);
        colors2.add(Color.blue);
        Shirt s6 = new Shirt("Zara", 30, "T-Shirt", 20, colors1);
        s6.addColor(Color.red);
        Shirt s7 = new Shirt("Zara", 30, "T-Shirt", 20, colors2);
        LinkedList<Color> colorsWhiteBlue = new LinkedList<Color>();
        colorsWhiteBlue.add(Color.white);
        colorsWhiteBlue.add(Color.blue);
        Shirt s8 = new Shirt("Zara", 30, "T-Shirt", 20, colorsWhiteBlue);
        // All should be true
        System.out.println(!s6.equals(s7));
        s6.removeColor(Color.red);
        System.out.println(s6.equals(s8));
    }

    public static void testDiscount(){
        System.out.println("\ntestDiscount:");
        LinkedList<Color> colors = new LinkedList<Color>();
        colors.add(Color.white);
        colors.add(Color.blue);
        Shirt shirt = new Shirt("Gucci", 7, "T-Shirt", 100, colors);
        Jeans jeans = new Jeans("Armani", 5, "Slim", 200);

        shirt.discount(10);
        jeans.discount(20);
        // All should be true
        System.out.println(shirt.getPrice() == 90.0);
        System.out.println(jeans.getPrice() == 160.0);
    }

    public static void testInvalidDiscount(){
        System.out.println("\ntestInvalidDiscount:");
        LinkedList<Color> colors = new LinkedList<Color>();
        colors.add(Color.white);
        colors.add(Color.blue);
        try {
            Shirt shirt = new Shirt("Gucci", 7, "T-Shirt", 100, colors);
            shirt.discount(101);
            System.out.println("false");
        } catch (Exception IllegalArgumentException) {
            System.out.println("true");
        }
    }

    public static void testJeans() {
        System.out.println("\nCreating jeans...");
        Jeans j1 = new Jeans("levy's", 2, "Skinny", 100);
        Jeans j2 = new Jeans("armani", 5, "Slim", 200);

        // All should be true
        System.out.println("\nEquals:");
    
        System.out.println(!j1.equals(j2));
        System.out.println(j1.equals(j1));

        System.out.println("\ntoString:");
        String strJeans1 = "class Jeans{name='Skinny', price=100.0}, brand='levy's'";
        String strJeans2 = "class Jeans{name='Slim', price=200.0}, brand='armani'";
        System.out.println(j1.toString().equals(strJeans1));
        System.out.println(j2.toString().equals(strJeans2));
    }
}