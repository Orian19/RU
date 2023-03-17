import java.util.LinkedList;
import java.awt.Color;

public class ShopTester {
    public static void main(String[] args) {
        testConstructor();
        testAddItem();
        testAddItemWhenArrayIsFull();
        testAddNullItem();
        testGetInvalidJeans();
        testAddInvalidItem();
        testGetInvalidShirt();
        testGetJeans();
        testGetShirt();
        testRemoveItem();
        testRemoveNullItem();
        testDiscountJeans();
        testDiscountPoloShirts();
        testSortShirts();
        testSortJeans();
        //playground(); //Your playground
    }

    public static void testConstructor() {
        Shop shop = new Shop(10);
        boolean testPassed = (shop.getshirtCount() == 0) && (shop.getjeansCount() == 0) && 
                            (shop.getShirts().length == 10) && (shop.getJeans().length == 10);
        if(!testPassed)
            System.out.println("testConstructor: Failed");
        else
            System.out.println("testConstructor: Passed");
    }

    public static void testAddItem() {
        Shop shop = new Shop(10);
        LinkedList<Color> colorsWhite = new LinkedList<Color>();
        colorsWhite.add(Color.white);
        LinkedList<Color> colorsBlack = new LinkedList<Color>();
        colorsBlack.add(Color.black);
        Shirt whiteShirt = new Shirt("zara", 3, "V-Shirt", 70, colorsWhite);
        Shirt blackShirt = new Shirt("zara", 4, "V-Shirt", 70, colorsBlack);
        Shirt blackShirt2 = new Shirt("zara", 4, "V-Shirt", 70, colorsBlack);
        Jeans jeans = new Jeans("armani", 5, "Slim", 200);
        shop.addItem(whiteShirt);
        shop.addItem(blackShirt);
        shop.addItem(blackShirt2);
        shop.addItem(jeans);
        boolean testPassed = (shop.getshirtCount() == 2) && (shop.getjeansCount() == 1);
        if(!testPassed)
            System.out.println("testAddItem: Failed");
        else
            System.out.println("testAddItem: Passed");
    }

    public static void testAddItemWhenArrayIsFull(){
        int n = 10;
        Shop shop = new Shop(n);
        LinkedList<Color> colorsWhite = new LinkedList<Color>();
        colorsWhite.add(Color.white);
        for (int i = 1; i < n + 1; i++) {
            shop.addItem(new Shirt("Zara", i, "T-Shirt", 10 * i, colorsWhite));
            shop.addItem(new Jeans("Armani", 100 + i, "Slim", 200 * i));
        }

        boolean testPassed = shop.getProfit() == 11550.0;
        shop.addItem(new Shirt("Zara", 20, "T-Shirt", 60, colorsWhite));
        testPassed = testPassed && (shop.getProfit() == 11600.0);
        shop.addItem(new Jeans("Armani", 30, "Slim", 1200));
        testPassed = testPassed && (shop.getProfit() == 12600.0);
        if(!testPassed) {
            System.out.println("testAddItemWhenArrayIsFull: Failed");
        }
        else {
            System.out.println("testAddItemWhenArrayIsFull: Passed");
        }
    }

    public static void testAddNullItem() {
        try {
            Shop shop = new Shop(10);
            shop.addItem(null);
            System.out.println("testAddNullItem: Failed");
        } catch (Exception e) {
            System.out.println("testAddNullItem: Passed");
        }

    }

    public static void testAddInvalidItem() {
        try {
            Shop shop = new Shop(10);
            Item item = new Item(1, "testItem", 1.0);
            shop.addItem(item);
            System.out.println("testAddInvalidItem: Failed");
        } catch (Exception e) {
            System.out.println("testAddInvalidItem: Passed");
        }

    }

    public static void testRemoveItem() {
        Shop shop = new Shop(10);
        LinkedList<Color> colorsWhite = new LinkedList<Color>();
        colorsWhite.add(Color.white);
        Shirt whiteShirt = new Shirt("zara", 3, "V-Shirt", 70, colorsWhite);
        Jeans jeans = new Jeans("armani", 5, "Slim", 200);
        shop.addItem(whiteShirt);
        shop.addItem(jeans);
        shop.removeItem(whiteShirt);
        shop.removeItem(jeans);
        boolean testPassed = (shop.getshirtCount() == 0) && (shop.getjeansCount() == 0);
        if(!testPassed) {
            System.out.println("testRemoveItem: Failed");
        }
        else {
            System.out.println("testRemoveItem: Passed");
        }
    }

    public static void testRemoveNullItem() {
        try {
            Shop shop = new Shop(10);
            shop.removeItem(null);
            System.out.println("testRemoveItem: Failed");
        } catch (Exception e) {
            System.out.println("testRemoveItem: Passed");
        }
    }

    public static void testGetShirt() {
        Shop shop = new Shop(10);
        LinkedList<Color> colorsWhite = new LinkedList<Color>();
        colorsWhite.add(Color.white);
        Shirt whiteShirt = new Shirt("zara", 3, "V-Shirt", 70, colorsWhite);
        shop.addItem(whiteShirt);
        boolean testPassed = shop.getShirt(3) == whiteShirt;
        if(!testPassed)
            System.out.println("testGetShirt: Failed");
        else
            System.out.println("testGetShirt: Passed");
    }

    public static void testGetInvalidShirt() {
        try {
            Shop shop = new Shop(10);
            shop.getShirt(100);
            System.out.println("testGetInvalidShirt: Failed");
        } catch (Exception IllegalArgumentException) {
            System.out.println("testGetInvalidShirt: Passed");
        }

    }

    public static void testGetJeans() {
        Shop shop = new Shop(10);
        Jeans jeans = new Jeans("armani", 5, "Slim", 200);
        shop.addItem(jeans);
        boolean testPassed = shop.getJeans(5) == jeans;
        if(!testPassed)
            System.out.println("testGetJeans: Failed");
        else
            System.out.println("testGetJeans: Passed");
    }

    public static void testGetInvalidJeans() {
        try {
            Shop shop = new Shop(10);
            shop.getJeans(100);
            System.out.println("testGetInvalidJeans: Failed");
        } catch (Exception IllegalArgumentException) {
            System.out.println("testGetInvalidJeans: Passed");
        }
    }

    public static void testDiscountJeans(){
        Shop shop = new Shop(10);
        Jeans j1 = new Jeans("levy's", 2, "Skinny", 100);
        Jeans j2 = new Jeans("armani", 5, "Slim", 200);
        Jeans j3 = new Jeans("armani", 6, "Slim", 200);
        shop.addItem(j1);
        shop.addItem(j2);
        shop.addItem(j3);
        boolean testPassed = shop.getProfit() == 500;
        shop.discountJeans(10);
        testPassed = testPassed && (shop.getProfit() == 450);
        if(!testPassed) {
        System.out.println("testDiscountJeans: Failed");
        }
        else {
            System.out.println("testDiscountJeans: Passed");
        }
    }

    public static void testDiscountPoloShirts(){
        Shop shop = new Shop(10);
        LinkedList<Color> colors = new LinkedList<Color>();
        colors.add(Color.GREEN);
        PoloShirt ps1 = new PoloShirt("zara", 2, "PoloShirt", 100, colors, true);
        PoloShirt ps2 = new PoloShirt("zara", 3, "PoloShirt", 100, colors, false);
        PoloShirt ps3 = new PoloShirt("polo", 4, "PoloShirt", 100, colors, true);
        shop.addItem(ps1);
        shop.addItem(ps2);
        shop.addItem(ps3);
        boolean testPassed = shop.getProfit() == 300;
        shop.discountPoloShirts(10);
        testPassed = testPassed && (shop.getProfit() == 270);
        Shirt shirt = new Shirt("zara", 3, "V-Shirt", 70, colors);
        shop.addItem(shirt);
        shop.discountPoloShirts(10);
        testPassed = testPassed && (shop.getProfit() == 243);
        if(!testPassed) {
        System.out.println("testDiscountPoloShirts: Failed");
        }
        else {
            System.out.println("testDiscountPoloShirts: Passed");
        }
    }

    public static void testSortShirts(){
        Shop shop = new Shop(4);
        LinkedList<Color> colorsWhite = new LinkedList<Color>();
        colorsWhite.add(Color.white);
        Shirt whiteShirt = new Shirt("zara", 10, "V-Shirt", 70, colorsWhite);
        PoloShirt ps1 = new PoloShirt("zara", 2, "PoloShirt", 60, colorsWhite, true);
        PoloShirt ps2 = new PoloShirt("zara", 3, "PoloShirt", 50, colorsWhite, false);
        PoloShirt ps3 = new PoloShirt("polo", 4, "PoloShirt", 100, colorsWhite, true);
        shop.addItem(ps1);
        shop.addItem(ps2);
        shop.addItem(ps3);
        shop.addItem(whiteShirt);
        shop.sortShirts();
        boolean testPassed = shop.getShirts()[0].equals(ps2);
        if(!testPassed) {
        System.out.println("testSortShirts: Failed");
        }
        else {
            System.out.println("testSortShirts: Passed");
        }
    }

    public static void testSortJeans(){
        Shop shop = new Shop(3);
        Jeans j1 = new Jeans("levy's", 2, "Skinny", 300);
        Jeans j2 = new Jeans("armani", 5, "Slim", 200);
        Jeans j3 = new Jeans("armani", 6, "Slim", 100);
        shop.addItem(j1);
        shop.addItem(j2);
        shop.addItem(j3);
        shop.sortJeans();
        boolean testPassed = shop.getJeans()[0].equals(j3);
        if(!testPassed) {
        System.out.println("testSortJeans: Failed");
        }
        else {
            System.out.println("testSortJeans: Passed");
        }
    }

    public static void playground(){
        Shop shop = new Shop(5);
        LinkedList<Color> colors = new LinkedList<Color>();
        colors.add(Color.white);
        colors.add(Color.blue);
        Shirt s1 = new Shirt("zara", 1, "T-Shirt", 20, colors);
        PoloShirt s2 = new PoloShirt("zara", 2, "PoloShirt", 80, colors, true);
        LinkedList<Color> colors2 = new LinkedList<Color>();
        colors2.add(Color.orange);
        Shirt s3 = new Shirt("zara", 3, "V-Shirt", 70, colors2);
        Shirt s4 = new Shirt("gucci", 7, "T-Shirt", 100, colors);
        PoloShirt s5 = new PoloShirt("fox kids", 8, "PoloShirt", 80, colors, true);
        Shirt s6 = new Shirt("loui biton", 9, "T-Shirt", 60, colors);
        Jeans j1 = new Jeans("levy's", 2, "Skinny", 100);
        Jeans j2 = new Jeans("armani", 5, "Slim", 200);
        shop.addItem(s1);
        shop.addItem(s2);
        shop.addItem(s3);
        shop.addItem(j1);
        shop.addItem(j2);

        shop.printDetails();
        /*  class Shirt{name='T-Shirt', price=20.0}, brand='zara', colors=java.awt.Color[r=255,g=255,b=255] java.awt.Color[r=0,g=0,b=255] 
            class PoloShirt{name='PoloShirt', price=80.0}, brand='zara', colors=java.awt.Color[r=255,g=255,b=255] java.awt.Color[r=0,g=0,b=255] , Sleeves='Long'
            class Shirt{name='V-Shirt', price=70.0}, brand='zara', colors=java.awt.Color[r=255,g=200,b=0] 
            class Jeans{name='Slim', price=200.0}, brand='armani'
         */
        System.out.println(shop.getProfit()); // 370.0
        System.out.println(shop.getshirtCount()); // 3
        shop.addItem(s3);
        shop.discountJeans(20);
        System.out.println(shop.getProfit()); // 330.0
        shop.discountPoloShirts(30);
        shop.printDetails();
        /*  class Shirt{name='T-Shirt', price=20.0}, brand='zara', colors=java.awt.Color[r=255,g=255,b=255] java.awt.Color[r=0,g=0,b=255] 
            class PoloShirt{name='PoloShirt', price=56.0}, brand='zara', colors=java.awt.Color[r=255,g=255,b=255] java.awt.Color[r=0,g=0,b=255] , Sleeves='Long'
            class Shirt{name='V-Shirt', price=70.0}, brand='zara', colors=java.awt.Color[r=255,g=200,b=0] 
            class Jeans{name='Slim', price=160.0}, brand='armani'
         */
        System.out.println(shop.getProfit()); // 306.0
        System.out.println(shop.getshirtCount()); // 3
        System.out.println(shop.getjeansCount()); // 1
        shop.removeItem(j1);
        System.out.println(shop.getjeansCount()); // 1
        shop.removeItem(j2);
        System.out.println(shop.getjeansCount()); // 0
        shop.addItem(s4);
        shop.addItem(s5);
        System.out.println(shop.getProfit()); // 326.0
        shop.addItem(s6);
        System.out.println(shop.getProfit()); // 366.0
    }
}