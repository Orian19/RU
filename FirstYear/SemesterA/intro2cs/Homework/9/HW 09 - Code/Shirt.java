import java.util.LinkedList;
import java.awt.Color;
import java.util.Iterator;

public class Shirt extends Item {
    protected String brand;
    protected LinkedList<Color> colors;

    public Shirt(String brand, int barcode, String name, double price, LinkedList<Color> colors) {
        super(barcode, name, price);
        this.brand = brand;
        this.colors = colors;
    }

    public String getBrand() {
        return this.brand;
    }

    public LinkedList<Color> getColors() {
        return this.colors;
    }

    public void addColor(Color color) {
        // Write your code here
        if (color != null) {
            if (!colors.contains(color)) {
                colors.add(color);
            }
        }
    }

    public void removeColor(Color color) {
        // Write your code here
        colors.remove(color);
    }

    public boolean equals(Object o) {
        // Write your code here
        if (o == null) {
            return false;
        }
        if (this == o) {
            return true;
        }

        if (getClass() != o.getClass()) {
            return  false;
        }

        Shirt other = (Shirt) o;
        if (!super.equals(o)) {
            return false;
        }
        if (!this.colors.equals(other.colors)) {
            return false;
        }
        if (!this.brand.equals(other.brand)) {
            return false;
        }
        return true;
    }

    public String toString() {
        // Write your code here
        String str =  super.toString() + ", brand='" + this.brand + "'" + ", colors="; //+ this.colors;
        Iterator itr = colors.iterator();
        Color color = null;
        while (itr.hasNext()) {
            color = (Color) itr.next();
            str += color.toString() + " ";
        }
        return str;
    }
}
