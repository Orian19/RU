import java.awt.Color;
import java.util.LinkedList;

public class PoloShirt extends Shirt {
    private boolean isLongSleeves;

    public PoloShirt(String brand, int barcode, String name, double price, LinkedList<Color> colors, boolean isLongSleeves) {
        super(brand, barcode, name, price, colors);
        this.isLongSleeves = isLongSleeves;
    }

    public boolean getIsLongSleeves() {
        return this.isLongSleeves;
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

        PoloShirt other = (PoloShirt) o;
        if (!super.equals(o)) {
            return false;
        }
        if (this.isLongSleeves != other.isLongSleeves) {
            return false;
        }
        return true;
    }

    public String toString() {
        // Write your code here
        if (this.isLongSleeves) {
            return super.toString() + ", Sleeves='Long'";
        } else {
            return super.toString() + "Sleeves='Short'";
        }
    }

}
