public class Item {
    protected int barcode;
    protected String name;
    protected double price;

    public Item(int barcode, String name, double price) {
        this.barcode = barcode;
        this.name = name;
        this.price = price;
    }

    public int getBarcode() {
        return this.barcode;
    }

    public String getName() {
        return this.name;
    }

    public double getPrice() {
        return this.price;
    }

    public void setPrice(double price) {
        this.price = price;
    }

    public void discount(double percentage) {
        // Write your code here
        if (percentage > 100) {
            throw new IllegalArgumentException();
        } else {
            this.price = this.price * (1 - percentage / 100);
        }
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

        Item other = (Item) o;
        if (barcode != ((Item) o).barcode) {
            return false;
        }
        return true;
    }

    public String toString() {
        // Write your code here
        return getClass() + "{name='" + this.name + "', price=" + this.price + "}";
    }
}
