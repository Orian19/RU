public class Jeans extends Item {
    private String brand;

    public Jeans(String brand, int barcode, String name, double price) {
        super(barcode, name, price);
        this.brand = brand;
    }

    public String getBrand() {
        return this.brand;
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

        Jeans other = (Jeans) o;
        if (!super.equals(o)) {
            return false;
        }
        if (!this.brand.equals(other.brand)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        // Write your code here
        return super.toString() + ", brand='" + this.brand + "'";
    }
}
