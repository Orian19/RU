public class Shop {
    private int shirtCount;
    private int jeansCount;
    private Shirt[] shirts;
    private Jeans[] jeans;

    public Shop(int size) {
        // Write your code here
        this.shirts = new Shirt[size];
        this.jeans = new  Jeans[size];
        this.shirtCount = 0;
        this.jeansCount = 0;
    }


    public Shirt[] getShirts() {
        return this.shirts;
    }

    public Jeans[] getJeans() {
        return this.jeans;
    }

    public int getshirtCount() {
        return this.shirtCount;
    }

    public int getjeansCount() {
        return this.jeansCount;
    }

    public void addItem(Item item) {
        // Write your code here
        if (item != null) {
            if (item.getClass() == Shirt.class || item.getClass() == PoloShirt.class) {
                Shirt shirtI = (Shirt) item;
                if (!checkIfShirtExists(shirtI)) {
                    if (this.shirtCount != this.shirts.length) {
                        this.shirts[this.shirtCount] = shirtI;
                    } else { // array is full --> minimal price item is taken out
                        Shirt minS = (Shirt) getMinimalP(this.shirts);
                        removeItem(minS);
                        this.shirts[this.shirtCount] = shirtI;
                    }
                    this.shirtCount++;
                }
            } else if (item.getClass() == Jeans.class) {
                Jeans jeansI = (Jeans) item;
                if (!checkIfJeansExists(jeansI)) {
                    if (this.jeansCount != this.jeans.length) {
                        this.jeans[this.jeansCount] = jeansI;
                    } else { // array is full --> minimal price item is taken out
                        Jeans minJ = (Jeans) getMinimalP(this.jeans);
                        removeItem(minJ);
                        this.jeans[this.jeansCount] = jeansI;
                    }
                    this.jeansCount++;
                }
            } else {
                throw new IllegalArgumentException();
            }
        } else {
            throw new IllegalArgumentException();
        }
    }

    private boolean checkIfShirtExists(Shirt shirt) {
        if (this.shirtCount > 0) {
            for (int i = 0; i < this.shirtCount; i++) {
                Shirt curShirt = this.shirts[i];
                if (curShirt.getBarcode() == shirt.getBarcode()) {
                    return true;
                }
            }
            return false;
        }
        return false;
    }

    private boolean checkIfJeansExists(Jeans jeansI) {
        if (this.jeansCount > 0) {
            for (int i = 0; i < this.jeansCount; i++) {
                Jeans curJeans = this.jeans[i];
                if (curJeans.equals(jeansI)) {
                    return true;
                }
            }
            return false;
        }
        return false;
    }

     private Item getMinimalP(Item[] items) {
        Item curMin = items[0];
         for (int i = 0; i < items.length; i++) {
             Item curItem = items[i];
             if (curItem.getPrice() < curMin.getPrice()) {
                 curMin = curItem;
             }
         }
         return curMin;
    }

    public void removeItem(Item item) {
        // Write your code here
        if (item != null) {
            if (item.getClass() == Shirt.class) {
                Shirt shirtI = (Shirt) item;
                Shirt[] newS = new Shirt[this.shirts.length];
                int newSIdx = 0;
                for (int i = 0; i < this.shirtCount; i++) {
                    if (!this.shirts[i].equals(shirtI)) {
                        newS[newSIdx] = this.shirts[i];
                        newSIdx++;
                    }
                }
                this.shirtCount--;
                this.shirts = newS;
            }
            if (item.getClass() == Jeans.class) {
                Jeans jeansI = (Jeans) item;
                Jeans[] newJ = new Jeans[this.jeans.length];
                int newJIdx = 0;
                for (int i = 0; i < this.jeansCount; i++) {
                    if (!this.jeans[i].equals(jeansI)) {
                        newJ[newJIdx] = this.jeans[i];
                        newJIdx++;
                    }
                }
                this.jeansCount--;
                this.jeans = newJ;
            }
        } else {
            throw new IllegalArgumentException();
        }
    }


    public Shirt getShirt(int barcode) {
        // Write your code here
        for (int i = 0; i < this.shirtCount; i++) {
            if (this.shirts[i].getBarcode() == barcode) {
                return this.shirts[i];
            }
        }
        throw new IllegalArgumentException();
    }

    public Jeans getJeans(int barcode) {
        // Write your code here
        for (int i = 0; i < this.jeansCount; i++) {
            if (this.jeans[i].getBarcode() == barcode) {
                return this.jeans[i];
            }
        }
        throw new IllegalArgumentException();
    }


    public void discountJeans(double percentage) {
        // Write your code here
        for (int i = 0; i < this.jeansCount; i++) {
            double curPrice = this.jeans[i].price;
            this.jeans[i].setPrice(curPrice * (1 - percentage / 100));;
        }
    }

    public void discountPoloShirts(double percentage) {
        // Write your code here
        for (int i = 0; i < this.shirtCount; i++) {
            if (this.shirts[i].getClass() == PoloShirt.class) {
                double curPrice = this.shirts[i].price;
                this.shirts[i].setPrice(curPrice * (1 - percentage / 100));
            }
        }
    }

    public void printDetails() {
        // Write your code here
        for (int i = 0; i < this.shirtCount; i++) {
            System.out.println(this.shirts[i]);
        }
        for (int i = 0; i < this.jeansCount; i++) {
            System.out.println(this.jeans[i]);
        }
    }

    public double getProfit() {
        // Write your code here
        double totalAmount = 0.0;
        for (int i = 0; i < this.shirtCount; i++) {
            totalAmount += this.shirts[i].getPrice();
        }
        for (int i = 0; i < this.jeansCount; i++) {
            totalAmount += this.jeans[i].getPrice();
        }
        return totalAmount;
    }

    public void sortShirts() {
        // Write your code here

        // sorting using selection sort
        for (int i = 0; i < this.shirtCount; i++) {
            int minIdx = i;
            for (int j = i; j < this.shirtCount; j++) {
                if (this.shirts[j].getPrice() < this.shirts[minIdx].getPrice()) {
                    minIdx = j;
                }
            }
            swap(this.shirts, i, minIdx);
        }
    }

    public void sortJeans() {
        // Write your code here

        // sorting using selection sort
        for (int i = 0; i < this.jeansCount; i++) {
            int minIdx = i;
            for (int j = i; j < this.jeansCount; j++) {
                if (this.jeans[j].getPrice() < this.jeans[minIdx].getPrice()) {
                    minIdx = j;
                }
            }
            swap(this.jeans, i, minIdx);
        }
    }

    private void swap(Item[] items, int i, int j) {
        Item temp = items[i];
        items[i] = items[j];
        items[j] = temp;
    }

}