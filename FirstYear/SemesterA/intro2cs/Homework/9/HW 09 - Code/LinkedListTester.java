import java.util.NoSuchElementException;

public class LinkedListTester {
    // Explanation on how to use: 
    /**
     * Each of those tests is a boolean function which simulate multiple scenarios
     * If on a given scenario every value was calculated correctly, and no UNEXPECTED errors occur you will get true. 
     * If there was an expected error, The tester knows how to handle it. If you didn't take care of it,  
     * It is going to return false. 
     * 
     * In every function there are one of 2 options
     * -> either check explicitly the value using a boolean 
     * -> check using the verifier function which takes a list and an array and checks whether the values match
     * the array passed is explicitly written inside the function and represent the correct result in the order the were added 
     */
    public static void main(String[] args) {
        System.out.println("Integer Tests");
        System.out.println("getFirst Test");
        System.out.println(getFirstTest());
        System.out.println("getLast Test");
        System.out.println(getLastTest());        
        System.out.println("addFirst Test");
        System.out.println(addFirstTestInt1());
        System.out.println("add Tests");
        System.out.println(addTestInt1());
        System.out.println(addTestInt2());
        System.out.println(addTestInt3());
        System.out.println("remove Tests");
        System.out.println(removeTestInt1());
        System.out.println(removeTestInt2());
        System.out.println(removeTestInt3());
        System.out.println("indexOf Tests");
        System.out.println(indexOfTestInt1()); 
        System.out.println(indexOfTestInt2()); 
        System.out.println(indexOfTestInt3()); 
        System.out.println();

        System.out.println("String Tests");
        System.out.println("getFirst Test");
        System.out.println(getFirstTestString());
        System.out.println("getLast Test");
        System.out.println(getLastTestString());
        System.out.println("addFirst Test");
        System.out.println(addFirstTestString1());
        System.out.println("add Tests");
        System.out.println(addTestString1());
        System.out.println(addTestString2());
        System.out.println(addTestString3());
        System.out.println("remove Tests");
        System.out.println(removeTestString1());
        System.out.println(removeTestString2());
        System.out.println(removeTestString3());
        System.out.println("indexOf Tests");
        System.out.println(indexOfTestString1());
        System.out.println(indexOfTestString2());
        System.out.println(indexOfTestString3());
        System.out.println(indexOfTestString4()); 
    }
    public static boolean verifier(LinkedList<String> list, String [] results){
        if (results.length != list.size()){
            return false;
        }
        ListIterator <String> it = list.iterator();
        for(String s : results){
            if (!s.equals(it.next())) {
                return false;
            }
        }
        return true;
    }
    public static boolean verifier(LinkedList<Integer> list, int [] results){
        if (results.length != list.size()){
            return false;
        }
        ListIterator <Integer> it = list.iterator();
        for(int s : results){
            if (!it.next().equals(s)) {
                return false;
            }
        }
        return true;        
    }
    public static boolean getLastTest(){
        LinkedList<Integer> list = new LinkedList<Integer>();
        try{
            list.getLast();
            return false;
        }catch (NoSuchElementException ex){
            list.add(1);
            try{
                return list.getLast() == 1;
            }catch (Exception e){
                return false;
            }
        }catch (Exception e){
            return false;
        }
    }
    public static boolean getFirstTest(){
        LinkedList<Integer> list = new LinkedList<Integer>();
        try{
            list.getFirst();
            return false;
        }catch (NoSuchElementException ex){
            list.add(1);
            try{
                boolean b = list.getFirst() == 1;
                list.add(2);
                return b && list.getFirst() == 1;
            }catch (Exception e){
                return false;
            }
        }catch (Exception e){
            return false;
        }
    }
    
    public static boolean addFirstTestInt1(){
        LinkedList<Integer> list = new LinkedList<Integer>();
        list.addFirst(4);
        boolean b = list.getFirst() == 4;
        list.addFirst(3);
        b = b && list.getFirst() == 3;
        list.addFirst(2);
        b = b && list.getFirst() == 2;
        int [] res = {2,3,4};
        return list.getLast() == res[res.length - 1] && verifier(list, res) && b;
    }

    public static boolean addTestInt1(){
        LinkedList<Integer> list = new LinkedList<Integer>();
        try {
            list.addFirst(4);
            list.addFirst(3);
            list.addFirst(2); 
            list.add(1,0);
            list.add(7,0);
            list.add(100,list.size());
            list.add(101,list.size());
            list.add(41,3);
        } catch (Exception e){
            return false;
        }
        try{
            list.add(-1,-1);
            return false;
        } catch (IllegalArgumentException e){
            try{
                list.add(-1,list.size()+(int)(Math.random() *5 + 1));
                return false;
            } catch (IllegalArgumentException ex){
                int [] res = {7,1,2,41,3,4,100,101};
                return list.getFirst() == res[0] && list.getLast() == res[res.length - 1] && verifier(list, res);
            }
        } catch (Exception e){
            return false;
        }
    }
    public static boolean addTestInt2() {
        LinkedList<Integer> list = new LinkedList<Integer>();
        try {
            list.add(4);
            list.add(3);
            list.add(2, 0);
            list.add(1, 0);
            list.add(7, list.size());
            list.add(8, list.size());
            list.add(-12, list.size());
            list.add(83, 4);
        } catch (Exception e) {
            return false;
        }
        try {
            list.add(-1, -1);
            return false;
        } catch (IllegalArgumentException e) {
            try {
                list.add(-1, list.size() + (int) (Math.random() * 5 + 1));
                return false;
            } catch (IllegalArgumentException ex) {
                int[] res = {1, 2, 4, 3, 83, 7, 8, -12};
                return list.getFirst() == res[0] && list.getLast() == res[res.length - 1] && verifier(list, res);
            }
        } catch (Exception e) {
            return false;
        }
    }
    public static boolean addTestInt3() {
        LinkedList<Integer> list = new LinkedList<Integer>();
        try {
            list.addFirst(5);
            list.addFirst(6);
            list.add(10, list.size());
            list.add(7, 1);
            list.add(2, 0);
            list.add(8, 2);
            list.add(1, 0);
            list.add(3, 2);
            list.add(9, list.size());
            list.add(11, list.size());
            list.add(12, 8);
        } catch (Exception e) {
            return false;
        }
        try {
            list.add(-1, -1);
            return false;
        } catch (IllegalArgumentException e) {
            try {
                list.add(-1, list.size() + (int) (Math.random() * 5 + 1));
                return false;
            } catch (IllegalArgumentException ex) {
                int[] res = {1, 2, 3, 6, 8, 7, 5, 10, 12, 9, 11};
                return list.getFirst() == res[0] && list.getLast() == res[res.length - 1] && verifier(list, res);
            }
        } catch (Exception e) {
            return false;
        }
    }
    
    public static boolean removeTestInt1(){
        LinkedList<Integer> list = new LinkedList<Integer>();
        try {
            list.add(5);
            list.addFirst(4);
            list.addFirst(3);
            list.addFirst(2); 
            list.add(1,0);
            list.add(7,0);
            list.add(4,0);
            list.add(100,list.size());
            list.add(101,list.size());
            list.add(41,3);
        } catch (Exception e){
            return false;
        }
        try {    
            int [] toRemove = {4,6,4,4};
            int [] sizes = {10,9,9,8,8};
            for (int i = 0; i < toRemove.length; i++) {
                boolean flag = true;
                if (sizes[i] != list.size()){
                    flag = false;
                }
                if (i == 0){
                    flag = flag && (list.getFirst() == 4);
                }
                if (list.remove(toRemove[i])) {
                    flag = flag && (sizes[i] != sizes[i + 1]);
                }else{
                    flag = flag && (sizes[i] == sizes[i + 1]);
                }
                if (i == 0 ){
                    flag = flag && (list.getFirst() == 7);
                }
                if (sizes[i+1] != list.size()){
                    flag = flag && false;
                } 
                if (!flag){
                    return false;
                }  
            }
        }catch (Exception e){
            return false;
        }
        return true;
    }
    public static boolean removeTestInt2() {
        LinkedList<Integer> list = new LinkedList<Integer>();
        try {
            list.add(5);
            list.addFirst(4);
            list.addFirst(3);
            list.addFirst(2);
            list.add(1, 0);
            list.add(7, 0);
            list.add(4, 0);
            list.add(100, list.size());
            list.add(101, list.size());
            list.add(41, 3);
            list.add(200, list.size());
            list.add(300, list.size());
        } catch (Exception e) {
            return false;
        }
        try {
            int[] toRemove = {4, 6, 4, 4, 100, 101 , 101,200,300};
            int[] sizes = {12, 11, 11, 10, 10 ,9 , 8, 8,7,6};
            for (int i = 0; i < toRemove.length; i++) {
                boolean flag = true;
                if (sizes[i] != list.size()) {
                    flag = false;
                }
                if (i == 0) {
                    flag = flag && (list.getFirst() == 4);
                }
                if (list.remove(toRemove[i])) {
                    flag = flag && (sizes[i] != sizes[i + 1]);
                }else{
                    flag = flag && (sizes[i] == sizes[i + 1]);
                }
                if (i == 0) {
                    flag = flag && (list.getFirst() == 7);
                }
                if (sizes[i + 1] != list.size()) {
                    flag = flag && false;
                }
                if (!flag) {
                    return false;
                }
            }
            if (list.size() != 6) {
                return false;
            }
            if (list.getFirst() != 7 || list.getLast() != 5) {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
        return true;
    }
    public static boolean removeTestInt3() {
        LinkedList<Integer> list = new LinkedList<Integer>();
        try {
            list.add(30);
            list.add(35);
            list.add(40);
            list.add(45);
            list.addFirst(50);
            list.add(60, 1);
            list.add(65, 2);
            list.add(20,4);

        } catch (Exception e) {
            return false;
        }
        try {
            int[] toRemove = {50, 40, 30};
            int[] sizes = {8,7,6,5};
            for (int i = 0; i < toRemove.length; i++) {
                boolean flag = true;
                if (sizes[i] != list.size()) {
                    flag = false;
                }
                if (i == 0) {
                    flag = flag && (list.getFirst() == 50);
                }
                if (list.remove(toRemove[i])) {
                    flag = flag && (sizes[i] != sizes[i + 1]);
                }else{
                    flag = flag && (sizes[i] == sizes[i + 1]);
                }
                if (sizes[i + 1] != list.size()) {
                    flag = flag && false;
                }
                if (!flag) {
                    return false;
                }
            }
        } catch (Exception e) {
            return false;
        }
        return true;
    }
    
    public static boolean indexOfTestInt1(){
        LinkedList<Integer> list = new LinkedList<Integer>();
        LinkedList<Integer> resList = new LinkedList<Integer>();
        
        try {
            resList.add(list.indexOf(4));
            list.add(5);
            list.addFirst(4);
            resList.add(list.indexOf(4)); 
            list.addFirst(3);
            list.addFirst(2); 
            resList.add(list.indexOf(4));
            list.add(1,0);
            list.add(7,0);
            resList.add(list.indexOf(4)); 
            list.add(8,3);
            list.add(5,2);
            list.add(10,7);
            list.add(12,6);
            resList.add(list.indexOf(4)); 
            list.add(4,0);
            resList.add(list.indexOf(4)); 
            list.add(100,list.size());
            list.add(101,list.size());
            list.add(41,3);
            resList.add(list.indexOf(null)); 
        } catch (Exception e){
            return false;
        }        
        int [] res = {-1,0,2,4,7,0,-1};
        return verifier(resList, res);

    }
    public static boolean indexOfTestInt2() {
        LinkedList<Integer> list = new LinkedList<Integer>();
        LinkedList<Integer> resList = new LinkedList<Integer>();
    
        try {
            resList.add(list.indexOf(4));
            list.add(5);
            list.addFirst(4);
            resList.add(list.indexOf(4));
            list.addFirst(3);
            list.addFirst(2);
            resList.add(list.indexOf(4));
            list.add(1, 0);
            resList.add(list.indexOf(4));
            list.add(4, 0);
            resList.add(list.indexOf(4));
            list.add(100, list.size());
            resList.add(list.indexOf(5));
            list.add(101, list.size());
            list.add(41, 3);
            resList.add(list.indexOf(null));
            list.remove(4);
            resList.add(list.indexOf(4));
            list.remove(4);
            resList.add(list.indexOf(4));
            resList.add(list.indexOf(5));

        } catch (Exception e) {
            return false;
        }
        int[] res = {-1, 0, 2, 3, 0, 5, -1, 4, -1, 4};
        return verifier(resList, res);
    }
    public static boolean indexOfTestInt3() {
        LinkedList<Integer> list = new LinkedList<Integer>();
        LinkedList<Integer> resList = new LinkedList<Integer>();
    
        try {
            resList.add(list.indexOf(-4));
            list.addFirst(-5);
            list.addFirst(-4);
            resList.add(list.indexOf(-4));
            list.add(3, 0);
            list.add(7, 0);
            resList.add(list.indexOf(-4));
            list.add(4, 0);
            resList.add(list.indexOf(-4));
            list.add(100, list.size());
            list.add(101, list.size());
            list.add(41, 3);
            resList.add(list.indexOf(null));
            list.remove(-4);
            resList.add(list.indexOf(-4));
            list.remove(-4);
            resList.add(list.indexOf(-4));
            list.add(-4,3);
            resList.add(list.indexOf(-4));
            resList.add(list.indexOf(6));
            resList.add(list.indexOf(7));
        } catch (Exception e) {
            return false;
        }
        int[] res = {-1, 0, 2, 3, -1, -1, -1,3,-1,1};
        return verifier(resList, res);
    }
    
    //////////
    public static boolean getLastTestString(){
        LinkedList<String> list = new LinkedList<String>();
        try{
            list.getLast();
            return false;
        }catch (NoSuchElementException ex){
            list.add("HELLO");
            try{
                boolean b = list.getLast().equals("HELLO");
                list.add("World");
                b = b && list.getLast().equals("World");
                return b;
            }catch (Exception e){
                return false;
            }
        }catch (Exception e){
            return false;
        }
    }
    public static boolean getFirstTestString(){
        LinkedList<String> list = new LinkedList<String>();
        try{
            list.getFirst();
            return false;
        }catch (NoSuchElementException ex){
            list.add("HELLO");
            try{
                boolean b = list.getFirst().equals("HELLO");
                list.add("World");
                b = b && list.getFirst().equals("HELLO");
                return b;
            }catch (Exception e){
                return false;
            }
        }catch (Exception e){
            return false;
        }
    }
    public static boolean addFirstTestString1(){
        LinkedList<String> list = new LinkedList<String>();
        try{
            list.addFirst("Water");
            boolean b = list.getFirst().equals("Water");
            list.addFirst("Desk");
            b = b && list.getFirst().equals("Desk");
            list.addFirst("Gum");
            b = b && list.getFirst().equals("Gum");
            String [] res = {"Gum","Desk","Water"};
            return list.getLast().equals(res[res.length - 1]) && verifier(list, res) && b;
        } catch (Exception e){
            return false;
        }
    }

    public static boolean addTestString1(){
        LinkedList<String> list = new LinkedList<String>();
        try {
            list.addFirst("Hello");
            list.addFirst("Water");
            list.addFirst("Screen"); 
            list.add("1",0);
            list.add("K",0);
            list.add("Helloo",list.size());
            list.add("Cat",2);
            list.add("101",list.size());
            list.add("Test",3);
            list.add("Hop",7);
        } catch (Exception e){
            return false;
        }
        try{
            list.add("Error",-1);
            return false;
        } catch (IllegalArgumentException e){
            try{
                list.add("Exception",list.size()+(int)(Math.random() *5 + 1));
                return false;
            } catch (IllegalArgumentException ex){
                String [] res = {"K","1","Cat","Test","Screen", "Water", "Hello","Hop","Helloo","101"};
                return list.getFirst().equals(res[0]) && list.getLast().equals(res[res.length - 1]) && verifier(list, res);
            }
        } catch (Exception e){
            return false;
        }
    }
    public static boolean addTestString2() {
        LinkedList<String> list = new LinkedList<String>();
        try {
            list.addFirst("c");
            list.addFirst("b");
            list.addFirst("a");
            list.add("d", 0);
            list.add("k");
            list.add("x", 0);
            list.add("z", list.size());
            list.add("y", list.size());
            list.add("e", 3);
        } catch (Exception e) {
            return false;
        }
        try {
            list.add(null, -1);
            return false;
        } catch (IllegalArgumentException e) {
            try {
                list.add(null, list.size() + (int) (Math.random() * -4 + 9));
                return false;
            } catch (IllegalArgumentException ex) {
                String[] res = {"x", "d", "a", "e", "b", "c", "k", "z", "y"};
                return list.getFirst().equals(res[0]) && list.getLast().equals(res[res.length - 1]) && verifier(list, res);
            }
        } catch (Exception e) {
            return false;
        }
    }
    public static boolean addTestString3() {
        LinkedList<String> list = new LinkedList<String>();
        try {
            list.addFirst("Kelso");
            list.addFirst("Donna");
            list.addFirst("Eric");
            list.add("Hyde", 0);
            list.add("Jackie");
            list.add("Fez", 2);
            list.add("Red", list.size());
            list.add("Kitty", list.size());
            list.add("Bob", 1);
        } catch (Exception e) {
            return false;
        }
        try {
            list.add(null, -1);
            return false;
        } catch (IllegalArgumentException e) {
            try {
                list.add(null, list.size() + (int) (Math.random() * 505 + 1));
                return false;
            } catch (IllegalArgumentException ex) {
                String[] res = {"Hyde", "Bob", "Eric", "Fez", "Donna", "Kelso", "Jackie", "Red", "Kitty"};
                return list.getFirst().equals(res[0]) && list.getLast().equals(res[res.length - 1]) && verifier(list, res);
            }
        } catch (Exception e) {
            return false;
        }
    }

    public static boolean removeTestString1() {
        LinkedList<String> list = new LinkedList<String>();
        try {
            list.add("Monica");
            list.addFirst("Rachel");
            list.addFirst("Ross");
            list.addFirst("Joey");
            list.add("Chandler", 0);
            list.add("Phoebe", 0);
            list.add("New York City", list.size());
            list.add("Unagi", 3);
            list.add("The One With", list.size());
            list.add("The Break", list.size());
        } catch (Exception e) {
            return false;
        }
        try {
            String[] toRemove = {"Chandler", "Central Perk", "Joey", "Unagi", "The One With","The Break","The Break","New York City","Thanksgiving"};
            int[] sizes = {10, 9, 9, 8, 7, 6, 5,5,4,4};
            for (int i = 0; i < toRemove.length; i++) {
                boolean flag = true;
                if (sizes[i] != list.size()) {
                    flag = false;
                }
                if (i == 0) {
                    flag = flag && (list.getFirst().equals("Phoebe"));
                }
                if (list.remove(toRemove[i])) {
                    flag = flag && (sizes[i] != sizes[i + 1]);
                }else{
                    flag = flag && (sizes[i] == sizes[i + 1]);
                }
                if (i == 0) {
                    flag = flag && (list.getFirst().equals("Phoebe"));
                }
                if (sizes[i + 1] != list.size()) {
                    flag = flag && false;
                }
                if (!flag) {
                    return false;
                }
            }
            if (list.size() != 4) {
                return false;
            }
            if (!list.getFirst().equals("Phoebe")  || !list.getLast().equals("Monica") ) {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
        return true;
    }
    public static boolean removeTestString2() {
        LinkedList<String> list = new LinkedList<String>();
        try {
            list.add("Robin");
            list.addFirst("Ted");
            list.addFirst("Lily");
            list.addFirst("Marshall");
            list.add("Tracy", 2);
            list.add("Barney", 0);
            list.add("MacLaren's Pub", 2);
            list.add("New York City", list.size());
            list.add("The Bro Code", 3);
            list.add("The Yellow Umbrella", list.size());
            list.add("The Mother", list.size());
            list.add("Ted's Blue French Horn", list.size());
        } catch (Exception e) {
            return false;
        }
        try {
            
            String[] toRemove = {"Barney", "MacLaren's Pub", "Marshall", "The Bro Code","The Yellow Umbrella","The Mother","The Mother","New York City", "Marshall"};
            int[] sizes = {12, 11, 10, 9, 8, 7, 6, 6, 5, 5};
            for (int i = 0; i < toRemove.length; i++) {
                boolean flag = true;
                if (sizes[i] != list.size()) {
                    flag = false;
                }
                if (i == 0) {
                    flag = flag && (list.getFirst().equals("Barney"));
                }
                if (list.remove(toRemove[i])) {
                    flag = flag && (sizes[i] != sizes[i + 1]);
                }else{
                    flag = flag && (sizes[i] == sizes[i + 1]);
                }
                if (i == 0) {
                    flag = flag && (list.getFirst().equals("Marshall"));
                }
                if (sizes[i + 1] != list.size()) {
                    flag = flag && false;
                }
                if (!flag) {
                    return false;
                }
            }
            if (list.size() != 5) {
                return false;
            }
            if (!list.getFirst().equals("Lily") || !list.getLast().equals( "Ted's Blue French Horn")) {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
        return true;
    }


    public static boolean removeTestString3() {
        LinkedList<String> list = new LinkedList<String>();
        try {
            list.add("Robin");
            list.add("Ted");
            list.add("Barney");
            list.add("Lily", 1);
            list.add("New York City",list.size());
            list.add("Judge Fudge",3);
        } catch (Exception e) {
            return false;
        }
        try {
            String[] toRemove = {"Barney", "Ted", "New York City","Judge Fudge"};
            int[] sizes = {6, 5, 4,3,2};
            for (int i = 0; i < toRemove.length; i++) {
                boolean flag = true;
                if (sizes[i] != list.size()) {
                    flag = false;
                }
                if (i == 0) {
                    flag = flag && (list.getFirst().equals("Robin"));
                }
                if (list.remove(toRemove[i])) {
                    flag = flag && (sizes[i] != sizes[i + 1]);
                }else{
                    flag = flag && (sizes[i] == sizes[i + 1]);
                }
                if (i == 0) {
                    flag = flag && (list.getFirst().equals("Robin"));
                }
                if (sizes[i + 1] != list.size()) {
                    flag = flag && false;
                }
                if (!flag) {
                    return false;
                }
            }
            if (list.size() != 2) {
                return false;
            }
            if (!list.getLast().equals("Lily") || !list.getFirst().equals("Robin")) {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
        return true;
    }

    public static boolean indexOfTestString1() {
        LinkedList<String> list = new LinkedList<String>();
        LinkedList<Integer> resList = new LinkedList<Integer>();
    
        try {
            list.addFirst("Monica");
            list.addFirst("Rachel");
            list.addFirst("Ross");
            list.addFirst("Richard");
            resList.add(list.indexOf("Monica"));
            list.add("Chandler", 0);
            list.add("Joey", 0);
            resList.add(list.indexOf("Monica"));
            list.add("Phoebe", 0);
            resList.add(list.indexOf("Monica"));
            resList.add(list.indexOf("Joey"));
            resList.add(list.indexOf("Gunther"));
            list.add("Central Perk", list.size());
            list.add("The Geller's House", list.size());
            list.add("Ross's divroce lawer", 3);
            resList.add(list.indexOf("Ross"));
        } catch (Exception e) {
            return false;
        }
        
        int[] res = {3, 5, 6, 1, -1, 5};
        return verifier(resList, res);
    }
    
    public static boolean indexOfTestString2() {
        LinkedList<String> list = new LinkedList<String>();
        LinkedList<Integer> resList = new LinkedList<Integer>();
    
        try {
            list.add("Cinnamon");
            list.add("Penny",1);
            list.addFirst("Sheldon");
            list.add("Leonard",0);
            resList.add(list.indexOf("Sheldon")); 
            list.add("Howard",2);
            list.add("Raj",4);
            resList.add(list.indexOf("Sheldon"));
            list.add("Bernadette",2);
            list.add("Amy",0);
            resList.add(list.indexOf("Sheldon"));
            list.add("Caltech",list.size());
            list.add("The Comic Book Store",list.size());
            list.add("The Big Bang Theory",6);
            list.add("Stuart",6);
            resList.add(list.indexOf("Penny"));
            list.remove("Sheldon");
            resList.add(list.indexOf("Sheldon"));
            list.remove("Amy");
            resList.add(list.indexOf("Amy"));
            resList.add(list.indexOf("Penny"));
            resList.add(list.indexOf("The Big Bang Theory"));
        } catch (Exception e) {
            return false;
        }
        int[] res = {1, 1, 2, 9, -1, -1, 7, 5};
        return verifier(resList, res);
    }
    public static boolean indexOfTestString3() {
        LinkedList<String> list = new LinkedList<String>();
        LinkedList<Integer> resList = new LinkedList<Integer>();
    
        try {
            resList.add(list.indexOf("Sheldon"));
            list.add("Leonard",0);
            list.add("Penny",1);
            resList.add(list.indexOf("Sheldon"));
            list.add("Raj",2);
            list.add("Bernadette",3);
            resList.add(list.indexOf("Sheldon"));
            list.add("Amy",1);
            list.add("Howard",3);
            resList.add(list.indexOf("Sheldon"));
            list.add("The Big Bang Theory",list.size());
            list.add("Caltech",list.size());
            list.add("The Comic Book Store",list.size());
            list.add("Stuart",6);
            resList.add(list.indexOf("Penny"));
            list.remove("Sheldon");
            resList.add(list.indexOf("Sheldon"));
            list.remove("Amy");
            resList.add(list.indexOf("Amy"));
            resList.add(list.indexOf("Penny"));
            resList.add(list.indexOf("The Big Bang Theory"));
        } catch (Exception e) {
            return false;
        }
        int[] res = {-1,-1,-1,-1,2,-1,-1,1,6};
        return verifier(resList, res);
    }
    public static boolean indexOfTestString4() {
        LinkedList<String> list = new LinkedList<String>();
        LinkedList<Integer> resList = new LinkedList<Integer>();
    
        try {
            resList.add(list.indexOf("Sheldon"));
            list.add("Leonard",0);
            list.add("Penny",1);
            resList.add(list.indexOf("Sheldon"));
            list.add("Raj",2);
            list.add("Bernadette",3);
            resList.add(list.indexOf("Sheldon"));
            list.add("Amy",1);
            list.add("Howard",3);
            resList.add(list.indexOf("Sheldon"));
            list.add("The Big Bang Theory",list.size());
            list.add("Caltech",list.size());
            list.add("The Comic Book Store",list.size());
            list.add("Stuart",6);
            resList.add(list.indexOf("Penny"));
            list.remove("Sheldon");
            resList.add(list.indexOf("Sheldon"));
            list.remove("Leonard");
            resList.add(list.indexOf("Leonard"));
            resList.add(list.indexOf("Bernadette"));
            resList.add(list.indexOf("Caltech"));
        } catch (Exception e) {
            return false;
        }
        int[] res = {-1,-1,-1,-1,2,-1,-1,4,7};
        return verifier(resList, res);
    }
}
