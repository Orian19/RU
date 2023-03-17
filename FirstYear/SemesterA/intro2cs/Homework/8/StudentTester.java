public class StudentTester {
    // Note this is a very short tester since it does have a lot of functions which are used in 
    // College
    public static void main(String[] args) {
        studentTest();
    }

    public static void studentTest() {
        System.out.println("Creating students...");
        Student gaga = new Student(1,"Lady Gaga");
        Student bing = new Student(2,"Chandler Bing");
        Student bond = new Student(7,"James Bond");
        Student craig = new Student(7,"Bond");
        System.out.println("Adding courses...");
		gaga.addCourse(new Course(5, "Dancing"), 100);
		gaga.addCourse(new Course(1, "Card Games"), 98);
		gaga.addCourse(new Course(2, "Psychiatry"), 60);
		bing.addCourse(new Course(6, "Data Reconfiguration"), 97);
		bing.addCourse(new Course(3, "Sarcasm 101"), 100);
		bing.addCourse(new Course(4, "Statisical Analysis"), 98);
		bing.addCourse(new Course(7, "Copywriting"), 95);
        System.out.println("\nStudent Reports:");
        System.out.println(gaga.studentReport());
		System.out.println(bing.studentReport());    
        System.out.println("\nGrade in Courses:");
        System.out.println(bing.gradeInCourse(new Course(3, "Sarcasm 101")));
        System.out.println(gaga.gradeInCourse(new Course(1, "Card Games")));
        System.out.println(gaga.gradeInCourse(new Course(4, "Statisical Analysis")));
        
        //// All should be true 
        System.out.println("\nTook Courses:");
        System.out.println(bing.tookCourse(new Course(3, "Sarcasm 101")));
        System.out.println(!bing.tookCourse(new Course(10, "Transfiguration")));
        System.out.println(gaga.tookCourse(new Course(1, "Card Games")));
        System.out.println(!gaga.tookCourse(new Course(6, "Data Reconfiguration")));
        
        // All should be true
        System.out.println("\nEquals:");
        System.out.println(!bing.equals(bond));
        System.out.println(!gaga.equals(bond));
        System.out.println(!bond.equals(gaga));
        System.out.println(!gaga.equals(bing));
        System.out.println(bond.equals(craig));

    }
}
