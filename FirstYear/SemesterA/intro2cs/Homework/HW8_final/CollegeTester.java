public class CollegeTester {
    // In this tester we are going to check questions differently
    // Here is a list of the functions and what questions they check
    // 
    // 1. hogwarts (Q1 && Q5)
    // 2. hogwartsRemove (Q3 && Q7)
    // 3. hogwartsGet (Q4 && Q8)
    // 4. hogwartsGrades (Q9)
    // 5. hogwartsStudentReport (Q10)
    // 6. hogwartsWhoTook (Q11)
    // 7. hogwartsWithGrade (Q12)
    // 8. hogwartsCoursesAverages (Q13)
    // 9. hogwartsTopPreformer (Q14)

    // Note : Q2 & Q6 are checked with the statement: "System.out.println(hogwarts);"  



    public static void main(String[] args) {
        System.out.println("Welcome to Hogwarts School of Witchcraft and Wizardry");
        College hogwarts = hogwarts();
        System.out.println(hogwarts);  
        boolean b = hogwartsRemove(hogwarts);
        System.out.println(b);// if false -> there is an error in the remove
        System.out.println(hogwarts);
        b = b && hogwartsGet(hogwarts);
        System.out.println(b);// if false -> there is an error in the get
        hogwartsGrades(hogwarts);
        hogwartsStudentReport(hogwarts);
        hogwartsWhoTook(hogwarts);
        hogwartsWithGrade(hogwarts);
        hogwartsCoursesAverages(hogwarts);
        hogwartsTopPreformer(hogwarts);
    }
    public static College hogwarts(){
        College hogwarts = new College("Hogwarts School of Witchcraft and Wizardry");
        System.out.println("Trying to add Courses and Students");
        // Adding courses (You should use courseList for debug)
        hogwarts.addCourse(22, "Soccer");
        hogwarts.addCourse(1, "Defense Against the Dark Arts");
        hogwarts.addCourse(2, "Charms");
        hogwarts.addCourse(3, "Potions");
        hogwarts.addCourse(21, "Wand Making");
        hogwarts.addCourse(4, "Divination");
        hogwarts.addCourse(5, "Herbology");
        hogwarts.addCourse(6, "Astronomy");
        hogwarts.addCourse(7, "Flying");
        hogwarts.addCourse(2, "Exploring the Castle");// Should not be successfull
        hogwarts.addCourse(8, "Muggle Studies");
        hogwarts.addCourse(9, "History of Magic");
        hogwarts.addCourse(10, "Transfiguration"); 
        hogwarts.addCourse(6, "Ancient Runes");// Should not be successfull
        hogwarts.addCourse(11, "Ancient Runes");
        hogwarts.addCourse(12, "Care of Magical Creatures");
        hogwarts.addCourse(13, "Arithmancy");
        hogwarts.addCourse(14, "Study of Ancient Witchcraft and Wizardry");
        hogwarts.addCourse(15, "Apparition");
        hogwarts.addCourse(16, "Study of Dragon Language");
        hogwarts.addCourse(17, "Study of Gobbledegook");
        hogwarts.addCourse(18, "Study of Mermish");
        hogwarts.addCourse(19, "Study of Centaur Language");
        hogwarts.addCourse(20, "Study of Kelpie and Grindylow Language");
        hogwarts.addCourse(22, "Basketball");// Should not be successfull

        // Adding students (You should use studentList for debug)
        hogwarts.addStudent(22, "Molly Weasley");
        hogwarts.addStudent(1, "Harry James Potter");
        hogwarts.addStudent(2, "Hermione Granger");
        hogwarts.addStudent(3, "Ron Weasley");
        hogwarts.addStudent(4, "Draco Malfoy");
        hogwarts.addStudent(5, "Neville Longbottom");
        hogwarts.addStudent(6, "Luna Lovegood");
        hogwarts.addStudent(7, "Ginny Weasley");
        hogwarts.addStudent(8, "Fred Weasley");
        hogwarts.addStudent(9, "George Weasley");
        hogwarts.addStudent(10, "Aberforth Dumbledore");
        hogwarts.addStudent(11, "Albus Percival Wulfric Brian Dumbledore");
        hogwarts.addStudent(12, "Severus Snape");
        hogwarts.addStudent(13, "Rubeus Hagrid");
        hogwarts.addStudent(14, "Minerva McGonagall");
        hogwarts.addStudent(15, "Remus Lupin");
        hogwarts.addStudent(16, "Sirius Black");
        hogwarts.addStudent(17, "Bellatrix Lestrange");
        hogwarts.addStudent(18, "Tom Marvollo Riddle");
        hogwarts.addStudent(8, "Professor Quirrell");//Should Fail to insert
        hogwarts.addStudent(3, "Dudley Dursley");//Should Fail to insert
        hogwarts.addStudent(21, "Arthur Weasley");
        hogwarts.addStudent(23, "James Potter");
        hogwarts.addStudent(24, "Lily Potter");
        hogwarts.addStudent(27, "Cho Chang");
        hogwarts.addStudent(28, "Cedric Diggory");
        hogwarts.addStudent(2, "Fleur Delacour");// Should Fail to insert
        hogwarts.addStudent(1, "Viktor Krum");// Should Fail to insert

        return hogwarts;

    }
    
    public static boolean hogwartsRemove(College hogwarts){
        System.out.println("Trying to remove Courses and Students");
        // removing courses
        boolean b = !hogwarts.removeCourse(0);// Should Fail since never existed
        b = b && hogwarts.removeCourse(16);
        b = b && hogwarts.removeCourse(17);
        b = b && hogwarts.removeCourse(18);
        b = b && hogwarts.removeCourse(19);
        b = b && hogwarts.removeCourse(20);
        b = b && !hogwarts.removeCourse(16);//Should fail already removed
        b = b && hogwarts.removeCourse(22);

        //removing students
        hogwarts.removeStudent(0); // Should fail
        for (int i = 15; i <= 29; i++) { 
            b = (i == 29 || i == 19 || i == 20 || i == 25 || i == 26)? (b && (!hogwarts.removeStudent(i))) : (b && hogwarts.removeStudent(i));
            // those are the values which are missing from the student list, so will fail on those!
            if (i == 23){
                b = b && !hogwarts.removeStudent(16); // Should fail already removed
            }    
        }
        return b; 
    }

    public static boolean hogwartsGet(College hogwarts){
        System.out.println("Get Tests");
        boolean course = true;
        boolean student = true;
        course = course && (hogwarts.getCourse(0) == null);
        course = course && (hogwarts.getCourse(22) == null);
        course = course && (hogwarts.getCourse(1).getTitle().equals("Defense Against the Dark Arts"));
        course = course && (hogwarts.getCourse(21).getTitle().equals("Wand Making"));
        course = course && (hogwarts.getCourse(10).getTitle().equals("Transfiguration"));
        course = course && (hogwarts.getCourse(19) == null);
        hogwarts.addCourse(19, "Study of Centaur Language");
        course = course && (hogwarts.getCourse(19).getTitle().equals("Study of Centaur Language"));
        hogwarts.removeCourse(19);
        course = course && (hogwarts.getCourse(19) == null);


        student = student && (hogwarts.getStudent(0) == null);
        student = student && (hogwarts.getStudent(22) == null);
        student = student && (hogwarts.getStudent(1).getName().equals("Harry James Potter"));
        student = student && (hogwarts.getStudent(2).getName().equals("Hermione Granger"));
        student = student && (hogwarts.getStudent(11).getName().equals("Albus Percival Wulfric Brian Dumbledore"));
        student = student && (hogwarts.getStudent(14).getName().equals("Minerva McGonagall"));

        student = student && (hogwarts.getStudent(28) == null);
        hogwarts.addStudent(28, "Cedric Diggory");
        student = student && (hogwarts.getStudent(28).getName().equals("Cedric Diggory"));
        hogwarts.removeStudent(28);
        student = student && (hogwarts.getStudent(28) == null);

        return student && course;

        
    }
    public static void hogwartsGrades(College hogwarts){
        System.out.println("Adding Grades to Students");
        // Grades for Harry
        hogwarts.addCourseTaken(1, 1, 96);
        hogwarts.addCourseTaken(1, 2, 97);
        hogwarts.addCourseTaken(1, 3, 76);
        hogwarts.addCourseTaken(1, 4, 65);
        hogwarts.addCourseTaken(1, 5, 89);
        hogwarts.addCourseTaken(1, 22, 92);// fail
        hogwarts.addCourseTaken(1, 0, 92);// fail
        hogwarts.addCourseTaken(1, 1, 100);// update
        hogwarts.addCourseTaken(1, 6, 96);
        hogwarts.addCourseTaken(1, 7, 98);
        hogwarts.addCourseTaken(1, 3, 92); // update
        hogwarts.addCourseTaken(1, 8, 100);
        hogwarts.addCourseTaken(1, 9, 93);
        hogwarts.addCourseTaken(1, 10, 95);
        hogwarts.addCourseTaken(1, 9, 87); //update 
        hogwarts.addCourseTaken(1, 15, 80);
        hogwarts.addCourseTaken(1, 11, 95);
        hogwarts.addCourseTaken(1, 12, 94);
        hogwarts.addCourseTaken(1, 12, 94); // update
        hogwarts.addCourseTaken(1, 9, 96); //update 



        // Grades for Hermaione
        hogwarts.addCourseTaken(2, 1, 95);
        hogwarts.addCourseTaken(2, 2, 97);
        hogwarts.addCourseTaken(2, 11, 87); 
        hogwarts.addCourseTaken(2, 3, 92);
        hogwarts.addCourseTaken(2, 5, 98);
        hogwarts.addCourseTaken(2, 6, 89);
        hogwarts.addCourseTaken(2, 8, 100);
        hogwarts.addCourseTaken(2, 9, 94);
        hogwarts.addCourseTaken(2, 10, 99);
        hogwarts.addCourseTaken(2, 11, 96);  // update
        hogwarts.addCourseTaken(2, 15, 85);
        hogwarts.addCourseTaken(2, 12, 95);
        hogwarts.addCourseTaken(2, 13, 97);
        hogwarts.addCourseTaken(2, 14, 91);
        hogwarts.addCourseTaken(2, 16, 91);// fail removed course
        hogwarts.addCourseTaken(2, 17, 88);// fail removed course
        hogwarts.addCourseTaken(2, 18, 94);// fail removed course
        hogwarts.addCourseTaken(2, 19, 92);// fail removed course
        hogwarts.addCourseTaken(2, 20, 96);// fail removed course

        // courses for Ron
        hogwarts.addCourseTaken(3, 6, 90);
        hogwarts.addCourseTaken(3, 1, 94);
        hogwarts.addCourseTaken(3, 2, 75);
        hogwarts.addCourseTaken(3, 8, 90);
        hogwarts.addCourseTaken(3, 3, 70);
        hogwarts.addCourseTaken(3, 5, 85);
        hogwarts.addCourseTaken(3, 6, 90);
        hogwarts.addCourseTaken(3, 7, 75);
        hogwarts.addCourseTaken(3, 9, 80);
        hogwarts.addCourseTaken(3, 10, 70);
        hogwarts.addCourseTaken(3, 14, 79);
        hogwarts.addCourseTaken(3, 4, 60);
        hogwarts.addCourseTaken(3, 4, -1); // fail invalid grade
        hogwarts.addCourseTaken(3, 4, 101); // fail invalid grade
        hogwarts.addCourseTaken(3, 4, 100); 
        hogwarts.addCourseTaken(3, 1, 84); // update

        // courses for Draco
        hogwarts.addCourseTaken(4, 3, 94);
        hogwarts.addCourseTaken(4, 2, 78);
        hogwarts.addCourseTaken(4, 6, 70);
        hogwarts.addCourseTaken(4, 1, 70);
        hogwarts.addCourseTaken(4, 8, 90);
        hogwarts.addCourseTaken(4, 4, 60);
        hogwarts.addCourseTaken(4, 5, 72);
        hogwarts.addCourseTaken(4, 6, 90);// update
        hogwarts.addCourseTaken(4, 7, 80);
        hogwarts.addCourseTaken(4, 9, 80);
        hogwarts.addCourseTaken(4, 10, 90);
        hogwarts.addCourseTaken(4, 6, 82); // update
        hogwarts.addCourseTaken(4, 6, 82); // update
        hogwarts.addCourseTaken(4, 11, 75); 
        hogwarts.addCourseTaken(4, 11, 55); 
        hogwarts.addCourseTaken(4, 13, 82); 
        hogwarts.addCourseTaken(4, 15, 80);
        hogwarts.addCourseTaken(4, 14, 74);

        // Neville                
        hogwarts.addCourseTaken(5, 1, 68);
        hogwarts.addCourseTaken(5, 2, 72);
        hogwarts.addCourseTaken(5, 3, 65);
        hogwarts.addCourseTaken(5, 5, 98);
        hogwarts.addCourseTaken(5, 7, 75);
        hogwarts.addCourseTaken(5, 9, 78);
        hogwarts.addCourseTaken(5, 10, 70);
        hogwarts.addCourseTaken(5, 12, 91);

        // Luna 
        hogwarts.addCourseTaken(6, 1, 90);
        hogwarts.addCourseTaken(6, 2, 97);
        hogwarts.addCourseTaken(6, 4, 80);
        hogwarts.addCourseTaken(6, 3, 92);
        hogwarts.addCourseTaken(6, 8, 87);
        hogwarts.addCourseTaken(6, 6, 95);
        hogwarts.addCourseTaken(6, 9, 92);
        hogwarts.addCourseTaken(6, 10, 70);
        hogwarts.addCourseTaken(6, 12, 96);
    }
    public static void hogwartsStudentReport(College hogwarts){
        System.out.println("Getting Students Reports");
        for (int i = 1; i < 8; i++) {
            System.out.println(hogwarts.studentReport(i));
        }
        System.out.println(hogwarts.studentReport(28)); // null
        hogwarts.addStudent(28, "Cedric Diggory");
        System.out.println(hogwarts.studentReport(28));
        hogwarts.addCourseTaken(28, 7, 98);
        System.out.println(hogwarts.studentReport(28));
        hogwarts.addCourseTaken(28, 9, 96);
        System.out.println(hogwarts.studentReport(28));
        hogwarts.addCourseTaken(28, 10, 94);
        System.out.println(hogwarts.studentReport(28));
        hogwarts.addCourseTaken(28, 10, 97);
        System.out.println(hogwarts.studentReport(28));
        hogwarts.addCourse(19, "Study of Centaur Language");
        hogwarts.addCourseTaken(28, 19, 95);
        hogwarts.addCourseTaken(2, 19, 95);
        System.out.println(hogwarts.studentReport(28));
        hogwarts.removeStudent(28);
        hogwarts.removeCourse(19);
        System.out.println(hogwarts.studentReport(28));
        System.out.println(hogwarts.studentReport(2)); // should include 19

        
    }
    public static void hogwartsWhoTook(College hogwarts){
        System.out.println("Testing Students Who took");
        for (int i = 1; i < 15; i++) {
            System.out.println(hogwarts.getCourse(i));
            System.out.println(hogwarts.studentsWhoTook(hogwarts.getCourse(i)));
        }
    }
    public static void hogwartsWithGrade(College hogwarts){
        System.out.println("Testing Students with Grade");
        for (int i = 1; i < 15; i++) {
            int grade = i == 1 ? 94 : (int)(Math.random() * 20 + 75); // i == 1-> Harry , Hermione, Ron
            System.out.println(hogwarts.getCourse(i) + " Grade :" + grade); 
            System.out.println(hogwarts.studentsWithGrade(hogwarts.getCourse(i),grade)); // You should verifry with the report / student with grade method 
        }
    }
    
    public static void hogwartsCoursesAverages(College hogwarts){
        System.out.println("Testing Courses averages");
        System.out.println(hogwarts.getCoursesAverages());
        hogwarts.addStudent(28, "Cedric Diggory");
        hogwarts.addCourseTaken(28, 7, 98);
        System.out.println(hogwarts.getCoursesAverages()); // with cedric
        hogwarts.removeStudent(28);
        System.out.println(hogwarts.getCoursesAverages());// without cedric

    }
    public static void hogwartsTopPreformer(College hogwarts){
        System.out.println("Testing top preformer(s) in courses ");
        for (int i = 0; i < 15; i++) {
            System.out.println("Course " + i + ", Top preformer(s):");
            System.out.println(hogwarts.topPerfomerReport(i)); 
        }
    }
    


}







