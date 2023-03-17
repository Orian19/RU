/** Tests some of the services of the College class. */
public class CollegeDemo {
	
	public static void main(String []args) {
		
		College c = buildCollege();
		
		c.coursesList();    // Prints the courses
		System.out.println(c);
		
		c.studentsList();    // Prints the students
		
		c.removeStudent(0);  // Removes the first student in the list
		System.out.println(c);
		c.removeStudent(4);  // Removes a student somewhere inside the list
		System.out.println(c);
		c.removeStudent(5);  // Removes the last student in the list
		
		c.studentsList();    // Prints the students after the removals
		System.out.println(c);

		System.out.println(c.studentReport(1));	
		System.out.println(c.studentReport(2));
		
		System.out.println(c.studentsWhoTook(new Course(2,"Musical Theory")));
		System.out.println(c.studentsWithGrade(new Course(1,"Arranging for Songwriters"),80));	
		System.out.println(c.topPerfomerReport(1));
		System.out.println(c.getCoursesAverages());
		// System.out.println(c.getCoursesAverages(new Course(3,"Basic Ear Training")));
	}
	
	//  Builds a demo college, populated with some demo courses and students. */
	private static College buildCollege() {
		College c = new College("Berkeley School of Music");
		
		c.addCourse(1,"Arranging for Songwriters");
		c.addCourse(2,"Musical Theory");
		c.addCourse(3,"Basic Ear Training");
		c.addCourse(4,"Artist Management");
		c.addCourse(5,"Improvisation");
		
		c.addStudent(0,"Prince");
		c.addStudent(1,"Lady Gaga");
		c.addStudent(2,"Bob Dylan");
		c.addStudent(3,"Shakira");
		c.addStudent(4,"Paul McCartney");
		c.addStudent(5,"Shawn Mendes");

		c.addCourseTaken(1,1, 80);
		c.addCourseTaken(1,2, 90);
		c.addCourseTaken(1,3,100);
		c.addCourseTaken(2,1,100);
		c.addCourseTaken(2,2, 80);
		c.addCourseTaken(3,1, 70);
		c.addCourseTaken(3,3, 70); 
		return c;
	}
}