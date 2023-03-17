import java.util.Iterator;
import java.util.NoSuchElementException;

/** 
 * Represents a student.
 */
public class Student {
	
	private int sid;                             
	private String name;                         
	private LinkedList<CourseTaken> courseList;
	
	/** 
	 * Constructs a new student with the given sid and name, and an empty course list.
	 * @param sid  the student's id
	 * @param name the student's name
	 */
	public Student(int sid, String name) {
		this.sid = sid;
		this.name = name;
		this.courseList = new LinkedList<CourseTaken>();
	}
	
	/** 
	 * Returns the id of this student.
	 * @return the sid of this student.
	 */
	public int getSid() {
		return this.sid;
	}
	
	/** 
	 * Returns the name of this student.
	 * @return the name of this student.
	 */
	public String getName() {
		return this.name;
	}
	
	/** 
	 * Adds the given course and grade to the course list of this student.
	 * @param c     the course to add
	 * @param grade the grade in the added course 
	 */
	public void addCourse(Course c, int grade) {
		// Put your code here.
		CourseTaken courseTaken = new CourseTaken(c, grade);
		courseList.add(courseTaken);
	}
	
	/** 
	 * Returns the grade that this student got in the given course, 
	 *  or -1 if the course was not taken by this student.
	 * @param c - the returned grade will be the grade in this course.
	 * @return the grade that this student got in the given course
	 */
	public int gradeInCourse(Course c) {
		// Replace the following statement with your code.
		ListIterator<CourseTaken> itrCourses = courseList.iterator();
		CourseTaken curCourse;

		if (tookCourse(c)) {
			while (itrCourses.hasNext()) {
				curCourse = itrCourses.next();
				if (curCourse.getCourse().getCid() == c.getCid()) {
					return curCourse.getGrade();
				}
			}
		}	
		return -1;	
	}
	
	/** 
	 * Returns true if this student took the given course, false otherwise.
	 * @param c  the course we want to know whether or not the student took.
	 * @return true if this student took the given course, false otherwise.
	 */
	public boolean tookCourse(Course c) {
		if(c == null) {
			return false;
		}
		// Replace the following statement with your code.
		ListIterator<CourseTaken> itrCourses = courseList.iterator();
		CourseTaken curCourse;

		while (itrCourses.hasNext()) {
			curCourse = itrCourses.next();
			if (curCourse.getCourse().getCid() == c.getCid()) {
				return true;
			}
		}	
		return false;
	}
	
	/**
	 * Prints this student, all the courses that s/he took, and the grade point average.
	 */
	public String studentReport() {
		// Put your code here.
		ListIterator<CourseTaken> itrCourses = this.courseList.iterator();
		CourseTaken curCourse;
		int amountCourses = courseList.size();
		double gpa = 0.0;
		String coursesTakenGpa = toString();

		try {
			while (itrCourses.hasNext()) {
				curCourse = itrCourses.next();
				coursesTakenGpa += "\n" + curCourse;
				gpa += curCourse.getGrade();
			}	
			gpa /= amountCourses;
			coursesTakenGpa += "\n" + "Average:" + gpa + "\n";
			return coursesTakenGpa;
		} catch (NoSuchElementException e) {
			return "";
		}
	}
	
	public boolean equals(Student other){
		if (this.sid == other.sid) {
			return true;
		}
		return false;
	}
	/**
	 * Textual representation of this student.
	 */
	public String toString() {
		return "Student " + this.sid + ": " + this.name;
	}

	public static void main(String[] args) {
		Student student = new Student(1,"Lady Gaga");
		student.addCourse(new Course(5, "Dancing"), 100);
		student.addCourse(new Course(1, "Card Games"), 98);
		student.addCourse(new Course(2, "Psychiatry"), 60);
		System.out.println(student.studentReport());
		
		Student student1 = new Student(2,"Chandler Bing");
		student1.addCourse(new Course(6, "Data Reconfiguration"), 97);
		student1.addCourse(new Course(3, "Sarcasm 101"), 100);
		student1.addCourse(new Course(4, "Statisical Analysis"), 98);
		student1.addCourse(new Course(7, "Copywriting"), 95);
		System.out.println(student1.studentReport());
		System.out.println("DONE");
		}
}