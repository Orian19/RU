
/**
 * Represents a college, and college management operations.
 * A college has courses, and students. Students take courses and get grades.
 * (See the Course, Student, and CourseTaken classes for more details).
 */
public class College {
	
	private String name; // the name of this college
	private LinkedList<Course> courses;
	private LinkedList<Student> students;
	
	/**
	 * Constructs a new college, with empty student and course lists.
	 */
	public College(String name) {
		this.name = name;
		this.courses = new LinkedList<Course>();
		this.students = new LinkedList<Student>();
	}
	
	/** 
	 * Adds the given course to the course list of this college.
	 * @param cid   course id.
	 * @param title course title.
	 */
	public void addCourse(int cid, String title) {
		// Put your code here
		if (this.getCourse(cid) == null) {
			Course course = new Course(cid, title);
			courses.add(course);
		}
	}
	
	/**
	 * Returns a list of all the courses.
	 */
	public LinkedList<Course> coursesList() {
		return this.courses;
	}

	/** 
	 * If the given course is in this college, removes it and returns true.
	 * Otherwise returns false.
	 * @param  cid the course to remove.
	 * @return True if the course was removed, false if there is no such course. 
	 */
	public boolean removeCourse(int cid) {
		// Replace the return statement with your code.
		// Note: You can get the course with the given cid by calling getCourse
		if (courses.remove(getCourse(cid))) {
			return true;
		}
		return false;
	}
	
	// Returns the course that has the given id, or null if there is no such course.
	public Course getCourse(int cid) {
		// Replace the return statement with your code.
		ListIterator<Course> itr = courses.iterator();
		Course curCourse;

		while (itr.hasNext()) {
			curCourse = itr.next();
			if (curCourse.getCid() == cid) {
				return curCourse;
			}
		}
		return null;
	}
	
	/** 
	 * Adds the given student to the students list of this college.
	 * @param sid   student id
	 * @param name  student name
	 */
	public void addStudent(int sid, String name) {
		// Put your code here
		if (this.getStudent(sid) == null) {
			Student student = new Student(sid, name);
			students.add(student);
		}
	}
	
	/**
	 * Returns a list of all the students.
	 */
	public LinkedList<Student> studentsList() {
		return students;
	}
	
	/** 
	 * If the given student is in this college, removes it and returns true.
	 * Otherwise returns false.
	 * @param  sid  the student's id.
	 * @return True if the student was removed, false if there is no such student. 
	 */
	public boolean removeStudent(int sid) {
		// Replace the return statement with your code.
		// Note: You can get the student with the given sid by calling the getStudent method.
		if (students.remove(getStudent(sid))) {
			return true;
		}
		return false;
	}
	
	// Returns the student that has the given id, or null if there is no such student.
	public Student getStudent(int sid) {
		// Replace the return statement with your code.
		ListIterator<Student> itr = students.iterator();
		Student curStudent;

		while (itr.hasNext()) {
			curStudent = itr.next();
			if (curStudent.getSid() == sid) {
				return curStudent;
			}
		}
		return null;
	}
	
	/** 
	 * Adds the given course to the course list of the given student.
	 * @param sid   student id
	 * @param cid   course id
	 * @param grade student's grade in the course 
	 */
	public void addCourseTaken(int sid, int cid, int grade) {
		// Put your code here.
		Course course = getCourse(cid);
		if(course != null) {
			getStudent(sid).addCourse(course, grade);
		}
	}
	
	/** 
	 * Returns the student report of the given student.
	 * See the Student class for more details.
	 * @param sid  student id
	 */
	public String studentReport(int sid) {
		// Put your code here
		// Your code should call the student's studentReport method
		if (this.getStudent(sid) != null) {
			return getStudent(sid).studentReport();
		}
		return "null";
	}
	
	// Returns a list of all the students who took the given course
	public LinkedList<Student> studentsWhoTook(Course c) {
		// replace the following statement with your code.
		LinkedList<Student> cStudentList = new LinkedList<Student>();
		ListIterator<Student> studentsItr = students.iterator();
		Student curStudent;
		
		while (studentsItr.hasNext()) {
			curStudent = studentsItr.next();
			if (curStudent.tookCourse(c)) {
				cStudentList.add(curStudent);
			}
		}
		return cStudentList;
	}
	
	// Returns a list of the all the student got the given grade or above it 
	public LinkedList<Student> studentsWithGrade(Course c, int grade) {
		// replace the following statement with your code.
		LinkedList<Student> cStudentList = new LinkedList<Student>();
		ListIterator<Student> studentsItr = students.iterator();
		Student curStudent;
		
		while (studentsItr.hasNext()) {
			curStudent = studentsItr.next();
			if (curStudent.gradeInCourse(c) >= grade) {
				cStudentList.add(curStudent);
			}
		}
		return cStudentList;
	}

	// Returns a list of courses with the grade averages of all the students who took it
	public LinkedList<CourseTaken> getCoursesAverages() {
		// replace the following statement with your code.
		LinkedList<CourseTaken> courseListGpa = new LinkedList<CourseTaken>();
		ListIterator<Course> coursesItr = courses.iterator();
		Course curCourse;

		while (coursesItr.hasNext()) {
			curCourse = coursesItr.next();
			double gpaCourse = 0.0;
			int tookCourse = 0;

			ListIterator<Student> studentsItr = students.iterator();
			Student curStudent;
			while (studentsItr.hasNext()) {
				curStudent = studentsItr.next();
				if (curStudent.tookCourse(curCourse)) {
					gpaCourse += curStudent.gradeInCourse(curCourse);
					tookCourse++;
				}
			}
			gpaCourse /= tookCourse;
			courseListGpa.add(new CourseTaken(curCourse, (int) gpaCourse));
		}
		return courseListGpa;
	}
	/** 
	 * Returns the students with the highest grade in the given course.
	 * @param cid  course id
	 */
	public LinkedList<Student> topPerfomerReport(int cid) {
		// Put your code here
		LinkedList<Student> cStudentList = new LinkedList<Student>();
		ListIterator<Student> studentsItr = students.iterator();
		Student curStudent;
		int highestGrade = 0;	
		
		while (studentsItr.hasNext()) {
			curStudent = studentsItr.next();
			int curStudentGrade = curStudent.gradeInCourse(getCourse(cid));
			if (curStudentGrade >= highestGrade) {
				cStudentList.add(curStudent);
				highestGrade = curStudentGrade;
			}
		}
		return cStudentList;
	}

	/** 
	 * Returns the college name
	 * @return the college name 
	 */
	public String getName() {
		return this.name;
	}
	
	/**
	 * A textual representation of this college, along with its courses and students.
	 */
	public String toString() {
		String str = name + "\n";
		str += "courses:" + "\n";
		str += courses.toString() + "\n";
		str += "students:" + "\n";
		str += students.toString() + "\n";
		return str;
	}
}
