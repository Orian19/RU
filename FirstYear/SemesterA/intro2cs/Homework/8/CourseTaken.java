/**
 * Represents pairs of Course object and grade.
 */
public class CourseTaken {

	private Course course;
	private int grade;
	
	/**
	 * constructs a new CourseTaken instance.
	 * @param c     the taken course
	 * @param grade the grade in the course
	 */
	public CourseTaken(Course c, int grade){
		this.course = c;
		this.grade = grade;
	}
	
	/**
	 * returns the Course object of this CourseTaken instance.
	 * @return the Course object of this CourseTaken instance.
	 */
	public Course getCourse() {
		return this.course;
	}
	
	/**
	 * returns the grade this CourseTaken instance.
	 * @return the grade of this CourseTaken instance.
	 */
	public int getGrade() {
		return this.grade;
	}

	/**
	 * returns the grade this CourseTaken instance.
	 * @return the grade of this CourseTaken instance.
	 */
	public boolean equals(Course other) {
		if (this.course.getCid() == other.getCid()) {
			return true;
		}
		return false;
	}

	/**
	 * A textual representation of this CourseTaken instance.
	 */
	public String toString(){
		return course.toString() + ", grade: " + grade;
	}
}
