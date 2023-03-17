/** 
 * Represents a course.
 */
public class Course {

	private int cid;
	private String title;
	
	/** 
	 * Constructs a new course.
	 * @param cid   the course id
	 * @param title the course title
	 */
	public Course(int cid, String title) {
		this.cid = cid;
		this.title = title;
	}
	
	/**
	 * returns the course id
	 * @return the course id
	 */
	public int getCid() {
		return this.cid;
	}
	
	/**
	 * returns the course title
	 * @return the course title
	 */
	public String getTitle() {
		return this.title;
	}

	public boolean equals(Course other){
		if (this.cid == other.cid) {
			return true;
		}
		return false;
	}
		
	/**
	 * A textual representation of this course.
	 */
	public String toString() {
		return "Course " + this.cid + ": " + this.title;
	}
}
