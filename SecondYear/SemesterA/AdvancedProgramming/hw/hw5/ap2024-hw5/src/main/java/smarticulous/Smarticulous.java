package smarticulous;

import smarticulous.db.Exercise;
import smarticulous.db.Submission;
import smarticulous.db.User;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * The Smarticulous class, implementing a grading system.
 */
public class Smarticulous {

    /**
     * The connection to the underlying DB.
     * <p>
     * null if the db has not yet been opened.
     */
    Connection db;

    /**
     * Open the {@link Smarticulous} SQLite database.
     * <p>
     * This should open the database, creating a new one if necessary, and set the {@link #db} field
     * to the new connection.
     * <p>
     * The open method should make sure the database contains the following tables, creating them if necessary:
     *
     * <table>
     *   <caption><em>Table name: <strong>User</strong></em></caption>
     *   <tr><th>Column</th><th>Type</th></tr>
     *   <tr><td>UserId</td><td>Integer (Primary Key)</td></tr>
     *   <tr><td>Username</td><td>Text</td></tr>
     *   <tr><td>Firstname</td><td>Text</td></tr>
     *   <tr><td>Lastname</td><td>Text</td></tr>
     *   <tr><td>Password</td><td>Text</td></tr>
     * </table>
     *
     * <p>
     * <table>
     *   <caption><em>Table name: <strong>Exercise</strong></em></caption>
     *   <tr><th>Column</th><th>Type</th></tr>
     *   <tr><td>ExerciseId</td><td>Integer (Primary Key)</td></tr>
     *   <tr><td>Name</td><td>Text</td></tr>
     *   <tr><td>DueDate</td><td>Integer</td></tr>
     * </table>
     *
     * <p>
     * <table>
     *   <caption><em>Table name: <strong>Question</strong></em></caption>
     *   <tr><th>Column</th><th>Type</th></tr>
     *   <tr><td>ExerciseId</td><td>Integer</td></tr>
     *   <tr><td>QuestionId</td><td>Integer</td></tr>
     *   <tr><td>Name</td><td>Text</td></tr>
     *   <tr><td>Desc</td><td>Text</td></tr>
     *   <tr><td>Points</td><td>Integer</td></tr>
     * </table>
     * In this table the combination of ExerciseId and QuestionId together comprise the primary key.
     *
     * <p>
     * <table>
     *   <caption><em>Table name: <strong>Submission</strong></em></caption>
     *   <tr><th>Column</th><th>Type</th></tr>
     *   <tr><td>SubmissionId</td><td>Integer (Primary Key)</td></tr>
     *   <tr><td>UserId</td><td>Integer</td></tr>
     *   <tr><td>ExerciseId</td><td>Integer</td></tr>
     *   <tr><td>SubmissionTime</td><td>Integer</td></tr>
     * </table>
     *
     * <p>
     * <table>
     *   <caption><em>Table name: <strong>QuestionGrade</strong></em></caption>
     *   <tr><th>Column</th><th>Type</th></tr>
     *   <tr><td>SubmissionId</td><td>Integer</td></tr>
     *   <tr><td>QuestionId</td><td>Integer</td></tr>
     *   <tr><td>Grade</td><td>Real</td></tr>
     * </table>
     * In this table the combination of SubmissionId and QuestionId together comprise the primary key.
     *
     * @param dburl The JDBC url of the database to open (will be of the form "jdbc:sqlite:...")
     * @return the new connection
     * @throws SQLException
     */
    public Connection openDB(String dburl) throws SQLException {
        this.db = DriverManager.getConnection(dburl); // getting connection

        //// creating sql tables
        String user_table = "CREATE TABLE IF NOT EXISTS User (" +
                "UserId INTEGER PRIMARY KEY," +
                "Username TEXT UNIQUE," +
                "Firstname TEXT," +
                "Lastname TEXT," +
                "Password TEXT" +
                ");";

        String exercise_table = "CREATE TABLE IF NOT EXISTS Exercise (" +
                "ExerciseId INTEGER PRIMARY KEY," +
                "Name TEXT," +
                "DueDate INTEGER" +
                ");";

        // In this table the combination of ExerciseId and QuestionId together comprise the primary key.
        String question_table = "CREATE TABLE IF NOT EXISTS Question (" +
                "ExerciseId INTEGER," +
                "QuestionId INTEGER," +
                "Name TEXT," +
                "Desc TEXT," +
                "Points INTEGER," +
                "PRIMARY KEY (ExerciseId, QuestionId)" +
                ");";

        String submission_table = "CREATE TABLE IF NOT EXISTS Submission (" +
                "SubmissionId INTEGER PRIMARY KEY," +
                "UserId INTEGER," +
                "ExerciseId INTEGER," +
                "SubmissionTime INTEGER" +
                ");";

        // In this table the combination of SubmissionId and QuestionId together comprise the primary key.
        String questionGrade_table = "CREATE TABLE IF NOT EXISTS QuestionGrade (" +
                "SubmissionId INTEGER," +
                "QuestionId INTEGER," +
                "Grade REAL," +
                "PRIMARY KEY (QuestionId, SubmissionId)" +
                ");";

        // updating the actual tables
        Statement st = this.db.createStatement();
        st.executeUpdate(user_table);
        st.executeUpdate(exercise_table);
        st.executeUpdate(question_table);
        st.executeUpdate(submission_table);
        st.executeUpdate(questionGrade_table);
        st.close(); // closing the statement

        return this.db;
    }


    /**
     * Close the DB if it is open.
     *
     * @throws SQLException
     */
    public void closeDB() throws SQLException {
        if (db != null) {
            db.close();
            db = null;
        }
    }

    // =========== User Management =============

    /**
     * Add a user to the database / modify an existing user.
     * <p>
     * Add the user to the database if they don't exist. If a user with user.username does exist,
     * update their password and firstname/lastname in the database.
     *
     * @param user
     * @param password
     * @return the userid.
     * @throws SQLException
     */
    public int addOrUpdateUser(User user, String password) throws SQLException {
        // parametrized query statement that adds or modifies an existing user
        PreparedStatement st = this.db.prepareStatement("INSERT OR REPLACE INTO User (" +
                "Username, Firstname, Lastname, Password) VALUES (?, ?, ?, ?);");

        // updating the relevant fields from User
        st.setString(1, user.username);
        st.setString(2, user.firstname);
        st.setString(3, user.lastname);
        st.setString(4, password);

        st.executeUpdate(); // performing the actual updates
        st.close(); // closing the statement

        // parametrized query to get the userID of the given user
        st = this.db.prepareStatement("SELECT UserId FROM User WHERE Username = ?;");
        st.setString(1, user.username);
        // extracting the actual result
        ResultSet resultSet = st.executeQuery();
        int user_id = resultSet.getInt("UserId");

        resultSet.close();
        st.close();

        // returning the user's id from the user table
        return user_id;
    }


    /**
     * Verify a user's login credentials.
     *
     * @param username
     * @param password
     * @return true if the user exists in the database and the password matches; false otherwise.
     * @throws SQLException <p>
     *                      Note: this is totally insecure. For real-life password checking, it's important to store only
     *                      a password hash
     * @see <a href="https://crackstation.net/hashing-security.htm">How to Hash Passwords Properly</a>
     */
    public boolean verifyLogin(String username, String password) throws SQLException {
        PreparedStatement st = this.db.prepareStatement("SELECT Password FROM User WHERE Username = ?;");
        st.setString(1, username);

        try {
            ResultSet resultSet = st.executeQuery();
            String real_password = resultSet.getString("Password");
            resultSet.close();
            return password.equals(real_password);
        } catch (SQLException e) {
            return false;
        } finally {
            st.close();
        }
    }

    // =========== Exercise Management =============

    /**
     * Add an exercise to the database.
     *
     * @param exercise
     * @return the new exercise id, or -1 if an exercise with this id already existed in the database.
     * @throws SQLException
     */
    public int addExercise(Exercise exercise) throws SQLException {
        //// handling exercise table
        // parametrized query statement that adds an exercise
        PreparedStatement st = this.db.prepareStatement("INSERT INTO Exercise (" +
                "ExerciseId, Name, DueDate) VALUES (?, ?, ?);");

        // updating the relevant fields from Exercise
        st.setInt(1, exercise.id);
        st.setString(2, exercise.name);
        st.setLong(3, exercise.dueDate.getTime()); // date in milliseconds

        st.executeUpdate(); // performing the actual updates
        st.close(); // closing the statement

        //// handling questions table
        // parametrized query statement that adds questions
        st = this.db.prepareStatement("INSERT INTO Question (" +
                "ExerciseId, Name, Desc, Points) VALUES (?, ?, ?, ?);");

        // updating the relevant fields from Question
        for (int i = 0; i < exercise.questions.size(); i++) {
            st.setInt(1, exercise.id);
            st.setString(2, exercise.questions.get(i).name);
            st.setString(3, exercise.questions.get(i).desc);
            st.setInt(4, exercise.questions.get(i).points);

            st.executeUpdate(); // performing the actual updates
        }
        st.close(); // closing the statement

        // getting exercise id
        st = this.db.prepareStatement("SELECT ExerciseId FROM Exercise WHERE ExerciseId = ?;");
        st.setInt(1, exercise.id);

        ResultSet resultSet = st.executeQuery();
        int exercise_id = -1;
        if (resultSet.next())
            exercise_id = resultSet.getInt("ExerciseId");

        // closing connections
        resultSet.close();
        st.close();

        return exercise_id;
    }

    /**
     * helper method to load all the questions for a specific exercise from the table
     *
     * @param ex specific exercise
     * @throws SQLException
     */
    private void loadQuestions(Exercise ex) throws SQLException {
        // getting all the questions from the db by the exerciseId
        PreparedStatement st = this.db.prepareStatement("SELECT * FROM Question WHERE ExerciseId = ?;");
        st.setInt(1, ex.id);
        ResultSet resultSet = st.executeQuery();

        String curName;
        String cueDesc;
        int curPoints;
        while (resultSet.next()) {
            // getting all the fields of a question from the table
            curName = resultSet.getString("Name");
            cueDesc = resultSet.getString("Desc");
            curPoints = resultSet.getInt("Points");

            // adding a question from the table to the current exercise
            ex.addQuestion(curName, cueDesc, curPoints);
        }
        resultSet.close();
        st.close();
    }

    /**
     * Return a list of all the exercises in the database.
     * <p>
     * The list should be sorted by exercise id.
     *
     * @return list of all exercises.
     * @throws SQLException
     */
    public List<Exercise> loadExercises() throws SQLException {
        // getting all the exercises from the db sorted by their id
        PreparedStatement st = this.db.prepareStatement("SELECT * FROM Exercise ORDER BY ExerciseId;");
        ResultSet resultSet = st.executeQuery();

        List<Exercise> exercisesList = new ArrayList<>();
        Exercise curExercise;
        int curId;
        String curName;
        Date curDueDate;
        while (resultSet.next()) {
            // getting all the fields of an exercise from the table
            curId = resultSet.getInt("ExerciseId");
            curName = resultSet.getString("Name");
            curDueDate = new Date(resultSet.getLong("DueDate"));

            // updating the list of exercises with data for the table
            curExercise = new Exercise(curId, curName, curDueDate);
            loadQuestions(curExercise);
            exercisesList.add(curExercise);
        }
        resultSet.close();
        st.close();

        return exercisesList;
    }

    /**
     * helper method to update all questionGrades for a given submission
     *
     * @param sub submission
     * @throws SQLException
     */
    private void updateQuestionGrades(Submission sub) throws SQLException {
        // updating all the questionGrades from the db by the submissionId
        PreparedStatement st = this.db.prepareStatement("INSERT INTO QuestionGrade " +
                "(SubmissionId, Grade) VALUES (?, ?);");

        float curGrade;
        for (int i = 0; i < sub.exercise.questions.size(); i++) {
            st.setInt(1, sub.id);

            // decoding the question grade (actual grade received for the question / points the current question is worth)
            curGrade = sub.questionGrades[i] / sub.exercise.questions.get(i).points;
            st.setFloat(2, curGrade);

            st.executeUpdate();
        }
        st.close();
    }

    // ========== Submission Storage ===============

    /**
     * Store a submission in the database.
     * The id field of the submission will be ignored if it is -1.
     * <p>
     * Return -1 if the corresponding user doesn't exist in the database.
     *
     * @param submission
     * @return the submission id.
     * @throws SQLException
     */
    public int storeSubmission(Submission submission) throws SQLException {
        // getting the userId of the user submitting
        PreparedStatement st = this.db.prepareStatement("SELECT UserId FROM User WHERE Username = ?;");
        st.setString(1, submission.user.username);

        ResultSet resultSet = st.executeQuery();
        int user_id = -1;
        if (resultSet.next())
            user_id = resultSet.getInt("UserId");
        else
            return user_id;  // returning -1 if the corresponding user doesn't exist in the db

        // storing the given submission in the db
        st = this.db.prepareStatement("INSERT INTO Submission" +
                "(SubmissionId, UserId, ExerciseId, SubmissionTime) VALUES  (?, ?, ?, ?);");

        st.setInt(1, submission.id);
        st.setInt(2, user_id);
        st.setInt(3, submission.exercise.id);
        st.setLong(4, submission.submissionTime.getTime());

        // storing a submission including updating question grades
        st.executeUpdate();
        updateQuestionGrades(submission);

        return submission.id;
    }


    // ============= Submission Query ===============


    /**
     * Return a prepared SQL statement that, when executed, will
     * return one row for every question of the latest submission for the given exercise by the given user.
     * <p>
     * The rows should be sorted by QuestionId, and each row should contain:
     * - A column named "SubmissionId" with the submission id.
     * - A column named "QuestionId" with the question id,
     * - A column named "Grade" with the grade for that question.
     * - A column named "SubmissionTime" with the time of submission.
     * <p>
     * Parameter 1 of the prepared statement will be set to the User's username, Parameter 2 to the Exercise Id, and
     * Parameter 3 to the number of questions in the given exercise.
     * <p>
     * This will be used by {@link #getLastSubmission(User, Exercise)}
     *
     * @return
     */
    PreparedStatement getLastSubmissionGradesStatement() throws SQLException {
        //// SQL statement breakdown:
        // 1. selecting the fields for the rows we want to output
        // 2. inner joining on the submission table to get only the matching lines for userId
        // 3. inner joining on question (at this stage we have both user and submission data) to get exercise
        // 4. inner joining on questionGrade (at this stage-user,submission,question) to get grade
        // 5. we select the user and exercise by the username and exerciseId supplied
        // 6. grouping the data by the questions with the latest submissionTime (keeping only rows with latest submission)
        // 7. finally, ordering the rows of questions resulted by their id given the number of question for the exercise
        return this.db.prepareStatement(
                "SELECT Submission.SubmissionId, Question.QuestionId, QuestionGrade.Grade, Submission.SubmissionTime FROM User " +
                        "INNER JOIN Submission ON User.UserId = Submission.UserId " +
                        "INNER JOIN Question ON Question.ExerciseId = Submission.ExerciseId " +
                        "INNER JOIN QuestionGrade ON (Question.QuestionId = QuestionGrade.QuestionId AND Submission.SubmissionId = QuestionGrade.SubmissionId) " +
                        "WHERE User.Username = ? AND Submission.ExerciseId = ? " +
                        "GROUP BY QuestionGrade.QuestionId HAVING MAX(Submission.SubmissionTime) " +
                        "ORDER BY Question.QuestionId LIMIT ? " +
                        ";");
    }

    /**
     * Return a prepared SQL statement that, when executed, will
     * return one row for every question of the <i>best</i> submission for the given exercise by the given user.
     * The best submission is the one whose point total is maximal.
     * <p>
     * The rows should be sorted by QuestionId, and each row should contain:
     * - A column named "SubmissionId" with the submission id.
     * - A column named "QuestionId" with the question id,
     * - A column named "Grade" with the grade for that question.
     * - A column named "SubmissionTime" with the time of submission.
     * <p>
     * Parameter 1 of the prepared statement will be set to the User's username, Parameter 2 to the Exercise Id, and
     * Parameter 3 to the number of questions in the given exercise.
     * <p>
     * This will be used by {@link #getBestSubmission(User, Exercise)}
     */
    PreparedStatement getBestSubmissionGradesStatement() throws SQLException {
        // TODO: Implement
        return null;
    }

    /**
     * Return a submission for the given exercise by the given user that satisfies
     * some condition (as defined by an SQL prepared statement).
     * <p>
     * The prepared statement should accept the user name as parameter 1, the exercise id as parameter 2 and a limit on the
     * number of rows returned as parameter 3, and return a row for each question corresponding to the submission, sorted by questionId.
     * <p>
     * Return null if the user has not submitted the exercise (or is not in the database).
     *
     * @param user
     * @param exercise
     * @param stmt
     * @return
     * @throws SQLException
     */
    Submission getSubmission(User user, Exercise exercise, PreparedStatement stmt) throws SQLException {
        stmt.setString(1, user.username);
        stmt.setInt(2, exercise.id);
        stmt.setInt(3, exercise.questions.size());

        ResultSet res = stmt.executeQuery();

        boolean hasNext = res.next();
        if (!hasNext)
            return null;

        int sid = res.getInt("SubmissionId");
        Date submissionTime = new Date(res.getLong("SubmissionTime"));

        float[] grades = new float[exercise.questions.size()];

        for (int i = 0; hasNext; ++i, hasNext = res.next()) {
            grades[i] = res.getFloat("Grade");
        }

        return new Submission(sid, user, exercise, submissionTime, (float[]) grades);
    }

    /**
     * Return the latest submission for the given exercise by the given user.
     * <p>
     * Return null if the user has not submitted the exercise (or is not in the database).
     *
     * @param user
     * @param exercise
     * @return
     * @throws SQLException
     */
    public Submission getLastSubmission(User user, Exercise exercise) throws SQLException {
        return getSubmission(user, exercise, getLastSubmissionGradesStatement());
    }


    /**
     * Return the submission with the highest total grade
     *
     * @param user     the user for which we retrieve the best submission
     * @param exercise the exercise for which we retrieve the best submission
     * @return
     * @throws SQLException
     */
    public Submission getBestSubmission(User user, Exercise exercise) throws SQLException {
        return getSubmission(user, exercise, getBestSubmissionGradesStatement());
    }
}
