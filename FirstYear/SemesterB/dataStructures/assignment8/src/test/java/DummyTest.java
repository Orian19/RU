import org.junit.Test;
import static org.junit.Assert.*;

public class DummyTest {
    
    @Test
    public void testSubstitute() {
        Dummy d = new Dummy("ACGT");
        d.substitute(2, 'A');
        assertEquals("ACAT", d.toString());
    }

    @Test
    public void testInsert() {
        Dummy st = new Dummy("ACGT");

        // Insert 'T' at position 0
        st.insert(0, 'T');
        assertEquals("TACGT", st.toString());

        // Insert 'G' at position 2
        st.insert(2, 'G');
        assertEquals("TAGCGT", st.toString());

        // Insert 'A' at position 5
        st.insert(5, 'A');
        assertEquals("TAGCGAT", st.toString());

        // Insert 'C' at position 7 (end of the sequence)
        st.insert(7, 'C');
        assertEquals("TAGCGATC", st.toString());

        // Test inserting a character at an invalid position
        Exception exception = assertThrows(IllegalArgumentException.class, () -> {
            st.insert(10, 'G');
        });

        String expectedMessage = "Invalid index: 10";
        String actualMessage = exception.getMessage();

        assertTrue(actualMessage.contains(expectedMessage));
    }

    @Test
    public void testDelete() {
        Dummy st = new Dummy("ACGT");

        // Delete 'A' at position 0
        st.delete(0);
        assertEquals("CGT", st.toString());

        // Delete 'C' at position 0
        st.delete(0);
        assertEquals("GT", st.toString());

        // Delete 'T' at position 1 (end of the sequence)
        st.delete(1);
        assertEquals("G", st.toString());

        // Test deleting a character at an invalid position
        Exception exception = assertThrows(IllegalArgumentException.class, () -> {
            st.delete(5);
        });

        String expectedMessage = "Invalid index: 5";
        String actualMessage = exception.getMessage();

        assertTrue(actualMessage.contains(expectedMessage));
    }


    @Test
    public void testTranslocate() {
        Dummy st = new Dummy("ACGT");

        
        st.translocate(2, 3, 0);
        assertEquals("GTAC", st.toString());

        // Test moving a subsequence to an invalid position
        Exception exception = assertThrows(IllegalArgumentException.class, () -> {
            st.translocate(0, 1, 5);
        });

        String expectedMessage = "Invalid index: 5";
        String actualMessage = exception.getMessage();

        assertTrue(actualMessage.contains(expectedMessage));
    }
    @Test
    public void testInvert() {
        Dummy st = new Dummy("ABCDEF");

        st.invert(0, 0);
        assertEquals("ABCDEF", st.toString());

        st.invert(1, 2);
        assertEquals("ACBDEF", st.toString());

        st.invert(3, 5);
        assertEquals("ACBFED", st.toString());

        st.invert(0, 5);
        assertEquals("DEFBCA", st.toString());
    }

    @Test
    public void testToString() {
        Dummy st = new Dummy("ABCDEF");

        // Checking initial string
        assertEquals("ABCDEF", st.toString());

        // After some operations
        st.translocate(1, 2, 4); // ABCDEF -> ADBCEF
        assertEquals("ADBCEF", st.toString());

        st.insert(3, 'Z'); // ADBCEF -> ADBCZEF
        assertEquals("ADBZCEF", st.toString());

        st.delete(4); // ADBCZEF -> ADBCEF
        assertEquals("ADBZEF", st.toString());
    }



}
