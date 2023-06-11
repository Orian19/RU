import org.junit.Test;
import static org.junit.Assert.*;

public class SplayTreeTest {
    
    @Test
    public void testSubstitute() {
        SplayTree splayTree = new SplayTree("ACGT");
        splayTree.substitute(2, 'A');
        assertEquals("ACAT", splayTree.toString(splayTree.root));
    }

    @Test
    public void testInsert() {
        SplayTree st = new SplayTree("ACGT");

        // Insert 'T' at position 0
        st.insert(0, 'T');
        assertEquals("TACGT", st.toString(st.root));

        // Insert 'G' at position 2
        st.insert(2, 'G');
        assertEquals("TAGCGT", st.toString(st.root));

        // Insert 'A' at position 5
        st.insert(5, 'A');
        assertEquals("TAGCGAT", st.toString(st.root));

        // Insert 'C' at position 7 (end of the sequence)
        st.insert(7, 'C');
        assertEquals("TAGCGATC", st.toString(st.root));

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
        SplayTree st = new SplayTree("ACGT");

        // Delete 'A' at position 0
        st.delete(0);
        assertEquals("CGT", st.toString(st.root));

        // Delete 'C' at position 0
        st.delete(0);
        assertEquals("GT", st.toString(st.root));

        // Delete 'T' at position 1 (end of the sequence)
        st.delete(1);
        assertEquals("G", st.toString(st.root));

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
        SplayTree st = new SplayTree("ACGT");

        
        st.translocate(2, 3, 0);
        assertEquals("GTAC", st.toString(st.root));

        // Test moving a subsequence to an invalid position
        Exception exception = assertThrows(IllegalArgumentException.class, () -> {
            st.translocate(0, 1, 5);
        });

        String expectedMessage = "Invalid index: 5";
        String actualMessage = exception.getMessage();

        assertTrue(actualMessage.contains(expectedMessage));
    }


    @Test
    public void testToString() {
        SplayTree st = new SplayTree("ABCDEF");

        // Checking initial string
        assertEquals("ABCDEF", st.toString(st.root));

        // After some operations
        st.translocate(1, 2, 4); // ABCDEF -> ADBCEF
        assertEquals("ADBCEF", st.toString(st.root));

        st.insert(3, 'Z'); // ADBCEF -> ADBCZEF
        assertEquals("ADBZCEF", st.toString(st.root));

        st.delete(4); // ADBCZEF -> ADBCEF
        assertEquals("ADBZEF", st.toString(st.root));
    }



}
