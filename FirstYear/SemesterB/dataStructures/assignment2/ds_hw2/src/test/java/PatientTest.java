
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class PatientTest {

    @Test
    void getPriority() {
        Patient p = new Patient(10, true);
        assertEquals(p.getPriority(), 10);
    }

    @Test
    void isVip() {
        Patient p = new Patient(5, true);
        assertEquals(p.isVip(), true);
    }

    @Test
    void isEqual() {
        Patient p = new Patient(5, true);
        Patient p2 = new Patient(5, true);
        assertEquals(p.equals(p2), false);
    }

    @Test
    void compareTo() {
        Patient p = new Patient(5, true);
        Patient p2 = new Patient(5, false);
        assertEquals(p.compareTo(p2), 1);

        p = new Patient(5, false);
        p2 = new Patient(5, false);
        assertEquals(p.compareTo(p2), 0);

        p = new Patient(6, false);
        p2 = new Patient(5, false);
        assertEquals(p.compareTo(p2), 1);
    }
}