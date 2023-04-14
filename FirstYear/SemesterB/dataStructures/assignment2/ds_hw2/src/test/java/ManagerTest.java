import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ManagerTest {

    @Test
    void managerTestByPriority() {
        Manager<Integer> manager = new Manager<>();

        manager.add(1);
        manager.add(2);
        manager.add(3);

        assertEquals(manager.getByPriority(), 3);
        assertEquals(manager.getByPriority(), 2);
        assertEquals(manager.getByPriority(), 1);
    }

    @Test
    void managerTestByTime() throws InterruptedException {
        Manager<Integer> manager = new Manager<>();

        manager.add(1);
        Thread.sleep(500);
        manager.add(2);
        Thread.sleep(500);
        manager.add(3);

        assertEquals(manager.getByCreationTime(), 1);
        assertEquals(manager.getByCreationTime(), 2);
        assertEquals(manager.getByCreationTime(), 3);
    }

    @Test
    void managerTestBoth() throws InterruptedException {
        Manager<Patient> manager = new Manager<>();

        Patient p1 = new Patient(1, false);
        Thread.sleep(1000);
        Patient p2 = new Patient(6,  false);
        Thread.sleep(1000);
        Patient p3 = new Patient(3,  true);
        Thread.sleep(1000);
        Patient p4 = new Patient(4,  false);

        manager.add(p1);
        manager.add(p2);
        manager.add(p3);
        manager.add(p4);

        assertEquals(manager.getByCreationTime(), p1);
        assertEquals(manager.getByPriority(), p3);
        assertEquals(manager.getByCreationTime(), p2);
        assertEquals(manager.getByPriority(), p4);
    }
}