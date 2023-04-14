public class Main {
    /**
     * a main function that creates a Manager object, and calls the
     * above functions.
     *
     * @param args .
     */
    public static void main(String[] args) throws InterruptedException {
        Manager <Patient> manager1 = new Manager<>();
        Manager <Patient> manager2 = new Manager<>();
        addPatients(10, manager1);
        addPatients(10, manager2);
        System.out.println("\nsimulateOnlyByPriority\n");
        simulateOnlyByPriority(manager1);
        System.out.println("\nsimulateOnlyByCreation\n");
        simulateOnlyByCreation(manager2); 
    }

    /**
     * a function that adds at least 5 different Patients with different
     * priorities into a Manager object it gets as input.
     * Use Thread.sleep function between your patient’s creation to make sure that
     * their enqueue times are well separated.
     *
     * @param pNum number of patients to add
     */
    public static void addPatients(int pNum, Manager <Patient> manager) throws InterruptedException {
        for (int i = 0; i < pNum; i++) {
            int rndP = (int) (Math.random() * 10);
            int rndV = (int) (Math.random() * 4);
            boolean vip = rndV > 2;
            manager.add(new Patient(rndP, vip));
            Thread.sleep(1000);
        }
    }

    /**
     * a function simulateOnlyByPriority that gets as input a Manager
     * object, removes all patients by priority and prints their details.
     *
     * @param manager object
     */
    public static void simulateOnlyByPriority(Manager <Patient> manager) {
        while (manager.getSize() > 0) {
            Patient curPatient = manager.getByPriority();
            System.out.println(curPatient);
        }
    }

    /**
     * a function simulateOnlyByCreation that gets as input a Manager
     * object, removes all patients by creation time and prints their details.
     *
     * @param manager object
     */
    public static void simulateOnlyByCreation(Manager <Patient> manager) {
        while (manager.getSize() > 0) {
            Patient curPatient = manager.getByCreationTime();
            System.out.println(curPatient);
        }
    }
}
