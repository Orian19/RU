import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.time.LocalTime;

/**
 * The Patient class holds all the relevant information for
 * a patient: id, queueTime, priority, isVip.
 *
 * This class implements its own compareTo and equals methods.
 *
 * This class will be used in your future implementation
 * instead of T in all the other generics classes.
 */
public class Patient implements Comparable<Patient>{
    final static int randomStringLength = 32;
    private int priority;
    private boolean vip;
    private String id;
    private LocalTime createdTime;

    /**
     * A standard constructor for the Patient class
     *
     * @param priority - the priority of the patient
     * @param vip - is the patient is vip
     */
    public Patient(int priority, boolean vip){
        this.priority = priority;
        this.vip = vip;
        this.id = generateRandomString(randomStringLength);
        this.setcreatedTime(LocalTime.now());
    }

    public LocalTime getcreatedTime(){
        return this.createdTime;
    }

    public void setcreatedTime(LocalTime createdTime){
        this.createdTime = createdTime;
    }

    public int getPriority(){
        return this.priority;
    }

    public boolean isVip(){
        return this.vip;
    }

    public String getId(){
        return this.id;
    }

    private String generateRandomString(int length) {
        String CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        SecureRandom RANDOM = new SecureRandom();
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append(CHARACTERS.charAt(RANDOM.nextInt(CHARACTERS.length())));
        }
        return sb.toString();
    }

    /**
     * An override for compareTo function.
     * the order relation is first determined by vip and then by priority.
     *
     * @param p - the patient to be compared with
     * @return int - 0 / >0 / <0
     */
    @Override
    public int compareTo(Patient p) {
        if(p == null) return 1;
        if(p.isVip() && !this.vip) return -1;
        if(this.vip && !p.isVip()) return 1;
        return this.getPriority() - p.getPriority();
    }

    /**
     * An override for compareTo equals.
     * Two patients are equal if and only if all their data
     * members are equal.
     *
     * @param o - other object to be compared with
     * @return boolean - true in case of equality
     */
    @Override
    public boolean equals(Object o) {
        if(!(o instanceof Patient)){
            return false;
        }
        if(o == this){
            return true;
        }
        Patient p = (Patient) o;
        if(p.getcreatedTime() != this.getcreatedTime())return false;
        if(p.getPriority() != this.getPriority()) return false;
        if(p.getId() != this.getId()) return false;
        if(p.isVip() != this.isVip()) return false;
        return true;
    }


    @Override
    public String toString() {
        return "\npriority: " + this.priority + "\nvip: " + this.vip + "\nid: " +
                this.id + "\ncreated time: " + this.createdTime;
    }
}

