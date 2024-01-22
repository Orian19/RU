import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Parser {

    private BufferedReader bufferedReader;
    private String instruction;
    public static final String A_INSTRUCTION = "A_INSTRUCTION";
    public static final String C_INSTRUCTION = "C_INSTRUCTION";
    public static final String L_INSTRUCTION = "L_INSTRUCTION";

    /**
     * constructor
     *
     * @return
     */
    public Parser(String file) {
        try {
            FileReader reader = new FileReader(file);
            this.bufferedReader = new BufferedReader(reader);
            this.instruction = this.bufferedReader.readLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * checks if there exists more instruction
     *
     * @return true if there is more instruction
     */
    public boolean hasMoreLines() throws IOException {
        return this.instruction != null;
    }

    /**
     * advances to the next instruction
     *
     */
    public void advance() throws IOException {
        this.instruction = this.bufferedReader.readLine();
        if (this.instruction != null) {
            this.instruction = this.instruction.trim();
        }
    }

    /**
     * gets the symobol of the current instruction
     *
     * @return symbol
     */
    public String instructionType() {
        boolean isC = false;
        for (int i = 0; i < this.instruction.length(); i++) {
            if (this.instruction.charAt(i) == '=' || this.instruction.charAt(i) == ';') {
                isC = true;
                break;
            }
        }
        if (this.instruction.charAt(0) == '@') {
            return A_INSTRUCTION;
        } else if (isC) {
            return C_INSTRUCTION;
        } else if (this.instruction.charAt(0) == '(') {
            return L_INSTRUCTION;
        }
        return "";
    }

    /**
     * gets the symobol of the current instruction
     *
     * @return symbol
     */
    public String symbol() {
        String type = instructionType();
        String symbol = null;

        switch (type) {
            case A_INSTRUCTION:
                symbol = this.instruction.substring(1);
                break;
            case L_INSTRUCTION:
                symbol = this.instruction.substring(1, this.instruction.length() - 1);
            default:
                break;
        }
        return symbol;
    }

    /**
     * gets the "dest" part of the instruction
     *
     * @return string of dest
     */
    public String dest() {
        String dest = null;
        if (instructionType().equals(C_INSTRUCTION)) {
            if (this.instruction.contains("=")) {
                dest = this.instruction.substring(0, this.instruction.indexOf('='));
            }
        }
        return dest;
    }

    /**
     * gets the "comp" part of the instruction
     *
     * @return string of comp
     */
    public String comp() {
        String comp = null;
        if (instructionType().equals(C_INSTRUCTION)) {
            if (this.instruction.indexOf(';') == -1) {
                comp = this.instruction.substring(this.instruction.indexOf('=') + 1);
            } else {
                comp = this.instruction.substring(this.instruction.indexOf('=') + 1, this.instruction.indexOf(';'));
            }
        }
        return comp;
    }

    /**
     * gets the "jmp" part of the instruction
     *
     * @return type of jmp
     */
    public String jmp() {
        String jmp = null;
        if (instructionType().equals(C_INSTRUCTION)) {
            if (this.instruction.indexOf(';') != -1) {
                jmp = this.instruction.substring(this.instruction.indexOf(';') + 1);
            }
        }
        return jmp;
    }

    /**
     * closes the current file
     *
     * @throws IOException
     */
    public void closeFile() throws IOException {
        this.bufferedReader.close();
    }

    /**
     * get current instruction
     *
     * @return current instruction
     */
    public String getInstruction() {
        return this.instruction;
    }
}
