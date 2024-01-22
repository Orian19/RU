import java.util.HashMap;
import java.util.Map;

public class Code {
    public static final Map<String, String> destMap = new HashMap<>() {{
        put(null, "000");
        put("M", "001");
        put("D", "010");
        put("DM", "011");
        put("MD", "011");
        put("A", "100");
        put("AM", "101");
        put("AD", "110");
        put("ADM", "111");
        put("AMD", "111");
    }};

    public static final Map<String, String> jmpMap = new HashMap<>() {{
        put(null, "000");
        put("JGT", "001");
        put("JEQ", "010");
        put("JGE", "011");
        put("JLT", "100");
        put("JNE", "101");
        put("JLE", "110");
        put("JMP", "111");
    }};

    public static final Map<String, String> compMap = new HashMap<>() {{
        put("0", "0101010");
        put("1", "0111111");
        put("-1", "0111010");
        put("D", "0001100");
        put("A", "0110000");
        put("!D", "0001101");
        put("!A", "0110001");
        put("-D", "0001111");
        put("-A", "0110011");
        put("D+1", "0011111");
        put("A-1", "0110010");
        put("D-1", "0001110");
        put("A+1", "0110111");
        put("D+A", "0000010");
        put("D-A", "0010011");
        put("A-D", "0000111");
        put("D&A", "0000000");
        put("D|A", "0010101");
        put("M", "1110000");
        put("!M", "1110001");
        put("-M", "1110011");
        put("M+1", "1110111");
        put("M-1", "1110010");
        put("D+M", "1000010");
        put("D-M", "1010011");
        put("M-D", "1000111");
        put("D&M", "1000000");
        put("D|M", "1010101");
    }};

    public Code() {
    }

    /**
     * returns the binary code of destination instruction
     *
     * @param dest_instruction str - dest part
     * @return value
     */
    public String dest(String dest_instruction) {
        return destMap.get(dest_instruction);
    }

    /**
     * returns the binary code of comp instruction
     *
     * @param comp_instruction str - comp part
     * @return value
     */
    public String comp(String comp_instruction) {
        return compMap.get(comp_instruction);
    }

    /**
     * returns the binary code of jump instruction
     *
     * @param jump_instruction str - jmp part
     * @return value
     */
    public String jump(String jump_instruction) {
        return jmpMap.get(jump_instruction);
    }
}
