import java.util.HashMap;
import java.util.Map;

public class SymbolTable {
    private Map<String, Integer> symbTable;

    public SymbolTable() {
        this.symbTable = new HashMap<>() {{
            put("R0", 0);
            put("R1", 1);
            put("R2", 2);
            put("R3", 3);
            put("R4", 4);
            put("R5", 5);
            put("R6", 6);
            put("R7", 7);
            put("R8", 8);
            put("R9", 9);
            put("R10", 10);
            put("R11", 11);
            put("R12", 12);
            put("R13", 13);
            put("R14", 14);
            put("R15", 15);
            put("SCREEN", 16384);
            put("KBD", 24576);
            put("SP", 0);
            put("LCL", 1);
            put("ARG", 2);
            put("THIS", 3);
            put("THAT", 4);
        }};
    }

    /**
     * adds a new symbol to the symbols table
     *
     * @param symbol string of new symbol
     * @param address address to save the symbol
     */
    public void addEntery(String symbol, int address) {
        this.symbTable.put(symbol, address);
    }

    /**
     * checks if a given symbol exists in the table
     *
     * @param symbol string of a symbol
     */
    public boolean contains(String symbol) {
        return this.symbTable.containsKey(symbol);
    }

    /**
     * gets the address of a given symbol
     *
     * @param symbol existing symbol
     */
    public int getAddress(String symbol) {
        return this.symbTable.get(symbol);
    }
}
