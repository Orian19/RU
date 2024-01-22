import java.io.IOException;
import java.io.BufferedWriter;
import java.io.FileWriter;

public class Main {
    public static void main(String[] args) {
        Parser asmFile = new Parser(args[0]); // asm to translate
        SymbolTable symbTable = new SymbolTable();
        int countLines = 0;

        // first pass 
        try {
            while (asmFile.hasMoreLines()) {
                if (asmFile.getInstruction().isEmpty() || asmFile.getInstruction().charAt(0) == '/') {
                    asmFile.advance(); // skipping empty lines and comments
                    continue;
                }
                String curInstType = asmFile.instructionType();
                // not contained in symbols + not a number
                if (curInstType.equals(Parser.L_INSTRUCTION)) {
                    String curSymbol = asmFile.symbol();
                    if (!symbTable.contains(curSymbol)) {
                        symbTable.addEntery(curSymbol, countLines);
                        countLines--;
                    }
                }
                countLines++;
                asmFile.advance();
            }
            asmFile.closeFile();
        } catch (IOException e) {
            e.printStackTrace();
        }

        // second pass
        Parser asmFileSecond = new Parser(args[0]);
        try {
            int startAddress = 16;
            // creating the hack file
            FileWriter writer = new FileWriter("Prog.hack");
            BufferedWriter bufferedWriter = new BufferedWriter(writer);
            Code code = new Code();
            String dest, jmp, comp;

            while (asmFileSecond.hasMoreLines()) {
                if (asmFileSecond.getInstruction().isEmpty() || asmFileSecond.getInstruction().charAt(0) == '/') {
                    asmFileSecond.advance(); // skipping empty lines and comments
                    continue;
                }
                StringBuilder instructionBin = new StringBuilder(); // will hold the instruction in binary

                String curInstType = asmFileSecond.instructionType();

                if (curInstType.equals(Parser.A_INSTRUCTION)) {
                    String curSymbol = asmFileSecond.symbol();

                    try { // if @number
                        int check = Integer.parseInt(curSymbol);
                        // parse an integer to a binary string of 16 chars
                        String binVal = Integer.toBinaryString(check);
                        String binValStr = String.format("%16s", binVal).replace(' ', '0');
                        instructionBin.append(binValStr);
                    } catch (NumberFormatException e) { // if @Nan
                        if (!symbTable.contains(curSymbol)) {
                            symbTable.addEntery(curSymbol, startAddress);
                            startAddress++;
                        }
                        // parse an integer to a binary string of 16 chars
                        String val = Integer.toBinaryString(symbTable.getAddress(curSymbol));
                        String binValStr = String.format("%16s", val).replace(' ', '0');
                        instructionBin.append(binValStr);
                    } catch (NullPointerException e) {  // if @Nan
                        if (!symbTable.contains(curSymbol)) {
                            symbTable.addEntery(curSymbol, startAddress);
                            startAddress++;
                        }
                        // parse an integer to a binary string of 16 chars
                        String val = Integer.toBinaryString(symbTable.getAddress(curSymbol));
                        String binValStr = String.format("%16s", val).replace(' ', '0');
                        instructionBin.append(binValStr);
                    }
                }
                // handling c-instruction type
                else if (curInstType.equals(Parser.C_INSTRUCTION)) {
                    dest = code.dest(asmFileSecond.dest());
                    jmp = code.jump(asmFileSecond.jmp());
                    comp = code.comp(asmFileSecond.comp());

                    // adding 111 as the first 3 bits
                    instructionBin.append("111");
                    // adding the rest in the correct order to create 16-bit string (instruction)
                    instructionBin.append(comp);
                    instructionBin.append(dest);
                    instructionBin.append(jmp);
                }
                // if the instruction string is not empty write that string in the file and create a new line
                if (!instructionBin.isEmpty()) bufferedWriter.write(instructionBin.toString() + "\n");

                asmFileSecond.advance(); // move to the next instruction
            }
            asmFileSecond.closeFile();
            bufferedWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
