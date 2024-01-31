import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;

public class CodeWriter {
    private BufferedWriter bufferedWriter;
    private String out_file;
    private int countEqual;

    /**
     * constructor
     * @param out_file
     */
    public CodeWriter(String out_file){
        this.out_file = out_file;
        try{
            FileWriter writer = new FileWriter (out_file);
            this.bufferedWriter = new BufferedWriter(writer);
            this.countEqual = 1;
        }catch(IOException e){
            e.printStackTrace();
        }   
    }

    /**
     * write the arithemtic command in asm language to ouput file
     * @param command
     * @throws IOException
     */
    public void writeArithmetic(String command) throws IOException {
        
        switch (command) {
            case "add":
                this.bufferedWriter.write("@SP\n" + 
                                          "M=M-1\n" + 
                                          "A=M\n" + 
                                          "D=M\n" + 
                                          "A=A-1\n" + 
                                          "M=D+M\n");
                break;
            case "sub":
                this.bufferedWriter.write("@SP\n" +
                                        "M=M-1\n" + 
                                        "A=M\n" + 
                                        "D=M\n" + 
                                        "A=A-1\n" + 
                                        "M=M-D\n");
                break;
            case "neg":
                this.bufferedWriter.write("@SP\n" +
                                          "A=M-1\n" + 
                                          "M=-M\n");
                break;
            case "eq":
                this.bufferedWriter.write("@SP\n" +
                                          "M=M-1\n" + 
                                          "A=M\n" +
                                          "D=M\n" +
                                          "A=A-1\n" +
                                          "D=D-M\n" +
                                          "@EQUAL_"+ this.countEqual + "\n" +
                                          "D;JEQ\n" +
                                          "@SP\n" +
                                          "A=M-1\n" +
                                          "M=0\n" +
                                          "@NOTEQUAL_" + this.countEqual + "\n" +
                                          "0;JMP\n" +
                                          "(EQUAL_" + this.countEqual + ")\n" +
                                          "@SP\n" +
                                          "A=M-1\n" +
                                          "M=-1\n" +
                                          "(NOTEQUAL_" + this.countEqual + ")\n");
                this.countEqual++;
                break;
            case "gt":
                this.bufferedWriter.write("@SP\n" +
                                        "M=M-1\n" + 
                                        "A=M\n" +
                                        "D=M\n" +
                                        "A=A-1\n" +
                                        "D=D-M\n" +
                                        "@GT_" + this.countEqual + "\n" +
                                        "D;JLT\n" +
                                        "@SP\n" +
                                        "A=M-1\n" +
                                        "M=0\n" +
                                        "@NOTGT_" + this.countEqual + "\n" +
                                        "0;JMP\n" +
                                        "(GT_" + this.countEqual + ")\n" +
                                        "@SP\n" +
                                        "A=M-1\n" +
                                        "M=-1\n" +
                                        "(NOTGT_" + this.countEqual + ")\n"); 
                this.countEqual++; 
                break;
            case "lt":
                this.bufferedWriter.write("@SP\n" +
                                        "M=M-1\n" + 
                                        "A=M\n" +
                                        "D=M\n" +
                                        "A=A-1\n" +
                                        "D=D-M\n" +
                                        "@LT_" + this.countEqual + "\n" +
                                        "D;JGT\n" +
                                        "@SP\n" +
                                        "A=M-1\n" +
                                        "M=0\n" +
                                        "@NOTLT_" + this.countEqual + "\n" +
                                        "0;JMP\n" +
                                        "(LT_" + this.countEqual + ")\n" +
                                        "@SP\n" +
                                        "A=M-1\n" +
                                        "M=-1\n" +
                                        "(NOTLT_" + this.countEqual + ")\n"); 
                this.countEqual++;  
                break;
            case "and":
                this.bufferedWriter.write("@SP\n" + 
                                        "M=M-1\n" + 
                                        "A=M\n" + 
                                        "D=M\n" + 
                                        "A=A-1\n" + 
                                        "M=D&M\n");
                    break;
            case "or":
                this.bufferedWriter.write("@SP\n" + 
                                        "M=M-1\n" + 
                                        "A=M\n" + 
                                        "D=M\n" + 
                                        "A=A-1\n" + 
                                        "M=D|M\n");
                break;
            case "not":
                this.bufferedWriter.write("@SP\n" + 
                                        "A=M-1\n" +    
                                        "M=!M\n");
                break;
            default:
                break;
        }
    }

    /**
     * write the push/pop command in asm language to ouput file
     * @param command
     * @param segement
     * @param index
     * @throws IOException 
     */
    public void writePushPop(String command, String segement, int index) throws IOException {
        String idx = Integer.toString(index);
        String [] segments = {"local","argument","this","that"};
        String [] segments_label = {"LCL","ARG","THIS","THAT"};
        
        if (command.equals(Parser.C_PUSH)) {
            if (segement.equals("local") || segement.equals("argument") ||
            segement.equals("this") || segement.equals("that")) {
                int i = Arrays.asList(segments).indexOf(segement);
                this.bufferedWriter.write(this.pushCommand(segments_label[i], idx));
            } else if (segement.equals("constant")) {
                this.bufferedWriter.write(this.push(idx));
            } else if (segement.equals("static")) {
                this.bufferedWriter.write(this.pushStat(idx));
            } else if (segement.equals("temp")) {
                this.bufferedWriter.write(this.pushTemp(idx));
            } else if (segement.equals("pointer")) {
                if (index == 0) {
                    this.bufferedWriter.write(this.pushPointer("THIS"));
                } else {
                    this.bufferedWriter.write(this.pushPointer("THAT"));
                }
            }
        } else if (command.equals(Parser.C_POP)){
            if (segement.equals("local") || segement.equals("argument") ||
            segement.equals("this") || segement.equals("that")) {
                int i = Arrays.asList(segments).indexOf(segement);
                this.bufferedWriter.write(this.popCommand(segments_label[i], idx));
            } else if (segement.equals("static")) {
                this.bufferedWriter.write(this.popStat(idx));
            } else if (segement.equals("temp")) {
                this.bufferedWriter.write(this.popTemp(idx));
            } else if (segement.equals("pointer")) {
                if (index == 0) {
                    this.bufferedWriter.write(this.popPointer("THIS"));
                } else {
                    this.bufferedWriter.write(this.popPointer("THAT"));
                }
            }
        }
    }

    public String popCommand(String segment, String idx) {
        return "@" + segment +"\n" +
                "D=M\n" +
                "@" + idx + "\n" +
                "D=D+A\n" +
                "@R13\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M-1\n" +
                "A=M\n" +
                "D=M\n" +
                "@R13\n" +
                "A=M\n" +
                "M=D\n";
    }

    public String popStat(String idx) {
        return "@" + this.out_file + "." + idx + "\n" +
                "D=A\n" +
                "@R13\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M-1\n" +
                "A=M\n" +
                "D=M\n" +
                "@R13\n" +
                "A=M\n" +
                "M=D\n";
    }

    public String popTemp(String idx) {
        return "@5\n" +
                "D=A\n" +
                "@" + idx + "\n" +
                "D=D+A\n" +
                "@R13\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M-1\n" +
                "A=M\n" +
                "D=M\n" +
                "@R13\n" +
                "A=M\n" +
                "M=D\n";
    }

    public String pop(String idx) {
        return "@" + idx + "\n" +
                "D=A\n" +
                "@R13\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M-1\n" +
                "A=M\n" +
                "D=M\n" +
                "@R13\n" +
                "A=M\n" +
                "M=D\n";
    }

    public String popPointer(String idx) {
        return  "@SP\n" +
                "M=M-1\n" +
                "A=M\n" +
                "D=M\n" +
                "@" + idx + "\n" +
                "M=D\n";
    }

    public String pushCommand(String segment, String idx) {
        return "@" + segment +" \n" +
                "D=M\n" +
                "@" + idx + "\n" +
                "D=D+A\n" +
                "@R13\n" +
                "M=D\n" +
                "@R13\n" +
                "A=M\n" +
                "D=M\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n";
    }

    public String push(String idx) {
        return "@" + idx +" \n" +
                "D=A\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n";
    }

    public String pushPointer(String idx) {
        return "@" + idx +" \n" +
                "D=M\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n";
    }

    public String pushStat(String idx) {
        return "@" + this.out_file + "." + idx + "\n" +
                "D=M\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n";
    }

    public String pushTemp(String idx) {
        return "@5\n" +
                "D=A\n" +
                "@" + idx + "\n" +
                "D=D+A\n" +
                "@R13\n" +
                "M=D\n" +
                "@R13\n" +
                "A=M\n" +
                "D=M\n" +
                "@SP\n" +
                "A=M\n" +
                "M=D\n" +
                "@SP\n" +
                "M=M+1\n";
    }

    public void close() throws IOException{
        this.bufferedWriter.close();
    }
}
