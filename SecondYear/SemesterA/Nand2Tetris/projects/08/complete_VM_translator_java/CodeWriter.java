import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;

public class CodeWriter {
    private BufferedWriter bufferedWriter;
    private String out_file;
    private int countEqual;
    private int countCalls;


    private String fileName;


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
            this.countCalls = 0;
        }catch(IOException e){
            e.printStackTrace();
        }   
    }

    /**
     * sets a new file name
     * @param command
     */
    public void SetFileName(String fileName){
        this.fileName = fileName;
    }


    /**
     * write the labal command in asm language to ouput file
     * @param command
     * @throws IOException
     */
    public void writeLabel(String label) throws IOException{
        this.bufferedWriter.write("("+label+")\n");
    }


    /**
     * write the goto command in asm language to ouput file
     * @param command
     * @throws IOException
     */
    public void writeGoto(String label) throws IOException{
        this.bufferedWriter.write("@"+label+"\n"+"0;JMP\n");

    }

    /**
     * write the if-goto command in asm language to ouput file
     * @param command
     * @throws IOException
     */
    public void writeIf(String label) throws IOException{
        // jumping if the result in the stack is not equal to zero (false)
        this.bufferedWriter.write("@SP\n"+
                                   "AM=M-1\n"+
                                   "D=M\n" + 
                                   "@"+label+"\n"+
                                   "D;JNE\n");
    }

    /**
     * assembly code that effects the function command
     * 
     * @param functionName
     * @param nVars
     * @throws IOException 
     */
    public void writeFunction(String functionName, int nVars) throws IOException {
        writeLabel(functionName);
        for(int i = 0; i < nVars; i++){
            this.bufferedWriter.write("@LCL\n"+
                                            "D=M\n"+
                                            "@" + i + "\n" +
                                            "A=D+A\n" +
                                            "M=0\n" +
                                            "@SP\n" +
                                            "M=M+1\n");              
        }
    }


    /**
     * assembly code that effects the return command
     * 
     * @param functionName
     * @param nVars
     * @throws IOException 
     */
    public void writeReturn() throws IOException {
       this.bufferedWriter.write("@LCL\n" +
                                        "D=M\n"+ 
                                        "@endFrame\n"+
                                        "M=D\n" +
                                        "@endFrame\n" +
                                        "D=M\n" +
                                        "@5\n" +
                                        "A=D-A\n"+
                                        "D=M\n" +
                                        "@retAddr\n" + 
                                        "M=D\n");  
                                        
        this.bufferedWriter.write("@SP\n" + 
                                    "AM=M-1\n" + 
                                    "D=M\n" + 
                                    "@ARG\n" + 
                                    "A=M\n" + 
                                    "M=D\n");      
        this.bufferedWriter.write("@ARG\n"+
                                    "D=M+1\n" +
                                    "@SP\n" +
                                    "M=D\n"); 
        for(String segment : new String[] {"THAT","THIS","ARG","LCL"}) {     
            this.bufferedWriter.write("@endFrame\n" +
                                        "AM=M-1\n" +
                                        "D=M\n" +
                                        "@"+segment+"\n" +
                                        "M=D\n");                      
        }  
        
       this.bufferedWriter.write("@retAddr\n" + 
                                    "A=M\n" + 
                                    "0;JMP\n");        
    }

    /**
     * assembly code that effects the call command
     * 
     * @param functionName
     * @param nVars
     * @throws IOException 
     */
    public void writeCall(String functionName, int nVars) throws IOException {
        this.bufferedWriter.write(push(functionName + "$ret." + this.countCalls));
        this.bufferedWriter.write(pushPointer("LCL"));
        this.bufferedWriter.write(pushPointer("ARG"));
        this.bufferedWriter.write(pushPointer("THIS"));
        this.bufferedWriter.write(pushPointer("THAT"));
        this.bufferedWriter.write("@SP\n" +
                                   "D=M\n" +
                                   "@5\n" +
                                   "D=D-A\n" +
                                   "@" + nVars + "\n" +
                                   "D=D-A\n" +
                                   "@ARG\n" +
                                   "M=D\n" + 
                                   "@SP\n" +
                                   "D=M\n" +
                                   "@LCL\n" +
                                   "M=D\n");
        writeGoto(functionName);
        writeLabel(functionName + "$ret." + this.countCalls);
        this.countCalls++;
    }

    
   

     /**
     * assembly code that effects the bootstrap commands
     * @throws IOException 
     */
    public void writeBootstrap() throws IOException{
        this.bufferedWriter.write("@256\n" +
                                "D=A\n" + 
                                "@SP\n" + 
                                "M=D\n");
        writeCall("Sys.init",0);

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
        return "@" + this.fileName + "." + idx + "\n" +
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
        return "@" + this.fileName + "." + idx + "\n" +
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
