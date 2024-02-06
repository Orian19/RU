import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class Parser {

    private BufferedReader bufferedReader;
    private String instruction;
    public static final String C_ARITHMETIC = "C_ARITHMETIC";
    public static final String C_PUSH = "C_PUSH";
    public static final String C_POP = "C_POP";
    public static final String C_LABEL = "C_LABEL";
    public static final String C_GOTO = "C_GOTO";
    public static final String C_IF = "C_IF";
    public static final String C_FUNCTION = "C_FUNCTION";
    public static final String C_RETURN = "C_RETURN";
    public static final String C_CALL = "C_CALL";
    /**
     * constructor
     * @return
     */
    public Parser(String file){

        try{
            FileReader reader = new FileReader (file);
            this.bufferedReader = new BufferedReader(reader);
            this.instruction = this.bufferedReader.readLine();

        }catch(IOException e){
            e.printStackTrace();
        }   
    }

    /**
     * checks if there exists more instruction
     * @return true if there is more instruction
     */
    public boolean hasMoreLines() throws IOException{        
        if (this.instruction == null){
            return false;
        }else{
            return true;
        }
    }

    /**
     * advances to the next instruction
     * @return string of instruction
     */
    public void advance() throws IOException{
        this.instruction = this.bufferedReader.readLine();
        if (this.instruction != null){
            this.instruction = this.instruction.trim();
        }
    }

    /**
     * gets the command of the current instruction
     * @return command
     */
    public String commandType(){

        String [] ar = {"add","sub","neg","eq","gt","lt","and","or","not"};

        if (this.instruction.indexOf("pop") != -1){
            return C_POP;
        } else if (this.instruction.indexOf("push") != -1){
            return C_PUSH;
        } else if (Arrays.asList(ar).contains(this.instruction.split("\\s+")[0].trim())){
            return C_ARITHMETIC;
        } else if (this.instruction.indexOf("label") != -1){
            return C_LABEL;
        } else if (this.instruction.indexOf("if-goto") != -1){
            return C_IF;
        } else if (this.instruction.indexOf("goto") != -1){
            return C_GOTO;
        }else if (this.instruction.indexOf("function") != -1){
            return C_FUNCTION;
        } else if (this.instruction.indexOf("call") != -1){
            return C_CALL;
        } else if (this.instruction.indexOf("return") != -1){
            return C_RETURN;
        }
        return "";
    }

    /**
     * returns the first argument of the current command
     * should not be called if the current command is c_return
     * @return
     */
    public String arg1(){
        String [] arguments = this.instruction.split("\\s+");
        String command = this.commandType();

        if (command.equals(C_ARITHMETIC)){
            return arguments[0].trim();
        } else {
            return arguments[1].trim();
        }
        
        //if (command.equals(C_POP) || command.equals(C_PUSH) || command.equals(C_LABEL)|| command.equals(C_GOTO)|| command.equals(C_IF)){
            
        
    }


    /**
     * returns the second argument of the current command
     * called only if the current command is c_push, c_pop, c_function, c_call
     * @return
     */
    public int arg2(){
        String [] arguments = this.instruction.split("\\s+");
        return Integer.parseInt(arguments[2].trim());
    }

    /**
     * closes the current file
     * 
     * 
     * @throws IOException
     */
    public void closeFile() throws IOException{
        this.bufferedReader.close();
    }

    /**
     * get current instruction
     * @return
     */
    public String getInstruction(){
        return this.instruction;

    }

}
