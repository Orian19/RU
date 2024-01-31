import java.io.IOException;
import java.io.File;
import java.io.FilenameFilter;

public class Main{
    public static void main(String [] args) throws IOException{
        String filesList[], fileName;
        File file = new File(args[0]);
        boolean isDirectory = file.isDirectory();

        if (isDirectory) {

            // filtering the files only with .asm signature
            FilenameFilter textFilefilter = new FilenameFilter(){
                public boolean accept(File dir, String name) {
                   String lowercaseName = name.toLowerCase();
                   if (lowercaseName.endsWith(".vm")) {
                      return true;
                   } else {
                      return false;
                   }
                }
             };
              // filesList - holding the .asm files names
              filesList = file.list(textFilefilter);
              // fileName - holding the hack files names
              fileName = file.getName()+".asm";
             
        }else{
              filesList = new String [1];
              // filesList - holding the .vm files names
              filesList[0] = file.getName();
              // fileName - holding the .asm files names
              fileName= file.getName().substring(0, file.getName().length()-3) + ".asm";
        }
        CodeWriter asmFile = new CodeWriter(fileName);
        for(int i = 0; i< filesList.length; i++){
            int arg2 = -1;
            String segment = null;
            // second pass
            Parser vmFile = new Parser(filesList[i]);
            

            try {
                while (vmFile.hasMoreLines()){
                    if (vmFile.getInstruction().isEmpty() || vmFile.getInstruction().charAt(0) == '/'){
                        vmFile.advance();
                        continue;
                    } 
                    boolean pushOrpop = false;
                    String typeInst = vmFile.commandType();
                    if(typeInst.equals(Parser.C_PUSH) || typeInst.equals(Parser.C_POP)|| typeInst.equals(Parser.C_FUNCTION)||typeInst.equals(Parser.C_CALL)){
                        arg2 = vmFile.arg2();
                        if(typeInst.equals(Parser.C_PUSH) || typeInst.equals(Parser.C_POP)) 
                            pushOrpop = true;
                    }
                    if (!typeInst.equals(Parser.C_RETURN)){
                        segment = vmFile.arg1();
                    }

                    if (pushOrpop){
                        asmFile.writePushPop(typeInst, segment, arg2);
                    } else {
                        asmFile.writeArithmetic(segment);
                    }
                    

                    vmFile.advance();
                }
                vmFile.closeFile();
                
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        
        }
        asmFile.close();
    }
}
