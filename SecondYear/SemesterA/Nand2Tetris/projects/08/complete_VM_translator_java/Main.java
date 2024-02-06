import java.io.IOException;
import java.io.File;
import java.io.FilenameFilter;

public class Main{
    public static void main(String [] args) throws IOException{
        String filesList[], fileName;
        boolean isSys = false;
        int countFiles = 0;
        File file = new File(args[0]);
        boolean isDirectory = file.isDirectory();

        if (isDirectory) {
            File[] files = file.listFiles();
            for(File f: files){
                if (f.getName().equals("Sys.vm")){
                    isSys = true;
                }
                if (f.getName().endsWith(".vm")){
                    countFiles++;
                }
            }
            filesList = new String[countFiles];
            int i = 0;
            for(File f: files){
                if (f.getName().endsWith(".vm")){
                    filesList[i] =  f.getPath();
                    i++;
                }
            }
              // fileName - holding the hack files names
              fileName = file.getPath() + "/" + file.getName() + ".asm";
             
        }else{
              filesList = new String [1];
              // filesList - holding the .vm files names
              filesList[0] = file.getPath();
              // fileName - holding the .asm files names
              fileName= file.getPath().substring(0, file.getPath().lastIndexOf(".")) + ".asm";
        }
        CodeWriter asmFile = new CodeWriter(fileName);
        if(isSys) asmFile.writeBootstrap();
        for(int i = 0; i< filesList.length; i++){
            int arg2 = -1;
            String segment = null;
            // second pass
            Parser vmFile = new Parser(filesList[i]);
            asmFile.SetFileName(filesList[i].substring(filesList[i].lastIndexOf("/")+1,filesList[i].length()-3));
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
                    } 
                    switch (typeInst) {
                        case Parser.C_ARITHMETIC:
                            asmFile.writeArithmetic(segment);
                            break;
                        case Parser.C_LABEL:
                            asmFile.writeLabel(segment);
                            break;
                        case Parser.C_GOTO:
                            asmFile.writeGoto(segment);
                            break;
                        case Parser.C_IF:
                            asmFile.writeIf(segment);
                            break;
                        case Parser.C_RETURN:
                            asmFile.writeReturn();
                            break;
                        case Parser.C_FUNCTION:
                            asmFile.writeFunction(segment, arg2);
                            break;
                        case Parser.C_CALL:
                            asmFile.writeCall(segment, arg2);
                            break;
                        default:
                            break;
                    }

                    vmFile.advance();
                }
                
                
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } finally {
                vmFile.closeFile();
            }
        
        }
        asmFile.close();
    }
}
