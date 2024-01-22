package Q3;

import java.util.*;
import java.io.*;

public class Test3 {
    static List<Byte> readGather(RandomAccessFile file, List<Long> indices)
            throws IOException  {
        List<Byte> output = new ArrayList<>();

        for (Long idx: indices) {
            file.seek(idx);
            output.add(file.readByte());
        }

        return output;
    }

    public static void main(String[] args) throws IOException {
        RandomAccessFile randomAccessFile = new RandomAccessFile("hello", "rw");
        randomAccessFile.writeBytes("Hello, World!\n");

        List<Long> indices = List.of(0L, 3L);

        readGather(randomAccessFile, indices);
    }
}
