package files;

import java.io.*;
import java.util.LinkedList;
import java.util.List;

public class Streams {
    /**
     * Read from an InputStream until a quote character (") is found, then read
     * until another quote character is found and return the bytes in between the two quotes.
     * If no quote character was found return null, if only one, return the bytes from the quote to the end of the stream.
     *
     * @param in
     * @return A list containing the bytes between the first occurrence of a quote character and the second.
     */
    public static List<Byte> getQuoted(InputStream in) throws IOException {
        List<Byte> bytes = new LinkedList<>();

        int qQount = 0;
        int curB;
        try {
            curB = in.read(); // reading one byte
            while (curB != -1) {  // running until end of file
                if (curB == '"')
                    qQount++;
                if (curB != '"' && qQount == 1) // checking if we are inside quotes
                    bytes.add((byte) curB);
                else if (qQount == 2) // found closing quote
                    break;
                curB = in.read();
            }
        } catch (IOException e) {
            throw new IOException(e);
        }

        if (qQount >= 1)
            return bytes;
        else
            return null;
    }


    /**
     * Read from the input until a specific string is read, return the string read up to (not including) the endMark.
     *
     * @param in      the Reader to read from
     * @param endMark the string indicating to stop reading.
     * @return The string read up to (not including) the endMark (if the endMark is not found, return up to the end of the stream).
     */
    public static String readUntil(Reader in, String endMark) throws IOException {
        StringBuilder str = new StringBuilder(); // efficient way to construct a string
        boolean markFound = false;

        try {
            BufferedReader bufferedReader = new BufferedReader(in);
            String line = bufferedReader.readLine(); // using bufferReader to read a line from the Reader input

            while (line != null) {
                if (line.contains(endMark)) { // checking if endMark is in the current line
                    str.append(line, 0, line.indexOf(endMark)); // getting entire line up to the endMark
                    markFound = true;
                    break;
                } else
                    str.append(line);

                line = bufferedReader.readLine(); // advancing to a new line
            }
        } catch (IOException e) {
            throw new IOException(e);
        }

        if (markFound)
            return str.toString();
        else
            return null;
    }

    /**
     * Copy bytes from input to output, ignoring all occurrences of badByte.
     *
     * @param in
     * @param out
     * @param badByte
     */
    public static void filterOut(InputStream in, OutputStream out, byte badByte) throws IOException {
        int curB;
        try {
            curB = in.read(); // reading one byte
            while (curB != -1) {  // running until end of file
                if ((byte) curB != badByte)
                    out.write(curB); // copying good byte

                curB = in.read();
            }
        } catch (IOException e) {
            throw new IOException(e);
        }
    }

    /**
     * Read a 40-bit (unsigned) integer from the stream and return it. The number is represented as five bytes,
     * with the most-significant byte first.
     * If the stream ends before 5 bytes are read, return -1.
     *
     * @param in
     * @return the number read from the stream
     */
    public static long readNumber(InputStream in) throws IOException {
        long num = 0;
        int bytesCounter = 0; // counter for the number of bytes read

        int curB;
        try {
            curB = in.read();
            while (curB != -1 && bytesCounter < 5) {
                bytesCounter++;

                num <<= 8; // shifting left in order to "make space" for the next number (MSB first)
                num += curB; // adding the new byte to num

                curB = in.read();
            }
        } catch (IOException e) {
            throw new IOException(e);
        }

        if (bytesCounter == 5) {
            return num; // returning the full number that is constructed of 5 bytes (MSB first)
        }
        else
            return -1;
    }
}
