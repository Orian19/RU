package files;

import java.io.IOException;
import java.io.RandomAccessFile;

public class RandomAccess {
    /**
     * Treat the file as an array of (unsigned) 8-bit values and sort them
     * in-place using a bubble-sort algorithm.
     * You may not read the whole file into memory!
     *
     * @param file
     */
    public static void sortBytes(RandomAccessFile file) throws IOException {
        boolean swapped; // makes bubble sort more efficient (if no swaps at some iteration we are done --> sorted)
        long len = file.length();

        int curB1; // value1
        int curB2; // value2 - adjacent to value1

        try {
            for (int i = 0; i < len; i++) {
                swapped = false; // resetting swap flag
                for (int j = 0; j < len - i; j++) {
                    file.seek(j); // seeking current index (moving pointer)
                    curB1 = file.read(); // reading current byte
                    curB2 = file.read(); // reading next byte
                    if (curB2 != -1 && curB1 > curB2) {
                        // swapping values
                        file.seek(j);
                        file.write(curB2);
                        file.seek(j + 1);
                        file.write(curB1);

                        swapped = true;
                    }
                }
                if (!swapped) // if not swaps --> file is sorted
                    break;
            }
        } catch (IOException e) {
            throw new IOException(e);
        }
    }

    /**
     * Treat the file as an array of unsigned 24-bit values (stored MSB first) and sort
     * them in-place using a bubble-sort algorithm.
     * You may not read the whole file into memory!
     *
     * @param file
     * @throws IOException
     */
    public static void sortTriBytes(RandomAccessFile file) throws IOException {
        boolean swapped; // makes bubble sort more efficient (if no swaps at some iteration we are done --> sorted)
        long len = file.length();
        int numOfBytes = 24 / 8;

        try {
            for (int i = 1; i < len; i += numOfBytes) { // jumping by numOfBytes to get to next "full" value
                swapped = false; // resetting swap flag
                file.seek(0);
                for (int j = 0; j < len - 3; j += numOfBytes) {
                    int value1 = 0;
                    int value2 = 0;

                    file.seek(j); // seeking current index (moving pointer)
                    value1 = getValue(file, numOfBytes, value1); // getting the actual value. 24-bit = 8-bit * 3 (MSB first)

                    file.seek(j + numOfBytes);
                    value2 = getValue(file, numOfBytes, value2);

                    if (value1 > value2) {
                        // swapping values
                        swapValues(file, value1, value2, j, j + numOfBytes, numOfBytes);

                        swapped = true;
                    }
                }
                if (!swapped) // if not swaps --> file is sorted
                    break;
            }
        } catch (IOException e) {
            throw new IOException(e);
        }
    }

    /**
     * helper function to get the actual value of n-bit values
     *
     * @param f RAF file
     * @param n number of bytes the value is constructed of
     * @return full value
     */
    public static int getValue(RandomAccessFile f, int n, int curVal) throws IOException {
        for (int i = 0; i < n; i++) {
            curVal <<= 8;
            curVal += f.read();
        }
        return curVal;
    }

    /**
     * helper function to swap n-bit values
     *
     * @param f    RAF file
     * @param val1 n-bit value1
     * @param val2 n-bit value2
     * @param p1   pointer to value1
     * @param p2   pointer to value2
     * @param n    number of bytes per value
     */
    public static void swapValues(RandomAccessFile f, int val1, int val2, int p1, int p2, int n) throws IOException {
        f.seek(p1);
        for (int i = n - 1; i >= 0; i--) {
            f.write(val2 >> i * 8); // swapping one byte at a time (shifting right to advance to next byte)
        }

        f.seek(p2);
        for (int i = n - 1; i >= 0; i--) {
            f.write(val1 >> i * 8);
        }
    }
}
