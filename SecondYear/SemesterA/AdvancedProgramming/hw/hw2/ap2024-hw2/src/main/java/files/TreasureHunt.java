package files;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

public class TreasureHunt {
    /**
     * Find the treasure by following the map.
     * <p>
     * Starting with the firstClue, and until it has found the treasure (the decoder returned -1), this method should
     * repeat the following actions:
     * <ol>
     *     <li>Decode the clue (using the decoder) to get the index of the next clue. </li>
     *     <li>Read the next clue from the map. Each clue consists of the 48 bits of the map starting at the index
     *     returned from the decoder (treat anything beyond the end of the map as 0).
     * <p>
     *     The index of a clue is given in bits from the beginning of the map,
     *     where 0 is the MSB of the first byte (and map_size_in_bytes*8-1 is the LSB of the last byte). </li>
     * </ol>
     *
     * @param map       This is a {@link FileChannel} containing the encoded treasure map.
     * @param decoder   The decoder used to find the location of the next clue
     * @param firstClue The first clue.
     * @return The index of the treasure in the file (in bits)
     * @throws IOException
     */
    public static long findTreasure(FileChannel map, TreasureMapDecoder decoder, long firstClue) throws IOException {
        int clueSize = 48;
        int byteLenInBits = Long.BYTES; // 8
        int bytesNum = clueSize / byteLenInBits; // 48 / 8 = 6
        long mapLength = map.size() * byteLenInBits;
        ByteBuffer buffer = ByteBuffer.allocate(byteLenInBits);  // allocate buffer size

        long nextClueLoc = -1;
        long curClue = firstClue; // value of the current clue
        long curClueLoc = nextClueLoc; // index of the current clue
        while (true) {
            int idx = 0;
            int isLocInMid = 0; // flag to check if the clue index is somewhere in the middle of a byte
            long bitsToTruncateLeft; // number of bits to "cut off" to the left (irrelevant for the clue)
            long bitsToTruncateRight; // number of bits to "cut off" to the right (irrelevant for the clue)

            nextClueLoc = decoder.decodeClue(curClue, curClueLoc, mapLength); // decoding the clue and getting its index

            if (nextClueLoc == -1) // checking if the treasure was found
                return curClueLoc;

            map.position(nextClueLoc / byteLenInBits); // move to new position in the map (given by decoder)

            buffer.clear(); // emptying the buffer

            // checking if the clue location is in the middle of a byte (we count location from start of map - index = 0)
            // we can't read certain parts of a byte, so need to manipulate the data to get only relevant bits for the clue
            if (nextClueLoc % byteLenInBits != 0)
                isLocInMid = 1;

            // pad with zero from the left (need this to be able to read the "isolated" bits)
            int len = byteLenInBits - bytesNum - isLocInMid;
            while (idx < len) {
                buffer.put((byte) 0);
                idx++; // will be 1 or 2
            }

            int bytesRead = map.read(buffer); // reading from the buffer (output = number of bytes read)
            buffer.rewind(); // put position to start (0)

            // getting the actual bits/bytes
            curClue = buffer.getLong(); // automatically pads with zeros to the right (end of map)

            // manipulating the 'long' value to contain only the clue
            bitsToTruncateLeft = (byteLenInBits * isLocInMid) + (nextClueLoc % byteLenInBits) + idx - 1; // padding + location inside the byte (loc % 8)
            bitsToTruncateRight = byteLenInBits - (nextClueLoc % byteLenInBits);

            curClue <<= bitsToTruncateLeft; // remove all irrelevant bits from the left
            curClue >>>= bitsToTruncateLeft; // "cut" the value to 48bit instead of 64 (not fully got read of left bits)
            curClue >>>= isLocInMid * bitsToTruncateRight; // remove all irrelevant bits from the right: 8 - (loc % 8). "pulling from left side we get leftovers in right side to complete long 64bits"
            // example (testFindTreasureReading):
            // clue index is 3:      00000000 XXX01010 01010101 00000001 00000010 00110011 01000100 011XXXXX (first clue in map) (X's are "junk" + zero padding to the left)
            // first shift (left):   01010010 10101000 00001000 00010001 10011010 00100011 XXXXXX00 00000000 (now we have zeros in the end + the "junk" from the right
            // second shift (right): 00000000 00001010 01010101 00000001 00000010 00110011 01000100 011XXXXX (now we have our clue + the "junk" from the right
            // third shift (right):  00000000 00000000 01010010 10101000 00001000 00010001 10011010 00100011 = 90881643354659 (now we have only the bits for our clue :) )

//            // attempt with masking (did not work - only passed first two tests)
//            long clueSizeMask = (1L << 48) -1;
//            long rightMask = (1L << clueSize + bitsToTruncateRight) - 1;
//            curClue = (rightMask & curClue >>> bitsToTruncateRight) & clueSizeMask; // (((1L << 48+5) -1) & curClue >>> 5) & ((1L << 48) -1)

            curClueLoc = nextClueLoc;
        }
    }
}
