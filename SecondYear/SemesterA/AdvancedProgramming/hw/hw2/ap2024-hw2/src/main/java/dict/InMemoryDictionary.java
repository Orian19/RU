package dict;

import java.io.*;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;

/**
 * Implements a persistent dictionary that can be held entirely in memory.
 * When flushed, it writes the entire dictionary back to a file.
 * <p>
 * The file format has one keyword per line:
 * <pre>word:def</pre>
 * <p>
 * Note that an empty definition list is allowed (in which case the entry would have the form: <pre>word:</pre>
 *
 * @author talm
 */
public class InMemoryDictionary extends TreeMap<String, String> implements PersistentDictionary {
    private static final long serialVersionUID = 1L; // (because we're extending a serializable class)
    private File dictFile;

    /**
     * constructor
     * @param dictFile in file
     */
    public InMemoryDictionary(File dictFile) {
        this.dictFile = dictFile;
    }

    /**
     * implementing open() from persistentDictionary
     *
     * @throws IOException might throw input output exception
     */
    @Override
    public void open() throws IOException {
        this.clear(); // discarding existing memory (of previous TreeMap)

        int colonSepPos;
        String curKey;
        String curValue;
        try {
            if (this.dictFile.exists()) {
                Reader reader = new FileReader(this.dictFile);
                BufferedReader bufferedReader = new BufferedReader(reader);
                String line = bufferedReader.readLine();

                while (line != null) {
                    colonSepPos = line.indexOf(":");
                    curKey = line.substring(0, colonSepPos); // string from start to colon (word)
                    curValue = line.substring(colonSepPos + 1); // string from colon to end (def)

                    this.put(curKey, curValue); // adding key: value pair to the dictionary

                    line = bufferedReader.readLine();
                }

                bufferedReader.close();
                reader.close();

            }
        } catch (IOException e) {
            throw new IOException(e);
        }
    }

    /**
     * implementing out() from persistentDictionary
     *
     * @throws IOException might throw input output exception
     */
    @Override
    public void close() throws IOException {
        try {
            Writer writer = new FileWriter(this.dictFile);
            BufferedWriter bufferedWriter = new BufferedWriter(writer);

            Iterator<String> itr = this.keySet().iterator(); // creating iterator to go over the set of keys
            String curKey;
            String curValue;
            while (itr.hasNext()) {
                StringBuilder line = new StringBuilder();
                curKey = itr.next();
                curValue = get(curKey);

                // creating the word:def (key:value) pairs for each line
                line.append(curKey);
                line.append(":");
                line.append(curValue);

                // writing the new line into the current file
                bufferedWriter.write(line.toString());
                bufferedWriter.newLine();
            }

            bufferedWriter.close();
            writer.close();

        } catch (IOException e) {
            throw new IOException(e);
        }
    }
}
