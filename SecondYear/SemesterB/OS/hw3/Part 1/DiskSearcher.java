import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Main application class. This application searches for all files under some given path that contain a given textual pattern.
 * All files found are copied to some specific directory.
 */


public class DiskSearcher {
    public static final int DIRECTORY_QUEUE_CAPACITY = 50;
    public static final int RESULTS_QUEUE_CAPACITY = 50;

    /**
     * Constructor
     */
    public DiskSearcher() {
    }

    /**
     * Main method. Reads arguments from command line and starts the search.
     *
     * @param args command line args
     */
    public static void main(String[] args) {
        // parsing all user inputs
        String filePattern = args[0];
        String fileExtension = args[1];
        File root = new File(args[2]);
        File destination = new File(args[3]);
        int searchersNum = Integer.parseInt(args[4]);
        int copiersNum = Integer.parseInt(args[5]);

        // initializing a directory and results queues
        SynchronizedQueue<File> directoryQueue = new SynchronizedQueue<>(DIRECTORY_QUEUE_CAPACITY);
        SynchronizedQueue<File> resultsQueue = new SynchronizedQueue<>(RESULTS_QUEUE_CAPACITY);

        // starting scouter thread
        Thread scouterThread = new Thread(new Scouter(directoryQueue, root));
        scouterThread.start();

        // starting searchers threads
        List<Thread> searcherThreads = new ArrayList<>();
        for (int i = 0; i < searchersNum; i++) {
            Thread searcherThread = new Thread(new Searcher(filePattern, fileExtension, directoryQueue, resultsQueue));
            searcherThreads.add(searcherThread);
            searcherThread.start();
        }

        // starting copiers threads
        List<Thread> copierThreads = new ArrayList<>();
        for (int i = 0; i < copiersNum; i++) {
            Thread copierThread = new Thread(new Copier(destination, resultsQueue));
            copierThreads.add(copierThread);
            copierThread.start();
        }

        // waiting for all threads to complete
        try {
            scouterThread.join();
            for (Thread searcherThread : searcherThreads) {
                searcherThread.join();
            }
            for (Thread copierThread : copierThreads) {
                copierThread.join();
            }
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

}
