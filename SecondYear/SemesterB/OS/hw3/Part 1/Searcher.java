import java.io.File;

/**
 * A searcher thread.
 * Searches for files containing a given pattern and that end with a specific extension in all directories listed in a directory queue.
 *
 */


public class Searcher implements Runnable {
    /**
     * Constructor. Initializes the searcher thread.
     *
     * @param pattern Pattern to look for
     * @param extension wanted extension
     * @param directoryQueue A queue with directories to search in (as listed by the scouter)
     * @param resultsQueue  A queue for files found (to be copied by a copier)
     */
    public Searcher(String pattern, String extension, SynchronizedQueue<File> directoryQueue,
                    SynchronizedQueue<File> resultsQueue) {
        // Todo: complete
        return;
    }

    /**
     * Runs the searcher thread. Thread will fetch a directory to search in from the directory queue,
     * then search all files inside it (but will not recursively search subdirectories!).
     * Files that a contain the pattern and have the wanted extension are enqueued to the results queue.
     * This method begins by registering to the results queue as a producer and when finishes, it unregisters from it.
     */
    public void run() {
        // Todo: complete
        return;
    }
}
