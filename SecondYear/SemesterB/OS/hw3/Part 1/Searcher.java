import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * A searcher thread.
 * Searches for files containing a given pattern and that end with a specific extension in all directories listed in a directory queue.
 */


public class Searcher implements Runnable {
    private String pattern;
    private String extension;
    private SynchronizedQueue<File> directoryQueue;
    private SynchronizedQueue<File> resultsQueue;

    /**
     * Constructor. Initializes the searcher thread.
     *
     * @param pattern        Pattern to look for
     * @param extension      wanted extension
     * @param directoryQueue A queue with directories to search in (as listed by the scouter)
     * @param resultsQueue   A queue for files found (to be copied by a copier)
     */
    public Searcher(String pattern, String extension, SynchronizedQueue<File> directoryQueue,
                    SynchronizedQueue<File> resultsQueue) {
        this.pattern = pattern;
        this.extension = extension;
        this.directoryQueue = directoryQueue;
        this.resultsQueue = resultsQueue;
    }

    /**
     * Runs the searcher thread. Thread will fetch a directory to search in from the directory queue,
     * then search all files inside it (but will not recursively search subdirectories!).
     * Files that a contain the pattern and have the wanted extension are enqueued to the results queue.
     * This method begins by registering to the results queue as a producer and when finishes, it unregisters from it.
     */
    @Override
    public void run() {
        this.resultsQueue.registerProducer(); // register new producer = new searcher thread

        try {
            File dir = this.directoryQueue.dequeue(); // fetching a directory from the queue
            if (dir != null) {
                File[] files = dir.listFiles();
                if (files != null) { // ignoring if null
                    int dotPos;
                    String fileName;
                    String fileExtension;
                    // checking for each file it matches the format and if so, enqueuing it to results
                    for (File file : files) {
                        if (file.isFile()) {
                            dotPos = file.getName().lastIndexOf(".");
                            fileName = file.getName().substring(0, dotPos);
                            fileExtension = file.getName().substring(dotPos + 1);
                            if (fileName.contains(this.pattern) && fileExtension.equals(this.extension)) {
                                this.resultsQueue.enqueue(file);
                            }
                        }
                    }
                }
            }
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        this.resultsQueue.unregisterProducer();
    }
}
