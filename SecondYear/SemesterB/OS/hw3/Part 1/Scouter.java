import java.io.File;

/**
 * A scouter thread This thread lists all sub-directories from a given root path.
 * Each sub-directory is enqueued to be searched for files by Searcher threads.
 *
 */


public class Scouter implements Runnable {

    /**
     * Construnctor. Initializes the scouter with a queue for the directories to be searched and a root directory to start from
     *
     * @param directoryQueue  A queue for directories to be searched
     * @param root Root directory to start from
     */
    public Scouter(SynchronizedQueue<File> directoryQueue, File root) {
        // Todo: complete
        return;
    }

    /**
     * Starts the scouter thread. Lists directories under root directory and adds them to queue,
     * then lists directories in the next level and enqueues them and so on.
     * This method begins by registering to the directory queue as a producer and when finishes, it unregisters from it.
     *
     */
    public void run() {
        // Todo: complete
        return;
    }
}
