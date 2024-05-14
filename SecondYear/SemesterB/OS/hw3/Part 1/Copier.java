import java.io.File;

/**
 * A copier thread. Reads files to copy from a queue and copies them to the given destination.
 *
 */


public class Copier implements Runnable{
    public static final int COPY_BUFFER_SIZE = 4096;

    /**
     *Constructor. Initializes the worker with a destination directory and a queue of files to copy.
     *
     * @param destination Destination directory
     * @param resultsQueue Queue of files found, to be copied
     */
    public Copier(File destination, SynchronizedQueue<File> resultsQueue) {
        // todo: complete
        return;
    }

    /**
     *Runs the copier thread. Thread will fetch files from queue and copy them,
     * one after each other, to the destination directory.
     * When the queue has no more files, the thread finishes.
     *
     */
    public void run() {
        // todo: complete
        return;
    }
}
