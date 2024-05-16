import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * A scouter thread This thread lists all sub-directories from a given root path.
 * Each sub-directory is enqueued to be searched for files by Searcher threads.
 */


public class Scouter implements Runnable {
    private SynchronizedQueue<File> directoryQueue;
    private File root;

    /**
     * Construnctor. Initializes the scouter with a queue for the directories to be searched and a root directory to start from
     *
     * @param directoryQueue A queue for directories to be searched
     * @param root           Root directory to start from
     */
    public Scouter(SynchronizedQueue<File> directoryQueue, File root) {
        this.directoryQueue = directoryQueue;
        this.root = root;
    }

    /**
     * Starts the scouter thread. Lists directories under root directory and adds them to queue,
     * then lists directories in the next level and enqueues them and so on.
     * This method begins by registering to the directory queue as a producer and when finishes, it unregisters from it.
     */
    @Override
    public void run() {
        this.directoryQueue.registerProducer(); // register new producer = new scouter thread

        List<File> dirs = new ArrayList<>();
        dirs.add(this.root);

        File cur_dir;
        while (!dirs.isEmpty()) {
            cur_dir = dirs.remove(0);
            File[] files = cur_dir.listFiles();
            try {
                this.directoryQueue.enqueue(cur_dir);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            if (files != null) {
                for (File file : files) {
                    if (file.isDirectory())
                        dirs.add(file);
                }
            }
        }

        this.directoryQueue.unregisterProducer();
    }
}
