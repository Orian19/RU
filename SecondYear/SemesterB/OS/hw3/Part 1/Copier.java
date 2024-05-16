import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * A copier thread. Reads files to copy from a queue and copies them to the given destination.
 */


public class Copier implements Runnable {
    public static final int COPY_BUFFER_SIZE = 4096;
    private File destination;
    private SynchronizedQueue<File> resultsQueue;

    /**
     * Constructor. Initializes the worker with a destination directory and a queue of files to copy.
     *
     * @param destination  Destination directory
     * @param resultsQueue Queue of files found, to be copied
     */
    public Copier(File destination, SynchronizedQueue<File> resultsQueue) {
        this.destination = destination;
        this.resultsQueue = resultsQueue;
    }

    /**
     * Runs the copier thread. Thread will fetch files from queue and copy them,
     * one after each other, to the destination directory.
     * When the queue has no more files, the thread finishes.
     */
    @Override
    public void run() {
        File file;
        // the queue has more files, coping them to the destination directory
        while (this.resultsQueue.getSize() > 0) {
            try {
                file = this.resultsQueue.dequeue();
                if (file != null) { // ignoring if null
                    // copying a buffered_size buffer until the entire file is copied to the destination directory
                    try (FileInputStream fis = new FileInputStream(file);
                         FileOutputStream fos = new FileOutputStream(this.destination)) {
                        byte[] buffer = new byte[COPY_BUFFER_SIZE];
                        int bytesRead;
                        while ((bytesRead = fis.read(buffer)) != -1)
                            fos.write(buffer, 0, bytesRead);
                    } catch (IOException e) {
                        throw new IOException(e);
                    }
                }
            } catch (InterruptedException | IOException e) {
                throw new RuntimeException(e);
            }

        }
    }
}
