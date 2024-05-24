import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class Main {

    public static void main(String[] args) {

        List<String> lines = getLinesFromFile();
        System.out.println("Number of lines found: " + lines.size());
        System.out.println("Starting to process");

        long startTimeWithoutThreads = System.currentTimeMillis();
        workWithoutThreads(lines);
        long elapsedTimeWithoutThreads = (System.currentTimeMillis() - startTimeWithoutThreads);
        System.out.println("Execution time: " + elapsedTimeWithoutThreads);


        long startTimeWithThreads = System.currentTimeMillis();
        workWithThreads(lines);
        long elapsedTimeWithThreads = (System.currentTimeMillis() - startTimeWithThreads);
        System.out.println("Execution time: " + elapsedTimeWithThreads);

    }

    private static void workWithThreads(List<String> lines) {
        //Your code:
        //Get the number of available cores
        //Assuming X is the number of cores - Partition the data into x data sets
        //Create a fixed thread pool of size X
        //Submit x workers to the thread pool
        //Wait for termination
        int coresNum = Runtime.getRuntime().availableProcessors(); // get number of available cores
        List<List<String>> dataSets = new ArrayList<>();

        // partition the data int coresNum data sets (using subList)
        int partitionsNum = lines.size() / coresNum;
        int startIdx = 0, endIdx;
        for (int i = 0; i < coresNum; i++) {
            endIdx = startIdx + partitionsNum;
            if (i == coresNum - 1)
                endIdx = lines.size();
            dataSets.add(lines.subList(startIdx, endIdx));
            startIdx = endIdx;
        }

        // creating a fixed thread pool of size coresNum (achieves maximum efficiency) and submit workers to the pool
        ExecutorService pool = Executors.newFixedThreadPool(coresNum);
        for (List<String> dataSet : dataSets) {
            pool.submit(new Worker(dataSet));
        }

        // wait for termination
        pool.shutdown();
        try {
            if (!pool.awaitTermination(10, TimeUnit.SECONDS)) {
                pool.shutdown();
            }
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    private static void workWithoutThreads(List<String> lines) {
        Worker worker = new Worker(lines);
        worker.run();
    }

    private static List<String> getLinesFromFile() {
        //Your code:
        //Read the shakespeare file provided from C:\Temp\Shakespeare.txt
        //and return an ArrayList<String> that contains each line read from the file.

        String filePath = "C:\\Temp\\shakespeare.txt";
        List<String> lines = new ArrayList<>();
        String line;
        // reading line by line an adding to the list
        try (BufferedReader bufferedReader = new BufferedReader(new FileReader(filePath))) {
            while ((line = bufferedReader.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return lines;
    }

}
