import java.lang.reflect.Array;
import java.util.Collections;
import java.util.Random;
import java.util.Arrays;

public class Main {
    public static void main(String[] args){
        System.out.println("Starting...");

        int[] inputs = {10000, 50000, 100000, 500000, 1000000};
        int[] thresholds = {};

        Sort[] sortAlgo = {new Sort()};

        Sort s = new Sort<Integer>();

        int NUMITER = 100;
        double[][] durationList = new double[6][NUMITER];

        for (int n = 0; n < inputs.length; n++) {  // input size
            for (int k = 0; k < NUMITER; k++) { // iterations for each algo
                int [] rndArrA = randomInputs(inputs[k]);
                int [] sortedIncArrA = Arrays.sort(rndArrA);
                int [] sortedDecArrA = Arrays.sort(rndArrA, Collections.reverseOrder());
            }
            for (int i = 0; i < 6; i++) {
                int [] rndArrB = int[rndArrA.length];
                int [] sortedIncArrB = int[sortedIncArrA.length];
                int [] sortedDecArrB = int[sortedDecArrA.length];

                System.arraycopy(rndArrA, 0, rndArrB, 0, rndArrB.length);
                System.arraycopy(sortedIncArrA, 0, sortedIncArrB, 0, rndArrB.length);
                System.arraycopy(sortedDecArrA, 0, sortedDecArrB, 0, rndArrB.length);

                long startTime = System.currentTimeMillis();
                s.setNaiveSortThreshold(thresholds[i]);
                sortAlgo[i].(rndArrB);
                sortAlgo[i].(sortedIncArrB);
                sortAlgo[i].(sortedDecArrB);
                long endTime = System.currentTimeMillis();
                durationList[i][k]= ((double) (endTime - startTime));
            }
        }

        double[] sum = new double[6];
        double[] std = new double[6];
        for (int i = 0; i < durationList.length; i++) {
            for (int j = 0; j < durationList[0].length; j++) {
                double curValue = durationList[i][j];
                sum[i] += curValue;
                std[i] += Math.pow(curValue - sum)
            }
            double avg = sum[i]/NUMITER;

            System.out.println("The average running time for - " + durationList[i] + "is: " + avg);
            System.out.println("The standard deviation for - " + durationList[i] + "is: " + std[i]);
        }
    }

    //TODO: fix all functions the answer need of range [0, Integer.MAX_VALUE]

    /**
     * random array of integers in range [0, Integer.MAX_VALUE]
     * @param size of input
     * @return randomized array
     */
    public static  int[] randomInputs(int size) {
        int [] nums = new int [size];
        Random randomGenerator = new Random();
        for(int i = 0 ; i < nums.length; i++){
            nums[i] = randomGenerator.nextInt(Integer.MAX_VALUE);
        }
        return nums;
    }
}
