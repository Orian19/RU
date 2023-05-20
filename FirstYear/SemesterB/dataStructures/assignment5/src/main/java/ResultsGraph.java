import javax.swing.*;
import java.awt.*;
import java.awt.geom.Line2D;

public class ResultsGraph {

    private static final String[] ALGORITHM_NAMES = {
            "Merge Sort Recursive", "Merge Sort Iterative", "Quick Sort Class",
            "Quick Sort Recitation", "Radix Sort", "Arrays.sort"
    };

    private static final Color[] ALGORITHM_COLORS = {
            Color.RED, Color.BLUE, Color.GREEN, Color.ORANGE, Color.MAGENTA, Color.CYAN
    };

    static void showGraph(double[][] averageList) {
        JFrame frame = new JFrame("Sorting Algorithm Durations");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(800, 600);

        JPanel panel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2d = (Graphics2D) g;

                int width = getWidth();
                int height = getHeight();

                // Calculate the maximum average to scale the graph
                double maxAverage = 0;
                for (int i = 0; i < averageList.length; i++) {
                    for (int j = 0; j < averageList[i].length; j++) {
                        maxAverage = Math.max(maxAverage, averageList[i][j]);
                    }
                }

                // Draw the graph lines
                for (int i = 0; i < averageList.length; i++) {
                    g2d.setColor(ALGORITHM_COLORS[i]);
                    g2d.setStroke(new BasicStroke(2)); // Set line thickness to bold
                    int prevX = 50, prevY = height - 50;
                    for (int j = 0; j < averageList[i].length; j++) {
                        int x = (int) ((double) j / (averageList[i].length - 1) * (width - 100)) + 50;
                        int y = height - (int) ((double) averageList[i][j] / maxAverage * (height - 100)) - 50;
                        if (j > 0) {
                            g2d.draw(new Line2D.Double(prevX, prevY, x, y));
                        }
                        prevX = x;
                        prevY = y;
                    }
                }

                // Draw legends
                int legendX = width - 150;
                int legendY = 50;
                for (int i = 0; i < ALGORITHM_NAMES.length; i++) {
                    g2d.setColor(ALGORITHM_COLORS[i]);
                    g2d.drawString(ALGORITHM_NAMES[i], legendX, legendY);
                    legendY += 20;
                }

                // Draw axes
                g2d.setColor(Color.BLACK);
                g2d.drawLine(50, height - 50, 50, 50); // y-axis
                g2d.drawLine(50, height - 50, width - 50, height - 50); // x-axis

                // Draw scale and numbers on axes
                g2d.setFont(new Font("Arial", Font.PLAIN, 10));
                int scaleSteps = 5;
                double yScale = maxAverage / scaleSteps;
                int yScaleHeight = (height - 100) / scaleSteps;
                for (int i = 0; i <= scaleSteps; i++) {
                    int y = height - 50 - yScaleHeight * i;
                    g2d.drawLine(45, y, 55, y);
                    g2d.drawString(String.format("%.2f", yScale * i), 30, y + 5);
                }
                for (int i = 0; i < averageList[0].length; i++) {
                    int x = (int) ((double) i / (averageList[0].length - 1) * (width - 100)) + 50;
                    int inputSize = (int) ((double) i / (averageList[0].length - 1) * (averageList[0].length - 1)) + 1;
                    g2d.drawLine(x, height - 45, x, height - 55);
                    g2d.drawString(Integer.toString(inputSize), x - 5, height - 30);
                }

                // Draw titles
                g2d.setFont(new Font("Arial", Font.BOLD, 16));
                g2d.drawString("Sorting Algorithm Durations", width / 2 - 160, 30);
                g2d.setFont(new Font("Arial", Font.PLAIN, 12));
                g2d.drawString("Input Size", width - 60, height - 30);
                g2d.drawString("Duration (ms)", 10, 40);
            }
        };

        frame.getContentPane().add(panel);
        frame.setVisible(true);
    }
}
