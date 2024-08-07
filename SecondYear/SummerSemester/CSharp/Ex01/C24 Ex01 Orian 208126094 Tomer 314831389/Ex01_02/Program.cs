using System;
using System.Text;

namespace Ex01_02
{
    public class Program
    {
        public static void Main()
        {
            Console.WriteLine("Iterative ABC tree");
            PrintABCTreeIterative(7);
            Console.WriteLine("Recursive ABC tree");
            PrintABCTreeRecursive(7);
            Console.ReadLine();
        }

        public static void PrintABCTreeIterative(int i_Height)
        {
            char letter = 'A';
            StringBuilder treeOutput = new StringBuilder();

            for (int row = 1; row <= i_Height; row++)
            {
                treeOutput.Append(row);
                treeOutput.Append("\t");
                if (row < i_Height - 1)
                {
                    int lettersInRow = row * 2 - 1;
                    // The number of letters in row with max letter is:
                    // a_heigth = a_1 + (height-3)d = 2*heigth - 5 ( d = 2 a_1 = 1 ) and +1 for |
                    int numberOfSpacesInEachSide = (Math.Max((i_Height * 2) - 5, 0) - lettersInRow + 1);
                    treeOutput.Append(new string(' ', numberOfSpacesInEachSide));
                    for (int i = 0; i < lettersInRow; i++)
                    {
                        treeOutput.Append(letter + " ");
                        letter = (char)((letter - 'A' + 1) % 26 + 'A');
                    }

                    treeOutput.Append(new string(' ', numberOfSpacesInEachSide));
                    treeOutput.AppendLine();
                    treeOutput.AppendLine();
                }
                else
                {
                    treeOutput.Append(new string(' ', Math.Max((i_Height * 2) - 6, 0)));
                    treeOutput.Append("|" + letter + "|");
                    treeOutput.Append(new string(' ', Math.Max((i_Height * 2) - 6, 0)));
                    treeOutput.AppendLine();
                    treeOutput.AppendLine();
                }
            }

            Console.WriteLine(treeOutput.ToString());
        }

        public static void PrintABCTreeRecursive(int i_Height)
        {
            PrintABCTreeRecursive(1, 'A', i_Height);
        }

        public static void PrintABCTreeRecursive(int i_Row, char i_Letter, int i_Height)
        {
            if (i_Row > i_Height) return;

            StringBuilder treeOutput = new StringBuilder();
            treeOutput.Append(i_Row);
            treeOutput.Append("\t");

            if (i_Row < i_Height - 1)
            {
                int lettersInRow = i_Row * 2 - 1;
                // The number of letters in row with max letter is:
                // a_heigth = a_1 + (height-3)d = 2*heigth - 5 ( d = 2 a_1 = 1 ) +1 for |
                int numberOfSpacesInEachSide = Math.Max((i_Height * 2) - 5 - lettersInRow + 1, 0);

                treeOutput.Append(new string(' ', numberOfSpacesInEachSide));
                for (int i = 0; i < lettersInRow; i++)
                {
                    treeOutput.Append(i_Letter + " ");
                    i_Letter = (char)((i_Letter - 'A' + 1) % 26 + 'A');
                }

                treeOutput.Append(new string(' ', numberOfSpacesInEachSide));
                treeOutput.AppendLine();
            }
            else
            {
                treeOutput.Append(new string(' ', Math.Max((i_Height * 2) - 6, 0)));
                treeOutput.Append("|" + i_Letter + "|");
                treeOutput.Append(new string(' ', Math.Max((i_Height * 2) - 6, 0)));
                treeOutput.AppendLine();
            }

            Console.WriteLine(treeOutput.ToString());
            PrintABCTreeRecursive(i_Row + 1, i_Letter, i_Height);
        }
    }
}
