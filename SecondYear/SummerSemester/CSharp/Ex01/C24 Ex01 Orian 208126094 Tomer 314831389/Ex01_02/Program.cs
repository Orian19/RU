using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex01_02
{
    class Program
    {
        public static void Main()
        {
            Console.WriteLine("Iterative ABC tree");
            PrintABCTreeIterative();
            Console.WriteLine("Recursive ABC tree");
            PrintABCTreeRecursive(1, 'A');
            Console.ReadLine();
        }

        public static void PrintABCTreeIterative()
        {
            char letter = 'A';
            StringBuilder treeOutput = new StringBuilder();

            for (int row = 1; row < 8; row++)
            {
                treeOutput.Append(row);
                if (row < 6)
                {
                    int numberOfSpacesInEachSide = 5 - row;
                    treeOutput.Append(new string(' ', numberOfSpacesInEachSide));
                    int lettersInRow = row * 2 - 1;
                    for (int i = 0; i < lettersInRow; i++)
                    {
                        treeOutput.Append(letter + " ");
                        letter++;
                    }
                    treeOutput.Append(new string(' ', numberOfSpacesInEachSide));
                    treeOutput.AppendLine();
                    treeOutput.AppendLine();
                }
                else
                {
                    treeOutput.Append(new string(' ', 4));
                    treeOutput.Append("|" + letter + "|");
                    treeOutput.Append(new string(' ', 4));
                    treeOutput.AppendLine();
                    treeOutput.AppendLine();
                }
            }

            Console.WriteLine(treeOutput.ToString());
        }

        public static void PrintABCTreeRecursive(int row, char letter)
        {
            if (row > 7) return;

            Console.Write(row);
            StringBuilder rowOutput = new StringBuilder();

            if (row < 6)
            {
                int numberOfSpacesInEachSide = 5 - row;
                rowOutput.Append(new string(' ', numberOfSpacesInEachSide));
                int lettersInRow = row * 2 - 1;
                for (int i = 0; i < lettersInRow; i++)
                {
                    rowOutput.Append(letter + " ");
                    letter++;
                }
                rowOutput.Append(new string(' ', numberOfSpacesInEachSide));
                rowOutput.AppendLine();
            }
            else
            {
                rowOutput.Append(new string(' ', 4));
                rowOutput.Append("|" + letter + "|");
                rowOutput.Append(new string(' ', 4));
                rowOutput.AppendLine();
            }

            Console.WriteLine(rowOutput.ToString());
            PrintABCTreeRecursive(row + 1, letter);
        }
    }
}