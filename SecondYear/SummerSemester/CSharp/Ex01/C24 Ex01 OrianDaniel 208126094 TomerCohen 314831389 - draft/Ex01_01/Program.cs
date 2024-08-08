using System;
using System.Text;

namespace Ex01_01
{
    public class Program
    {
        public static void Main()
        {
            string[] inputBinaryNumbers = new string[3];

            Console.WriteLine("Please enter 3 binary numbers with 7 digits each:");
            for (int i = 0; i < 3; i++)
            {
                inputBinaryNumbers[i] = ReadValidBinary();
            }

            BinarySeries(inputBinaryNumbers);

            Console.WriteLine("Press enter to exit...");
            Console.ReadLine();
        }

        public static string ReadValidBinary()
        {
            string input;

            do
            {
                input = Console.ReadLine();
            }
            while (!IsValidBinary(input));

            return input;
        }

        public static void BinarySeries(string[] i_Numbers)
        {
            int[] decimalNumbers = new int[3];
            int[] longestBitsSequence = new int[3];
            int palindromesCount = 0;
            int[] mostOnesInBinaryNumbers = new int[3];

            for (int i = 0; i < 3; i++)
            {
                decimalNumbers[i] = BinaryToDecimal(i_Numbers[i]);
                longestBitsSequence[i] = LongestBitsSequence(i_Numbers[i]);
                if (IsPalindrome(i_Numbers[i]) == true)
                {
                    palindromesCount++;
                }

                mostOnesInBinaryNumbers[i] = MostOnes(i_Numbers[i]);
            }

            BubbleSort(decimalNumbers);
            StringBuilder stringBuilderPrints = new StringBuilder();
            stringBuilderPrints.AppendLine(string.Format("Decimal Numbers in ascending order: {0}, {1}, {2}",
                decimalNumbers[0], decimalNumbers[1], decimalNumbers[2]));
            stringBuilderPrints.AppendLine(string.Format("Average: {0:0.##}",
                AverageOfArray(decimalNumbers)));
            stringBuilderPrints.AppendLine(string.Format("Longest bit sequence: {0}",
                MaxNumberInArray(longestBitsSequence)));
            stringBuilderPrints.AppendLine(string.Format("Number of palindromes: {0}",
                palindromesCount));
            int maxMostOnes = MaxNumberInArray(mostOnesInBinaryNumbers);
            int indexMaxOnes = IndexOf(mostOnesInBinaryNumbers, maxMostOnes);
            stringBuilderPrints.AppendLine(string.Format("The number with most 1's and least 0's: {0}",
                BinaryToDecimal(i_Numbers[indexMaxOnes])));
            Console.WriteLine(stringBuilderPrints.ToString());
        }

        public static bool IsValidBinary(string i_BinaryNumber)
        {
            bool isValid = true;
            string invalidMsg = "Invalid input, please enter new binary numbers of 7 digits";

            if (i_BinaryNumber.Length != 7)
            {
                Console.WriteLine(invalidMsg);
                isValid = false;
            }
            else
            {
                foreach (char c in i_BinaryNumber)
                {
                    if (c != '0' && c != '1')
                    {
                        Console.WriteLine(invalidMsg);
                        isValid = false;
                        break;
                    }
                }
            }

            return isValid;
        }

        public static int BinaryToDecimal(string i_BinaryNumber)
        {
            int decimalNumber = 0;

            for (int i = 0; i < i_BinaryNumber.Length; i++)
            {
                decimalNumber += (i_BinaryNumber[i] - '0') * (int)Math.Pow(2, i_BinaryNumber.Length - 1 - i);
            }

            return decimalNumber;
        }

        public static bool IsPalindrome(string i_BinaryNumber)
        {
            bool isPalindrome = true;

            for (int i = 0; i < i_BinaryNumber.Length; i++)
            {
                if (i_BinaryNumber[i] != i_BinaryNumber[i_BinaryNumber.Length - 1 - i])
                {
                    isPalindrome = false;
                    break;
                }
            }

            return isPalindrome;
        }

        public static int LongestBitsSequence(string i_BinaryNumber)
        {
            int[] dpMaxSequence = new int[i_BinaryNumber.Length];
            dpMaxSequence[0] = 1;

            for (int i = 1; i < i_BinaryNumber.Length; i++)
            {
                if (i_BinaryNumber[i] == i_BinaryNumber[i - 1])
                {
                    dpMaxSequence[i] = dpMaxSequence[i - 1] + 1;
                }
                else
                {
                    dpMaxSequence[i] = 1;
                }
            }

            return MaxNumberInArray(dpMaxSequence);
        }

        public static int MostOnes(string i_BinaryNumber)
        {
            int maxOnes = 0;

            foreach (char c in i_BinaryNumber)
            {
                if (c == '1')
                {
                    maxOnes++;
                }
            }

            return maxOnes;
        }

        public static int MaxNumberInArray(int[] i_Numbers)
        {
            int maxNumber = int.MinValue;

            for (int i = 0; i < i_Numbers.Length; i++)
            {
                if (i_Numbers[i] > maxNumber)
                {
                    maxNumber = i_Numbers[i];
                }
            }

            return maxNumber;
        }

        public static int IndexOf(int[] i_Numbers, int i_Value)
        {
            int indexOf = -1;

            for (int i = 0; i < i_Numbers.Length; i++)
            {
                if (i_Numbers[i] == i_Value)
                {
                    indexOf = i;
                    break;
                }
            }

            return indexOf;
        }

        public static double AverageOfArray(int[] i_Numbers)
        {
            double sumOfArray = 0;
            int lengthOfArray = i_Numbers.Length;

            for (int i = 0; i < lengthOfArray; i++)
            {
                sumOfArray += i_Numbers[i];
            }

            return sumOfArray / lengthOfArray;
        }

        public static void BubbleSort(int[] i_Array)
        {
            for (int i = 0; i < i_Array.Length - 1; i++)
            {
                for (int j = 0; j < i_Array.Length - i - 1; j++)
                {
                    if (i_Array[j] > i_Array[j + 1])
                    {
                        int swapTemp = i_Array[j];
                        i_Array[j] = i_Array[j + 1];
                        i_Array[j + 1] = swapTemp;
                    }
                }
            }
        }
    }
}
