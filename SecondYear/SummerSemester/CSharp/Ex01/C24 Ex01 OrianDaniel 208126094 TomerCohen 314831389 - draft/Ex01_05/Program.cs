using System;
using System.Text;

namespace Ex01_05
{
    public class Program
    {
        private const int k_LengthOfNumber = 8;

        public static void Main()
        {
            Console.WriteLine(string.Format("Please enter an {0}-digit number:",
                k_LengthOfNumber));
            AnalyzeNumber(ReadValidString());

            Console.WriteLine("Press enter to exit...");
            Console.ReadLine();
        }

        public static string ReadValidString()
        {
            string input;

            do
            {
                input = Console.ReadLine();
            }
            while (!IsValidNumber(input));

            return input;
        }

        public static bool IsValidNumber(string i_InputNumber)
        {
            bool isValid = false;

            if (i_InputNumber.Length == k_LengthOfNumber && AllDigits(i_InputNumber))
            {
                isValid = true;
            }
            else
            {
                isValid = false;
                Console.WriteLine(string.Format("Invalid input. Please enter an {0}-digit number.",
                    k_LengthOfNumber));
            }

            return isValid;
        }

        public static bool AllDigits(string i_InputString)
        {
            foreach (char c in i_InputString)
            {
                if (!IsDigit(c))
                {
                    return false;
                }
            }

            return true;
        }

        public static bool IsDigit(char c)
        {
            return c >= '0' && c <= '9';
        }

        public static void AnalyzeNumber(string i_InputNumber)
        {
            int unitsDigit = GetUnitsDigit(i_InputNumber);
            int maxDigit = GetMaxDigit(i_InputNumber);
            int minDigit = GetMinDigit(i_InputNumber);
            int uniqueDigitsCount = GetUniqueDigitsCount(i_InputNumber);

            StringBuilder result = new StringBuilder();
            result.AppendLine(string.Format("A. Number of digits smaller than the units digit ({0}): {1}",
                unitsDigit, CountDigitsSmallerThanUnitsDigit(i_InputNumber, unitsDigit)));
            result.AppendLine(string.Format("B. Number of digits divisible by 3 without remainder: {0}",
                CountDigitsDivisibleByThree(i_InputNumber)));
            result.AppendLine(string.Format("C. Difference between the largest and smallest digits: {0}",
                maxDigit - minDigit));
            result.AppendLine(string.Format("D. Number of unique digits: {0}", uniqueDigitsCount));

            Console.WriteLine(result.ToString());
        }

        public static int GetUnitsDigit(string i_InputNumber)
        {
            return i_InputNumber[i_InputNumber.Length - 1] - '0';
        }

        public static int GetMaxDigit(string i_InputNumber)
        {
            int maxDigit = 0;

            foreach (char digit in i_InputNumber)
            {
                int currentDigit = digit - '0';
                if (currentDigit > maxDigit)
                {
                    maxDigit = currentDigit;
                }
            }

            return maxDigit;
        }

        public static int GetMinDigit(string i_InputNumber)
        {
            int minDigit = 9;

            foreach (char digit in i_InputNumber)
            {
                int currentDigit = digit - '0';
                if (currentDigit < minDigit)
                {
                    minDigit = currentDigit;
                }
            }

            return minDigit;
        }

        public static int GetUniqueDigitsCount(string i_InputNumber)
        {
            bool[] seenDigits = new bool[10];
            int uniqueCount = 0;

            foreach (char digit in i_InputNumber)
            {
                int digitValue = digit - '0';
                if (!seenDigits[digitValue])
                {
                    seenDigits[digitValue] = true;
                    uniqueCount++;
                }
            }

            return uniqueCount;
        }

        public static int CountDigitsSmallerThanUnitsDigit(string i_InputNumber, int i_UnitsDigit)
        {
            int count = 0;

            foreach (char digit in i_InputNumber)
            {
                if (digit - '0' < i_UnitsDigit)
                {
                    count++;
                }
            }

            return count;
        }

        public static int CountDigitsDivisibleByThree(string i_InputNumber)
        {
            int count = 0;

            foreach (char digit in i_InputNumber)
            {
                if ((digit - '0') % 3 == 0)
                {
                    count++;
                }
            }

            return count;
        }
    }
}
