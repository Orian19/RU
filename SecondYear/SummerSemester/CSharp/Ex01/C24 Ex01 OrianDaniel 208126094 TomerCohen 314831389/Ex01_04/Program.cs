using System;
using System.Linq;
using System.Text;

namespace Ex01_04
{
    public class Program
    {
        private const int k_LengthOfString = 8;
        public static void Main()
        {
            Console.WriteLine(string.Format("Please enter a string of {0} characters:", k_LengthOfString));
            AnalyzeString(ReadValidString());

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
            while (!IsValidString(input));

            return input;
        }

        public static bool IsValidString(string i_InputString)
        {
            bool isValid = false;

            if (i_InputString.Length == k_LengthOfString && (AllLetters(i_InputString) || AllDigits(i_InputString)))
            {
                isValid = true;
            }
            else
            {
                Console.WriteLine(string.Format("Invalid input. Please enter {0} characters (only letters or only digits).",
                    k_LengthOfString));
                isValid = false;
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

        public static bool AllLetters(string i_InputString)
        {
            foreach (char c in i_InputString)
            {
                if (!IsLetter(c))
                {
                    return false;
                }
            }

            return true;
        }

        public static bool IsLetter(char c)
        {
            return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
        }

        public static void AnalyzeString(string i_InputString)
        {
            StringBuilder outputToUser = new StringBuilder();
            outputToUser.AppendLine(string.Format("1. Is palindrome: {0}", IsPalindrome(i_InputString)));

            if (i_InputString.All(char.IsDigit))
            {
                outputToUser.AppendLine(string.Format("2. Is divisible by 3: {0}",
                    IsDivisibleByThree(i_InputString)));
            }
            else
            {
                outputToUser.AppendLine(string.Format("3. Number of uppercase letters: {0}",
                    CountUpperLetters(i_InputString)));
                outputToUser.AppendLine(string.Format("4. Is in alphabetical order: {0}",
                     IsAlphabeticalOrder(i_InputString)));
            }

            Console.WriteLine(outputToUser.ToString());
        }

        public static bool IsPalindromeRecursive(string i_InputString, int i_Start, int i_End)
        {
            bool result;

            if (i_Start >= i_End)
            {
                result = true;
            }
            else if (i_InputString[i_Start] != i_InputString[i_End])
            {
                result = false;
            }
            else
            {
                result = IsPalindromeRecursive(i_InputString, i_Start + 1, i_End - 1);
            }

            return result;
        }

        public static bool IsPalindrome(string i_InputString)
        {
            return IsPalindromeRecursive(i_InputString, 0, i_InputString.Length - 1);
        }

        public static bool IsDivisibleByThree(string i_Number)
        {
            bool isDivisible = false;

            if (int.TryParse(i_Number, out int parsedNumber))
            {
                isDivisible = parsedNumber % 3 == 0;
            }

            return isDivisible;
        }

        public static int CountUpperLetters(string i_StringLetters)
        {
            int count = 0;

            foreach (char c in i_StringLetters)
            {
                if (IsUpper(c))
                {
                    count++;
                }
            }

            return count;
        }

        public static bool IsUpper(char i_Char)
        {
            return i_Char >= 65 && i_Char <= 90;
        }

        public static bool IsAlphabeticalOrder(string i_InputString)
        {
            string normalizedStr = ToUpper(i_InputString);

            for (int i = 0; i < normalizedStr.Length - 1; i++)
            {
                if (normalizedStr[i] > normalizedStr[i + 1])
                {
                    return false;
                }
            }

            return true;
        }

        public static string ToUpper(string i_InputString)
        {
            StringBuilder result = new StringBuilder(i_InputString.Length);

            foreach (char c in i_InputString)
            {
                if (c >= 'a' && c <= 'z')
                {
                    result.Append((char)(c - 'a' + 'A'));
                }
                else
                {
                    result.Append(c);
                }
            }

            return result.ToString();
        }
    }
}
