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

        public static bool IsValidString(string i_String)
        {
            bool isValid = false;

            if (i_String.Length == k_LengthOfString && (AllLetters(i_String) || AllDigits(i_String)))
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

        public static bool AllDigits(string i_String)
        {
            bool allDigits = true;

            foreach (char c in i_String)
            {
                if (!IsDigit(c))
                {
                    allDigits = false;
                }
            }

            return allDigits;
        }

        public static bool IsDigit(char i_Char)
        {
            return i_Char >= '0' && i_Char <= '9';
        }

        public static bool AllLetters(string i_String)
        {
            bool isLetter = true;

            foreach (char c in i_String)
            {
                if (!IsLetter(c))
                {
                    isLetter = false;
                }
            }

            return isLetter;
        }

        public static bool IsLetter(char i_Char)
        {
            return (i_Char >= 'A' && i_Char <= 'Z') || (i_Char >= 'a' && i_Char <= 'z');
        }

        public static void AnalyzeString(string i_String)
        {
            StringBuilder outputToUser = new StringBuilder();
            outputToUser.AppendLine(string.Format("1. Is palindrome: {0}", IsPalindrome(i_String)));

            if (i_String.All(char.IsDigit))
            {
                outputToUser.AppendLine(string.Format("2. Is divisible by 3: {0}",
                    IsDivisibleByThree(i_String)));
            }
            else
            {
                outputToUser.AppendLine(string.Format("3. Number of uppercase letters: {0}",
                    CountUpperLetters(i_String)));
                outputToUser.AppendLine(string.Format("4. Is in alphabetical order: {0}",
                     IsAlphabeticalOrder(i_String)));
            }

            Console.WriteLine(outputToUser.ToString());
        }

        public static bool IsPalindromeRecursive(string i_String, int i_Start, int i_End)
        {
            bool result;

            if (i_Start >= i_End)
            {
                result = true;
            }
            else if (i_String[i_Start] != i_String[i_End])
            {
                result = false;
            }
            else
            {
                result = IsPalindromeRecursive(i_String, i_Start + 1, i_End - 1);
            }

            return result;
        }

        public static bool IsPalindrome(string i_String)
        {
            return IsPalindromeRecursive(i_String, 0, i_String.Length - 1);
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

        public static bool IsAlphabeticalOrder(string i_String)
        {
            string normalizedStr = ToUpper(i_String);
            bool isAlphabeticalOrder = true;

            for (int i = 0; i < normalizedStr.Length - 1; i++)
            {
                if (normalizedStr[i] > normalizedStr[i + 1])
                {
                    isAlphabeticalOrder = false;
                    break;
                }
            }

            return isAlphabeticalOrder;
        }

        public static string ToUpper(string i_String)
        {
            StringBuilder result = new StringBuilder(i_String.Length);

            foreach (char c in i_String)
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