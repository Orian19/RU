using System;
using System.Linq;
using System.Text;

namespace Ex01_04
{
    public class Program
    {
        public static void Main()
        {

            Console.WriteLine("Please enter a string of 8 characters:");
            string input = Console.ReadLine();

            if (IsValidString(input))
            {
                AnalyzeString(input);
            }
            else
            {
                Console.WriteLine("Invalid input. Please enter 8 characters (only letters or only digits).");
            }
            Console.ReadLine();
        }

        public static bool IsValidString(string i_InputString)
        {
            return i_InputString.Length == 8 && (AllLetters(i_InputString) || AllDigits(i_InputString));
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
            if (int.TryParse(i_Number, out int result))
            {
                isDivisible = int.Parse(i_Number) % 3 == 0;
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

        public static bool IsAlphabeticalOrder(string i_Str)
        {
            for (int i = 0; i < i_Str.Length - 1; i++)
            {
                if (i_Str[i] > i_Str[i + 1])
                {
                    return false;
                }
            }
            return true;
        }
    }
}
