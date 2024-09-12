using Ex04.Menus.Interfaces;
using System;
using System.Linq;

namespace Ex04.Menus.Test.InterfacesTest
{
    public class CountCapitalsMenuItem : IMenuItem
    {
        private readonly string r_Title = "Count Capitals";

        public string Title
        {
            get { return r_Title; }
        }

        public void Show()
        {
            Console.WriteLine("Enter a sentence:");
            string input = Console.ReadLine();
            int capitalsCount = input.Count(char.IsUpper);
            Console.WriteLine($"There are {capitalsCount} uppercase letters.");
            Console.ReadLine();
        }
    }
}
