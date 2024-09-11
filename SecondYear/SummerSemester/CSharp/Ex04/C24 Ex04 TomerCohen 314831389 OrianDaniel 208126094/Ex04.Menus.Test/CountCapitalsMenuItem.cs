using Ex04.Menus.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex04.Menus.Test
{
    public class CountCapitalsMenuItem : MenuItem
    {
        public CountCapitalsMenuItem() : base("Count Capitals") { }

        public override void Execute()
        {
            Console.WriteLine("Enter a sentence:");
            string input = Console.ReadLine();
            int capitalsCount = input.Count(char.IsUpper);
            Console.WriteLine($"There are {capitalsCount} uppercase letters.");
        }
    }
}
