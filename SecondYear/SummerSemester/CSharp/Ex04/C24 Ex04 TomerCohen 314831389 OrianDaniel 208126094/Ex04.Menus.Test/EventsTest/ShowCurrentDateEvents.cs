using Ex04.Menus.Interfaces;
using Ex04.Menus.Events;
using System;

namespace Ex04.Menus.Test
{
    public class ShowCurrentDateEvents
    {
        private readonly string r_Title = "Show Current Date/Time";

        public string Title
        {
            get { return r_Title; }
        }

        public void Execute()
        {
            Console.WriteLine($"Current Date is {DateTime.Now:dd/MM/yyyy}");
        }
    }
}
