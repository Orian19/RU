using Ex04.Menus.Interfaces;
using System;

namespace Ex04.Menus.Test
{
    public class ShowCurrentDateInterfaces : IMenuItemOperation
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
