using Ex04.Menus.Interfaces;
using System;

namespace Ex04.Menus.Test
{
    public class ShowCurrentTimeInterfaces : IMenuItemOperation
    {
        private readonly string r_Title = "Show Current Time";

        public string Title
        {
            get { return r_Title; }
        }

        public void Execute()
        {
            Console.WriteLine($"Current Time is {DateTime.Now:HH:mm:ss}");
        }
    }
}
