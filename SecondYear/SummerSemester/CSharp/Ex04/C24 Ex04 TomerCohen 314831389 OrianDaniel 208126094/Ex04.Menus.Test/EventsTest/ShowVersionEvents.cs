using Ex04.Menus.Interfaces;
using Ex04.Menus.Events;
using System;

namespace Ex04.Menus.Test
{
    public class ShowVersionEvents
    {
        private readonly string r_Title = "Show Version";

        public string Title
        {
            get { return r_Title; }
        }

        public void Execute()
        {
            Console.WriteLine("App Version: 24.3.4.0495");
        }
    }
}
