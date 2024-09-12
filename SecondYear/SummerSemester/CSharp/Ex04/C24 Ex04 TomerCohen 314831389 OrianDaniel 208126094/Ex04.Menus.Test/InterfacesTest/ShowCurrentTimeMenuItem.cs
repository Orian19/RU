using Ex04.Menus.Interfaces;
using System;

namespace Ex04.Menus.Test.InterfacesTest
{
    public class ShowCurrentTimeMenuItem : IMenuItem
    {
        private readonly string r_Title = "Show Current Time";

        public string Title
        {
            get { return r_Title; }
        }

        public void Show()
        {
            Console.WriteLine($"Current Time is {DateTime.Now:HH:mm:ss}");
            Console.ReadLine();
        }
    }
}
