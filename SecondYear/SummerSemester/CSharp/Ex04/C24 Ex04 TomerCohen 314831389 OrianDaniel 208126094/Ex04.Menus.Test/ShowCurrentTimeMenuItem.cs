using Ex04.Menus.Interfaces;
using Ex04.Menus.Events;
using System;

namespace Ex04.Menus.Test
{
    public class ShowCurrentTimeMenuItem : IMenuItemOperation
    {
        private string m_Title = "Show Current Time";

        public string Title
        {
            get { return m_Title; }
        }

        public void Execute()
        {
            Console.WriteLine($"Current Time is {DateTime.Now:HH:mm:ss}");
        }
    }
}
