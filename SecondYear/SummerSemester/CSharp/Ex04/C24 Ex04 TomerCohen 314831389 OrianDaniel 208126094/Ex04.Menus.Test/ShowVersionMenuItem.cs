using Ex04.Menus.Interfaces;
using Ex04.Menus.Events;
using System;

namespace Ex04.Menus.Test
{
    public class ShowVersionMenuItem : IMenuItemOperation
    {
        private string m_Title = "Show Version";
        public string Title
        {
            get { return m_Title; }
        }

        public void Execute()
        {
            Console.WriteLine("App Version: 24.3.4.0495");
        }
    }
}
