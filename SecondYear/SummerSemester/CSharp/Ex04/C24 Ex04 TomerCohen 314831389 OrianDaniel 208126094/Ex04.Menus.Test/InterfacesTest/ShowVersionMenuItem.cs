using Ex04.Menus.Interfaces;
using System;

namespace Ex04.Menus.Test.InterfacesTest
{
    public class ShowVersionMenuItem : IMenuItem
    {
        private readonly string r_Title = "Show Version";
        public string Title
        {
            get { return r_Title; }
        }

        public void Show()
        {
            Console.WriteLine("App Version: 24.3.4.0495");
            Console.ReadLine();
        }
    }
}
