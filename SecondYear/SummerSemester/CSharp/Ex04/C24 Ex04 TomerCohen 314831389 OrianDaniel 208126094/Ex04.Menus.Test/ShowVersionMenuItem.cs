using Ex04.Menus.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex04.Menus.Test
{
    public class ShowVersionMenuItem : MenuItem
    {
        public ShowVersionMenuItem() : base("Show Version") { }

        public override void Execute()
        {
            Console.WriteLine("App Version: 24.3.4.0495");
        }
    }
}
