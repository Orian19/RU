using Ex04.Menus.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex04.Menus.Test
{
    public class ShowCurrentTimeMenuItem : MenuItem
    {
        public ShowCurrentTimeMenuItem() : base("Show Current Time") { }

        public override void Execute()
        {
            Console.WriteLine($"Current Time is {DateTime.Now:HH:mm:ss}");
        }
    }
}
