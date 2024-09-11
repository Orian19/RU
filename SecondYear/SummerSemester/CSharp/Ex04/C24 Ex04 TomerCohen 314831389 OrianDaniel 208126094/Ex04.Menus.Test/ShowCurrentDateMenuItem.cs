using Ex04.Menus.Interfaces;
using System;


namespace Ex04.Menus.Test
{
    public class ShowCurrentDateMenuItem : MenuItem
    {
        public ShowCurrentDateMenuItem() : base("Show Current Date") { }

        public override void Execute()
        {
            Console.WriteLine($"Current Date is {DateTime.Now:dd/MM/yyyy}");
        }
    }
}
