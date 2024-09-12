﻿using Ex04.Menus.Interfaces;
using System;

namespace Ex04.Menus.Test
{
    public class ShowCurrentDateMenuItem : IMenuItem
    {
        private string m_Title = "Show Current Date/Time";

        public string Title
        {
            get { return m_Title; }
        }

        public void Show()
        {
            Console.WriteLine($"Current Date is {DateTime.Now:dd/MM/yyyy}");
            Console.ReadLine();
        }
    }
}
