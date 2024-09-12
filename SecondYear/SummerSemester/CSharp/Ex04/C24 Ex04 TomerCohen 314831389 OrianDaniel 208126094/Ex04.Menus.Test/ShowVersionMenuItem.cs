﻿using Ex04.Menus.Interfaces;
using System;

namespace Ex04.Menus.Test
{
    public class ShowVersionMenuItem : IMenuItem
    {
        private string m_Title = "Show Version";
        public string Title
        {
            get { return m_Title; }
        }

        public void Show()
        {
            Console.WriteLine("App Version: 24.3.4.0495");
            Console.ReadLine();
        }
    }
}
