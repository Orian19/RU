﻿using Ex04.Menus.Interfaces;
using System;

namespace Ex04.Menus.Test
{
    public class ShowCurrentTimeMenuItem : IMenuItem
    {
        private string m_Title = "Show Current Time";

        public string Title
        {
            get { return m_Title; }
        }

        public void Show()
        {
            Console.WriteLine($"Current Time is {DateTime.Now:HH:mm:ss}");
            Console.ReadLine();
        }
    }
}
