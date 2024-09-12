using System;
using System.Collections.Generic;

namespace Ex04.Menus.Events
{
    public class MainMenu
    {
        private readonly MenuItem r_MainMenu;

        public MainMenu(string i_Title)
        {
            r_MainMenu = new MenuItem(i_Title, true);
        }

        public string Title
        {
            get { return r_MainMenu.Title; }
        }

        public void Show()
        {
            r_MainMenu.Show();
        }

        public MenuItem AddMenuItem(string i_Title)
        {
            return r_MainMenu.AddMenuItem(i_Title);
        }
    }
}
