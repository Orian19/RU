using Ex04.Menus.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex04.Menus.Events
{
    public class MainMenu
    {
        private MainMenu m_MainMenu;

        public MainMenu(string i_Title)
        {
            m_MainMenu = new MainMenu(i_Title);
        }

        public MenuItem MainMenu
        {
            get { return m_MainMenu; }
        }


    }
}
