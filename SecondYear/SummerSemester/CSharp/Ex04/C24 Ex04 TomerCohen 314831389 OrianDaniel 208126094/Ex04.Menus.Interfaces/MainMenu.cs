using System.Collections.Generic;

namespace Ex04.Menus.Interfaces
{
    public class MainMenu: IMenuItem
    {
        private readonly IMenuItem r_MainMenu;

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

        public IMenuItem AddMenuItem(string i_Title)
        {
            return r_MainMenu.AddMenuItem(i_Title);
        }

        public void AddOperation(IMenuItemOperation i_Operation)
        {
            r_MainMenu.AddOperation(i_Operation);
        }
    }
}
