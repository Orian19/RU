using System.Collections.Generic;

namespace Ex04.Menus.Interfaces
{
    public class MainMenu : Menu
    {
        public MainMenu(string title, List<IMenuItem> items) : base(title, items, true)
        {
        }
    }
}