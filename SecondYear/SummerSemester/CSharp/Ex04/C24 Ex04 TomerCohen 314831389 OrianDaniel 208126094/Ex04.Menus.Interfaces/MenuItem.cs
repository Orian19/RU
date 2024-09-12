using System.Collections.Generic;

namespace Ex04.Menus.Interfaces
{
    public class MenuItem : Menu
    {
        public MenuItem(string title, List<IMenuItem> items) : base(title, items, false)
        {
        }
    }
}