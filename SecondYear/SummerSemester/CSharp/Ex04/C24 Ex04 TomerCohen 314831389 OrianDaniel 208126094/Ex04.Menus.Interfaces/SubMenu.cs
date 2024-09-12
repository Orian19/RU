using System.Collections.Generic;

namespace Ex04.Menus.Interfaces
{
    public class SubMenu : Menu
    {
        public SubMenu(string title, List<IMenuItem> items) : base(title, items, false)
        {
        }
    }
}