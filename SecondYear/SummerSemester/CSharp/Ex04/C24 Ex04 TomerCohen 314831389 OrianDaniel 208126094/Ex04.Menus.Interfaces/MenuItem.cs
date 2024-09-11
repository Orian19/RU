using System;
using System.Collections.Generic;

namespace Ex04.Menus.Interfaces
{
    public class MenuItem : IMenuItem
    {
        public string Title { get; private set; }
        public List<IMenuItem> SubMenuItems { get; private set; }

        public MenuItem(string title)
        {
            Title = title;
            SubMenuItems = new List<IMenuItem>();
        }

        public void AddSubMenuItem(IMenuItem item)
        {
            SubMenuItems.Add(item);
        }

        public bool IsSubMenu()
        {
            return SubMenuItems.Count > 0;
        }

        public virtual void Execute()
        {
        }
    }
}
