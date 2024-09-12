using System;
using System.Collections.Generic;

namespace Ex04.Menus.Events
{
    public class MenuItem
    {
        public event Action<string, string> OnShow;
        public string Title { get; private set; }
        public List<MenuItem> SubMenuItems { get; private set; }

        public MenuItem(string title)
        {
            Title = title;
            SubMenuItems = new List<MenuItem>();
        }

        public void AddSubMenuItem(MenuItem item)
        {
            SubMenuItems.Add(item);
        }

        public bool IsSubMenu()
        {
            return SubMenuItems.Count > 0;
        }

        public void Show()
        {
            if (IsSubMenu())
            {
                foreach (MenuItem subItem in SubMenuItems)
                {
                    subItem.Show();
                }
            }
            else
            {
                OnShow?.Invoke(Title, "");
            }
        }
    }
}
