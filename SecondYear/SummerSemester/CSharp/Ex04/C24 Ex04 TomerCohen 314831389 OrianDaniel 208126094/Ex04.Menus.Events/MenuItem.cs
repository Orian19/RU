﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex04.Menus.Interfaces
{
    public class MenuItem
    {
        private string m_Title;
        private readonly List<MenuItem> r_SubMenuItems;

        public MenuItem(string i_Title)
        {
            m_Title = i_Title;
            r_SubMenuItems = new List<MenuItem>();
        }

        public string Title
        {
            get { return m_Title; }
        }

        public List<MenuItem> SubMenuItems
        {
            get { return r_SubMenuItems; }
        }

        public void AddSubMenuItem(MenuItem i_Item)
        {
            r_SubMenuItems.Add(i_Item);
        }

        public bool IsSubMenu()
        {
            return r_SubMenuItems.Count > 0;
        }
    }
}
