using Ex04.Menus.Events;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex04.Menus.Interfaces
{
    public interface IMenuItem
    {
        string Title { get; }
        void Execute();
        List<IMenuItem> SubMenuItems { get; }
        bool IsSubMenu();
    }
}
