using System.Collections.Generic;

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
