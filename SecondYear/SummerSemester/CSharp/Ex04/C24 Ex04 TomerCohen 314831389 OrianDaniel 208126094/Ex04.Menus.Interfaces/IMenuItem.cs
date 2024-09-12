namespace Ex04.Menus.Interfaces
{
    public interface IMenuItem
    {
        string Title { get; }
        void Show();
        IMenuItem AddMenuItem(string i_Title);
        void AddOperation(IMenuItemOperation i_Opertation);
    }
}
