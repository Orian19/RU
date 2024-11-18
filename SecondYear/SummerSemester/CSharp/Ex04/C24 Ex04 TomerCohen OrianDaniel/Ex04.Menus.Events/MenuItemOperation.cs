namespace Ex04.Menus.Events
{
    public delegate void MenuItemSelectedOperation();

    public class MenuItemOperation : MenuItem
    {
        public event MenuItemSelectedOperation SelectedOperation;

        public MenuItemOperation(string i_Title) : base(i_Title)
        {
        }

        public void DoOnSelectedOperation()
        {
            OnSelectedOperation();
        }

        protected virtual void OnSelectedOperation()
        {
            SelectedOperation?.Invoke();
        }
    }
}
