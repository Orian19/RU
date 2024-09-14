using System;

namespace Ex04.Menus.Interfaces
{
    public class MainMenu : MenuItem
    {
        public MainMenu(string i_Title) : base(i_Title)
        {
        }

        protected override void DisplayMenu()
        {
            base.DisplayMenuContent("0. Exit");
        }

        protected override int GetUserChoice()
        {
            return base.GetUserChoiceInput("exit");
        }
    }
}
