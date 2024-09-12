using System;
using System.Collections.Generic;
using System.Linq;

namespace Ex04.Menus.Interfaces
{
    public abstract class Menu : IMenuItem
    {
        public string Title { get; }
        public List<IMenuItem> SubMenuItems { get; }
        private bool m_IsMainMenu;

        public Menu(string title, List<IMenuItem> items, bool i_IsMainMenu)
        {
            Title = title;
            SubMenuItems = items.ToList();
            m_IsMainMenu = i_IsMainMenu;
        }

        public void Show()
        {
            while (true)
            {
                DisplayCurrentMenu();

                int choice = GetUserChoice();

                if (choice == 0)
                {
                    break;
                }
                else
                {
                    HandleUserChoice(choice);
                }
            }
        }

        private void DisplayCurrentMenu()
        {
            Console.Clear();
            Console.WriteLine($"** {Title} **");
            Console.WriteLine(new string('-', Title.Length + 6));

            for (int i = 0; i < SubMenuItems.Count; i++)
            {
                Console.WriteLine($"{i + 1}. {SubMenuItems[i].Title}");
            }

            Console.WriteLine(m_IsMainMenu ? "0. Exit" : "0. Back");
        }

        private int GetUserChoice()
        {
            while (true)
            {
                Console.WriteLine($"Please enter your choice (1-{SubMenuItems.Count} or 0 to {(m_IsMainMenu ? "exit" : "go back")}):");
                if (int.TryParse(Console.ReadLine(), out int choice) && choice >= 0 && choice <= SubMenuItems.Count)
                {
                    return choice;
                }

                Console.WriteLine("Invalid input, please try again.");
            }
        }

        private void HandleUserChoice(int i_Choice)
        {
            IMenuItem selectedItem = SubMenuItems[i_Choice - 1];
            selectedItem.Show();
        }
    }
}