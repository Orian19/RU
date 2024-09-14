using System;
using System.Collections.Generic;

namespace Ex04.Menus.Events
{
    public class MenuItem
    {
        private readonly string r_Title;
        private readonly List<MenuItem> r_SubMenuItems;
        private MenuItemOperation m_Operation;
        private readonly bool r_IsMainMenu;

        public MenuItem(string i_Title, bool i_IsMainMenu = false)
        {
            r_Title = i_Title;
            r_SubMenuItems = new List<MenuItem>();
            r_IsMainMenu = i_IsMainMenu;
        }

        public string Title
        {
            get { return r_Title; }
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
                else if (choice > 0 && choice <= r_SubMenuItems.Count)
                {
                    MenuItem selectedItem = r_SubMenuItems[choice - 1];
                    if (selectedItem.m_Operation != null)
                    {
                        selectedItem.m_Operation.DoOnSelectedOperation();
                        Console.WriteLine();
                        Console.WriteLine("Press Enter to continue...");
                        Console.ReadLine();
                    }
                    else
                    {
                        selectedItem.Show();
                    }
                }
            }
        }

        public MenuItem AddMenuItem(string i_Title)
        {
            MenuItem newItem = new MenuItem(i_Title);
            r_SubMenuItems.Add(newItem);

            return newItem;
        }

        public void AddOperation(MenuItemOperation i_Operation)
        {
            m_Operation = i_Operation;
        }

        private void DisplayCurrentMenu()
        {
            Console.Clear();
            Console.WriteLine($"** {r_Title} **");
            Console.WriteLine(new string('-', r_Title.Length + 6));

            for (int i = 0; i < r_SubMenuItems.Count; i++)
            {
                Console.WriteLine($"{i + 1}. {r_SubMenuItems[i].Title}");
            }

            Console.WriteLine(r_IsMainMenu ? "0. Exit" : "0. Back");
        }

        private int GetUserChoice()
        {
            while (true)
            {
                Console.WriteLine($"Please enter your choice (1-{r_SubMenuItems.Count} or 0 to {(r_IsMainMenu ? "exit" : "go back")}):");
                if (int.TryParse(Console.ReadLine(), out int choice) && choice >= 0 && choice <= r_SubMenuItems.Count)
                {
                    return choice;
                }

                Console.WriteLine("Invalid input, please try again.");
            }
        }
    }
}
