using System;
using System.Collections.Generic;

namespace Ex04.Menus.Events
{
    public class MainMenu
    {
        private readonly List<MenuItem> m_MenuItems;
        private readonly string m_Title;

        public MainMenu(string title)
        {
            m_Title = title;
            m_MenuItems = new List<MenuItem>();
        }

        public void AddMenuItem(MenuItem item)
        {
            m_MenuItems.Add(item);
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
                    MenuItem selectedItem = m_MenuItems[choice - 1];
                    if (selectedItem.IsSubMenu())
                    {
                        selectedItem.Show();
                    }
                    else
                    {
                    }
                }
            }
        }

        private void DisplayCurrentMenu()
        {
            Console.Clear();
            Console.WriteLine($"** {m_Title} **");
            Console.WriteLine(new string('-', m_Title.Length + 6));

            for (int i = 0; i < m_MenuItems.Count; i++)
            {
                Console.WriteLine($"{i + 1}. {m_MenuItems[i].Title}");
            }

            Console.WriteLine("0. Exit");
        }

        private int GetUserChoice()
        {
            while (true)
            {
                Console.WriteLine($"Please enter your choice (1-{m_MenuItems.Count} or 0 to exit):");
                if (int.TryParse(Console.ReadLine(), out int choice) && choice >= 0 && choice <= m_MenuItems.Count)
                {
                    return choice;
                }

                Console.WriteLine("Invalid input, please try again.");
            }
        }
    }
}
