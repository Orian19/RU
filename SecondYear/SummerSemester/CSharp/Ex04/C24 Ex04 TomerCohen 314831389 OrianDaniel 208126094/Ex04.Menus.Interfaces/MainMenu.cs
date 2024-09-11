using Ex04.Menus.Interfaces;
using System;
using System.Collections.Generic;

public class MainMenu : IMenuItem
{
    public string Title { get; private set; }
    public List<IMenuItem> SubMenuItems { get; private set; }

    public MainMenu(string title)
    {
        Title = title;
        SubMenuItems = new List<IMenuItem>();
    }

    public void AddMenuItem(IMenuItem item)
    {
        SubMenuItems.Add(item);
    }

    public bool IsSubMenu()
    {
        return SubMenuItems.Count > 0;
    }

    public void Execute()
    {
        ShowMenu(this);
    }

    private void ShowMenu(IMenuItem menu)
    {
        while (true)
        {
            Console.Clear();
            Console.WriteLine($"** {menu.Title} **");
            Console.WriteLine(new string('-', menu.Title.Length + 4));

            for (int i = 0; i < menu.SubMenuItems.Count; i++)
            {
                Console.WriteLine($"{i + 1}. {menu.SubMenuItems[i].Title}");
            }

            Console.WriteLine("0. Back");
            Console.Write("Please enter your choice: ");

            if (int.TryParse(Console.ReadLine(), out int choice) && choice >= 0 && choice <= menu.SubMenuItems.Count)
            {
                if (choice == 0)
                {
                    return;
                }
                else
                {
                    IMenuItem selectedItem = menu.SubMenuItems[choice - 1];
                    if (selectedItem.IsSubMenu())
                    {
                        ShowMenu(selectedItem);
                    }
                    else
                    {
                        selectedItem.Execute();
                        Console.WriteLine("Press any key to continue...");
                        Console.Read();
                    }
                }
            }
            else
            {
                Console.WriteLine("Invalid input, please try again.");
            }
        }
    }
}
