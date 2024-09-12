using Ex04.Menus.Interfaces;
using Ex04.Menus.Events;
using System.Collections.Generic;

namespace Ex04.Menus.Test
{
    public class Test
    {
        public static void InterfaceTest()
        {
            Interfaces.MainMenu mainMenu = new Interfaces.MainMenu("Interfaces Main Menu");
            IMenuItem versionAndCapitals = mainMenu.AddMenuItem("Version and Capitals");
            IMenuItem showDateAndTime = mainMenu.AddMenuItem("Show Current Date/Time");

            versionAndCapitals.AddMenuItem("Count Capitals").AddOperation(new CountCapitalsMenuItem());
            versionAndCapitals.AddMenuItem("Show Version").AddOperation(new ShowVersionMenuItem());
            showDateAndTime.AddMenuItem("Show Date").AddOperation(new ShowCurrentDateMenuItem());
            showDateAndTime.AddMenuItem("Show Time").AddOperation(new ShowCurrentTimeMenuItem());
            mainMenu.Show();
        }

        public static void EventsTest()
        {
            return;
        }
    }
}
