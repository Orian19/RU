using Ex04.Menus.Interfaces;

namespace Ex04.Menus.Test
{
    public class Test
    {
        public static void InterfacesTest()
        {
            Interfaces.MainMenu mainMenu = new Interfaces.MainMenu("Interfaces Main Menu");
            IMenuItem versionAndCapitals = mainMenu.AddMenuItem("Version and Capitals");
            IMenuItem showDateAndTime = mainMenu.AddMenuItem("Show Current Date/Time");

            versionAndCapitals.AddMenuItem("Count Capitals").AddOperation(new CountCapitalsInterfaces());
            versionAndCapitals.AddMenuItem("Show Version").AddOperation(new ShowVersionInterfaces());
            showDateAndTime.AddMenuItem("Show Current Date").AddOperation(new ShowCurrentDateInterfaces());
            showDateAndTime.AddMenuItem("Show Current Time").AddOperation(new ShowCurrentTimeInterfaces());
            mainMenu.Show();
        }

        public static void EventsTest()
        {
            Events.MainMenu mainMenu = new Events.MainMenu("Delegates Main Menu");
            Events.MenuItem versionAndCapitals = mainMenu.AddMenuItem("Version and Capitals");
            Events.MenuItem showDateAndTime = mainMenu.AddMenuItem("Show Current Date/Time");
            Events.MenuItemOperation countCapitalsOperation = new Events.MenuItemOperation("Count Capitals");
            Events.MenuItemOperation showVersionOperation = new Events.MenuItemOperation("Show Version");
            Events.MenuItemOperation showDateOperation = new Events.MenuItemOperation("Show Date");
            Events.MenuItemOperation showTimeOperation = new Events.MenuItemOperation("Show Time");

            countCapitalsOperation.SelectedOperation += new CountCapitalsEvents().Execute;
            versionAndCapitals.AddMenuItem("Count Capitals").AddOperation(countCapitalsOperation);
            showVersionOperation.SelectedOperation += new ShowVersionEvents().Execute;
            versionAndCapitals.AddMenuItem("Show Version").AddOperation(showVersionOperation);
            showDateOperation.SelectedOperation += new ShowCurrentDateEvents().Execute;
            showDateAndTime.AddMenuItem("Show Current Date").AddOperation(showDateOperation);
            showTimeOperation.SelectedOperation += new ShowCurrentTimeEvents().Execute;
            showDateAndTime.AddMenuItem("Show Current Time").AddOperation(showTimeOperation);
            mainMenu.Show();
        }
    }
}
