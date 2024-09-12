using Ex04.Menus.Interfaces;
using System.Collections.Generic;

namespace Ex04.Menus.Test.InterfacesTest
{
    public class Test
    {
        public static void InterfaceTest()
        {
            IMenuItem mainMenu = new MainMenu("Main Menu", new List<IMenuItem> {
                new MenuItem("Version and Capitals", new List<IMenuItem> {
                    new CountCapitalsMenuItem(),
                    new ShowVersionMenuItem()
                }),
                new MenuItem("Show Current Date/Time", new List<IMenuItem> {
                    new ShowCurrentDateMenuItem(),
                    new ShowCurrentTimeMenuItem()
                })
            });

            mainMenu.Show();
        }
    }
}
