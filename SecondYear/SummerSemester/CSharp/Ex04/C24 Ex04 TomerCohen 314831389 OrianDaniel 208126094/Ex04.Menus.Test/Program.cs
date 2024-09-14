using System;

namespace Ex04.Menus.Test
{
    public class Program
    {
        public static void Main()
        {
            RunTests();
        }

        public static void RunTests()
        {
            Test test = new Test();

            test.InterfacesTest();
            Console.Clear();
            test.EventsTest();
        }
    }
}
