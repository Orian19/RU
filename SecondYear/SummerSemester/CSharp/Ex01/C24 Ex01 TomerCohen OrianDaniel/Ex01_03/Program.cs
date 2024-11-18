using System;

namespace Ex01_03
{
    public class Program
    {
        public static void Main()
        {
            Console.WriteLine(string.Format("The height is: {0}", 3));
            Ex01_02.Program.PrintABCTreeRecursive(3);
            Console.WriteLine(string.Format("The height is: {0}", 5));
            Ex01_02.Program.PrintABCTreeRecursive(5);
            Console.WriteLine(string.Format("The height is: {0}", 6));
            Ex01_02.Program.PrintABCTreeRecursive(6);
            Console.WriteLine(string.Format("The height is: {0}", 9));
            Ex01_02.Program.PrintABCTreeRecursive(9);

            Console.WriteLine("Press enter to exit...");
            Console.ReadLine();
        }
    }
}