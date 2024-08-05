using System;
using Ex01_02;

namespace Ex01_03
{
    public class Program
    {
        public static void Main()
        {
            Console.WriteLine(string.Format("The heigth is: {0}", 3));
            Ex01_02.Program.PrintABCTreeIterative(3);

            Console.WriteLine(string.Format("The heigth is: {0}", 5));
            Ex01_02.Program.PrintABCTreeIterative(5);

            Console.WriteLine(string.Format("The heigth is: {0}", 6));
            Ex01_02.Program.PrintABCTreeIterative(6);

            Console.WriteLine(string.Format("The heigth is: {0}", 9));
            Ex01_02.Program.PrintABCTreeIterative(9);
            Console.ReadLine();
        }
    }
}
