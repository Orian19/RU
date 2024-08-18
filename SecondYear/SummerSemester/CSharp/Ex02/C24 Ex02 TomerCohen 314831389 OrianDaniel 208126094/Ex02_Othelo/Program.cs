using System;

namespace Ex02_Othelo
{
    public class Program
    {
        static void Main(string[] args)
        {
            Game game = new Game();
            game.GamePlay();

            Console.WriteLine("Press enter to exit...");
            Console.ReadLine();
        }
    }
}
