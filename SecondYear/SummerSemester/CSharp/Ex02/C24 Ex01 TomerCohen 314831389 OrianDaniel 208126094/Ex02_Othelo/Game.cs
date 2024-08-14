using System;
using System.Collections.Generic;
using System.Diagnostics.Eventing.Reader;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex02_Othelo
{
    public class Game
    {
        private Player m_PlayerOne;
        private Player m_PlayerTwo;
        private Board m_BoardGame;

        public Game()
        {
            initGame();
        }

        private void initGame()
        {
            int gameMode = 0;
            Console.WriteLine("Player 1 please enter your name: ");
            string playerOneName = Console.ReadLine();
            m_PlayerOne = new Player(playerOneName, 'X', false);

            Console.WriteLine("Would you like to play against the computer or another player?");
            Console.WriteLine("1. Against the computer");
            Console.WriteLine("2. Two players");
            while (gameMode != 1 && gameMode != 2)
            {
                Console.Write("Enter 1 for computer or 2 for another player: ");
                int.TryParse(Console.ReadLine(), out gameMode);
            }

            string playerTwoName = "Computer";
            bool isComputer = true;
            if (gameMode == 2)
            {
                Console.Write("Enter the name of Player 2: ");
                playerTwoName = Console.ReadLine();
                isComputer = true;

            }

            m_PlayerTwo = new Player(playerTwoName, 'O', isComputer);

            Console.WriteLine("Choose the size of the board:");
            Console.WriteLine("1. 6x6");
            Console.WriteLine("2. 8x8");
            int boardOptions = 0;

            while (boardOptions != 1 && boardOptions != 2)
            {
                Console.Write("Enter 1 for 6x6 or 2 for 8x8: ");
                int.TryParse(Console.ReadLine(), out boardOptions);
            }

            int boardSize = (boardOptions == 1) ? 6 : 8;

            Console.WriteLine($"\nGame Setup:");
            Console.WriteLine($"Player 1: {playerOneName}");
            Console.WriteLine($"Player 2: {playerTwoName}");
            Console.WriteLine($"Board size: {boardSize}x{boardSize}");

            m_BoardGame = new Board(boardSize);
        }


        public void GameStart()
        {

        }
    }
}
