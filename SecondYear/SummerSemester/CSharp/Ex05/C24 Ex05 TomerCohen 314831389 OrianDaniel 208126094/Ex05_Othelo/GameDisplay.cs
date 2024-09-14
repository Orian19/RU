using System;
using System.Text;

namespace Ex05_Othelo
{
    public class GameDisplay
    {
        public void DisplayBoard(Board i_Board)
        {
            StringBuilder displayBoard = new StringBuilder();
            int rows = i_Board.Grid.GetLength(0);
            int cols = i_Board.Grid.GetLength(1);

            displayBoard.Append(" ");
            for (int col = 0; col < cols; col++)
            {
                displayBoard.Append($"   {(char)('A' + col)}");
            }

            displayBoard.AppendLine();
            borderEqualSymbols(displayBoard, cols);
            for (int row = 0; row < rows; row++)
            {
                displayBoard.Append($"{row + 1} |");
                for (int col = 0; col < cols; col++)
                {
                    char cellValue = i_Board.Grid[row, col] == '\0' ? ' ' : i_Board.Grid[row, col];
                    displayBoard.Append($" {cellValue} |");
                }

                displayBoard.AppendLine();
                borderEqualSymbols(displayBoard, cols);
            }

            Console.WriteLine(displayBoard.ToString());
        }

        private void borderEqualSymbols(StringBuilder i_DisplayBoard, int i_Cols)
        {
            i_DisplayBoard.Append("  ");
            for (int col = 0; col < i_Cols; col++)
            {
                i_DisplayBoard.Append("====");
            }

            i_DisplayBoard.AppendLine("=");
        }

        public string GetPlayerName(int i_PlayerNumber)
        {
            Console.Write($"Player {i_PlayerNumber}, please enter your name: ");

            return Console.ReadLine();
        }

        public bool IsOpponentComputer()
        {
            Console.WriteLine("Would you like to play against the computer or another player?");
            Console.WriteLine("1. Against the computer");
            Console.WriteLine("2. Two players");
            string choice = "";

            do
            {
                Console.Write("Enter 1 for computer or 2 for another player: ");
                choice = Console.ReadLine();
            } while (choice != "1" && choice != "2");

            return choice == "1";
        }

        public string GetOpponentName(bool i_IsComputer)
        {
            return i_IsComputer ? "Computer" : GetPlayerName(2);
        }

        public int GetBoardSize()
        {
            Console.WriteLine("Choose the size of the board:");
            Console.WriteLine("1. 6x6");
            Console.WriteLine("2. 8x8");
            string choice = "";

            do
            {
                Console.Write("Enter 1 for 6x6 or 2 for 8x8: ");
                choice = Console.ReadLine();
            } while (choice != "1" && choice != "2");

            return choice == "1" ? 6 : 8;
        }

        public void DisplayGameSetup(string i_PlayerOneName, string i_PlayerTwoName, int i_BoardSize)
        {
            Console.WriteLine();
            Console.WriteLine($"Game Setup:");
            Console.WriteLine($"Player 1: {i_PlayerOneName}");
            Console.WriteLine($"Player 2: {i_PlayerTwoName}");
            Console.WriteLine($"Board size: {i_BoardSize}x{i_BoardSize}");
            Console.WriteLine();
        }

        public void DisplayComputerMove(string i_Move)
        {
            Console.WriteLine($"Computer chose: {i_Move}");
        }

        public void DisplayQuitMessage()
        {
            Console.WriteLine("Game has been quit.");
        }

        public bool AskToPlayAgain()
        {
            Console.WriteLine("Would you like to play another round? (Y/N)");
            string response = "";

            do
            {
                response = Console.ReadLine();
            } while (response != "Y" && response != "N" && response != "Q");

            return response == "Y";
        }

        public void DisplayNewGameMessage()
        {
            Console.WriteLine("Starting a new game...");
        }

        public void DisplayGoodbyeMessage()
        {
            Console.WriteLine("Thank you for playing! Goodbye.");
            Console.WriteLine("Press enter to exit...");
        }

        public void DisplaySkipTurnMessage(string i_PlayerName)
        {
            Console.WriteLine($"{i_PlayerName} has no valid moves. Skipping turn.");
        }

        public string GetPlayerMove(Player i_Player, string i_ValidMoves)
        {
            Console.WriteLine($"{i_Player.Name}, what is your next move from the following valid moves: {i_ValidMoves}");

            return Console.ReadLine();
        }

        public string GetValidMove(string i_ValidMoves, string i_Move, bool i_IsValidFormat)
        {
            string feedback = !i_IsValidFormat
                        ? $"The format of the move '{i_Move}' is incorrect. Please choose one of the following valid moves: {i_ValidMoves}"
                        : $"The move '{i_Move}' is not valid. Please choose one of the following valid moves: {i_ValidMoves}";

            Console.WriteLine(feedback);

            return Console.ReadLine();
        }

        public void DisplayWinner(string i_WinnerName, int i_WinnerScore, string i_LoserName, int i_LoserScore)
        {
            Console.WriteLine($"Congratulations, {i_WinnerName}! You are the winner with {i_WinnerScore} points.");
            Console.WriteLine($"Better luck next time {i_LoserName}. You had {i_LoserScore} points.");
        }

        public void DisplayTie(int i_Score)
        {
            Console.WriteLine($"It's a tie! Both players ended with {i_Score} pieces each.");
        }
    }
}
