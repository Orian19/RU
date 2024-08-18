using Ex02_Othelo;
using System;
using System.Linq;

public class Game
{
    private Player m_PlayerOne;
    private Player m_PlayerTwo;
    private Board m_BoardGame;
    private bool m_IsComputer;

    public Game()
    {
        initGame();
    }

    private void initGame()
    {
        Console.Write("Player 1 please enter your name: ");
        string playerOneName = Console.ReadLine();
        m_PlayerOne = new Player(playerOneName, 'X', false);
        string playerTwoName = chooseGameMode();
        m_PlayerTwo = new Player(playerTwoName, 'O', m_IsComputer);

        int boardSize = chooseBoardSize();

        Console.WriteLine();
        Console.WriteLine($"Game Setup:");
        Console.WriteLine($"Player 1: {playerOneName}");
        Console.WriteLine($"Player 2: {playerTwoName}");
        Console.WriteLine($"Board size: {boardSize}x{boardSize}");

        m_BoardGame = new Board(boardSize);
        m_BoardGame.DisplayBoard();
    }

    public void GamePlay()
    {
        Console.WriteLine("At any point you can type 'Q' to quit the game.");

        int noMovesPlayerCounter = 0;
        bool isPlayerOneTurn = true;
        AIPlayer aiPlayer = new AIPlayer();

        while (noMovesPlayerCounter < 2)
        {
            Player currentPlayer = isPlayerOneTurn ? m_PlayerOne : m_PlayerTwo;
            Moves allValidMoves = new Moves(m_BoardGame, currentPlayer);

            if (allValidMoves.ValidMoves.Any())
            {
                noMovesPlayerCounter = 0;
                string move = "";

                if (currentPlayer.IsComputer)
                {
                    move = aiPlayer.GetBestMove(m_BoardGame, currentPlayer, allValidMoves.ValidMoves);
                    Console.WriteLine($"Computer chose: {move}");
                }
                else
                {
                    move = getPlayerMove(currentPlayer, allValidMoves);
                    Ex02.ConsoleUtils.Screen.Clear();
                    if (handleQuit(move))
                    {
                        Console.WriteLine("Game has been quit. Thank you for playing!");
                        break;
                    }
                }

                currentPlayer.MakeMove(move, m_BoardGame);
                m_BoardGame.DisplayBoard();
            }
            else
            {
                if (noMovesPlayerCounter < 1 && !m_BoardGame.IsBoardFull())
                {
                    Console.WriteLine($"{currentPlayer.Name} has no valid moves. Skipping turn.");
                }

                noMovesPlayerCounter++;
            }

            isPlayerOneTurn = !isPlayerOneTurn;
        }

        determineWinner();
    }

    private string chooseGameMode()
    {
        Console.WriteLine();
        Console.WriteLine("Would you like to play against the computer or another player?");
        Console.WriteLine("1. Against the computer");
        Console.WriteLine("2. Two players");
        Console.WriteLine();
        string gameMode = "";
        string playerTwoName;

        while (!gameMode.Equals("1") && !gameMode.Equals("2"))
        {
            Console.Write("Enter 1 for computer or 2 for another player: ");
            gameMode = Console.ReadLine();
        }

        if (gameMode.Equals("1"))
        {
            m_IsComputer = true;
            playerTwoName = "Computer";
        }
        else
        {
            Console.Write("Enter the name of Player 2: ");
            m_IsComputer = false;
            playerTwoName = Console.ReadLine();
        }

        return playerTwoName;
    }

    private int chooseBoardSize()
    {
        Console.WriteLine();
        Console.WriteLine("Choose the size of the board:");
        Console.WriteLine("1. 6x6");
        Console.WriteLine("2. 8x8");
        Console.WriteLine();
        string boardOptions = "";

        while (!boardOptions.Equals("1") && !boardOptions.Equals("2"))
        {
            Console.Write("Enter 1 for 6x6 or 2 for 8x8: ");
            boardOptions = Console.ReadLine();
        }

        return boardOptions.Equals("1") ? 6 : 8;
    }

    private string getPlayerMove(Player i_CurrentPlayer, Moves i_AllValidMoves)
    {
        Console.WriteLine($"{i_CurrentPlayer.Name}, what is your next move from the following valid moves: {string.Join(", ", i_AllValidMoves.ValidMoves)}");
        string move = Console.ReadLine();

        while (!i_AllValidMoves.ValidMoves.Contains(move))
        {
            if (handleQuit(move))
            {
                break;
            }

            Console.WriteLine($"It wasn't a valid move, please choose one of the following valid moves: {string.Join(", ", i_AllValidMoves.ValidMoves)}");
            move = Console.ReadLine();
        }

        return move;
    }

    private bool handleQuit(string i_Move)
    {
        bool wantToQuit = false;

        if (i_Move == "Q")
        {
            wantToQuit = true;
        }

        return wantToQuit;
    }

    private void determineWinner()
    {
        int numberOfBlackCoins = m_BoardGame.BlackAndWhitePointCounters().Item1;
        int numberOfWhiteCoins = m_BoardGame.BlackAndWhitePointCounters().Item2;

        if (numberOfBlackCoins > numberOfWhiteCoins)
        {
            Console.WriteLine($"Congratulations, {m_PlayerOne.Name}! You are the winner with {numberOfBlackCoins} points.");
            Console.WriteLine($"Better luck next time {m_PlayerTwo.Name}. You had {numberOfWhiteCoins} points.");
        }
        else if (numberOfBlackCoins < numberOfWhiteCoins)
        {
            Console.WriteLine($"Congratulations, {m_PlayerTwo.Name}! You are the winner with {numberOfWhiteCoins} points.");
            Console.WriteLine($"Better luck next time {m_PlayerOne.Name}. You had {numberOfBlackCoins} points.");
        }
        else
        {
            Console.WriteLine($"It's a tie! Both players ended with {numberOfBlackCoins} pieces each.");
        }
    }
}
