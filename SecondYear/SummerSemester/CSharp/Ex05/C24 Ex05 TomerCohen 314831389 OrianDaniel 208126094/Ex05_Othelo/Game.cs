using System.Linq;

namespace Ex05_Othelo
{
    public class Game
    {
        private Player m_PlayerOne;
        private Player m_PlayerTwo;
        private Board m_Board;
        private GameDisplay m_GameDisplay;
        private bool m_IsGameQuit;
        private bool m_IsComputer;

        private const string k_QuitGame = "Q";

        public Game()
        {
            initGame();
        }

        private void initGame()
        {
            m_GameDisplay = new GameDisplay();
            m_PlayerOne = new Player(m_GameDisplay.GetPlayerName(1), 'X', false);
            m_IsComputer = m_GameDisplay.IsOpponentComputer();
            m_PlayerTwo = new Player(m_GameDisplay.GetOpponentName(m_IsComputer), 'O', m_IsComputer);
            m_Board = new Board(m_GameDisplay.GetBoardSize());

            m_GameDisplay.DisplayGameSetup(m_PlayerOne.Name, m_PlayerTwo.Name, m_Board.Grid.GetLength(0));
            m_GameDisplay.DisplayBoard(m_Board);
        }
        // $G$ DSN-002 (-15) The logic does not need to even know about the UI, the UI needs to use the logic not vice versa,
        // in case of other UI you will need to change this part and also the UI.
        public void StartGame()
        {
            bool playAgain = true;

            while (playAgain)
            {
                m_IsGameQuit = false;
                playGameRound();

                if (m_IsGameQuit)
                {
                    m_GameDisplay.DisplayQuitMessage();
                    break;
                }

                playAgain = m_GameDisplay.AskToPlayAgain();
                if (playAgain)
                {
                    Ex02.ConsoleUtils.Screen.Clear();
                    m_GameDisplay.DisplayNewGameMessage();
                    m_Board = new Board(m_Board.Grid.GetLength(0));
                    m_GameDisplay.DisplayBoard(m_Board);
                }
            }

            m_GameDisplay.DisplayGoodbyeMessage();
        }

        private void playGameRound()
        {
            int noMovesPlayerCounter = 0;
            bool isPlayerOneTurn = true;
            AIPlayer aiPlayer = new AIPlayer();

            while (noMovesPlayerCounter < 2)
            {
                Player currentPlayer = isPlayerOneTurn ? m_PlayerOne : m_PlayerTwo;
                Moves allValidMoves = new Moves(m_Board, currentPlayer);

                if (allValidMoves.ValidMoves.Any())
                {
                    noMovesPlayerCounter = 0;
                    string move;

                    if (currentPlayer.IsComputer)
                    {
                        move = aiPlayer.GetBestMove(m_Board, currentPlayer, allValidMoves.ValidMoves);
                        m_GameDisplay.DisplayComputerMove(move);
                    }
                    else
                    {
                        move = getPlayerMove(currentPlayer, allValidMoves);
                        Ex02.ConsoleUtils.Screen.Clear();
                        if (m_IsGameQuit)
                        {
                            break;
                        }
                    }

                    currentPlayer.MakeMove(move, m_Board);
                    m_GameDisplay.DisplayBoard(m_Board);
                }
                else
                {
                    if (noMovesPlayerCounter < 1 && !m_Board.IsBoardFull())
                    {
                        m_GameDisplay.DisplaySkipTurnMessage(currentPlayer.Name);
                    }

                    noMovesPlayerCounter++;
                }

                isPlayerOneTurn = !isPlayerOneTurn;
            }

            determineWinner();
        }

        private string getPlayerMove(Player i_CurrentPlayer, Moves i_AllValidMoves)
        {
            string validMoves = string.Join(", ", i_AllValidMoves.ValidMoves);
            string move = m_GameDisplay.GetPlayerMove(i_CurrentPlayer, validMoves);

            while (true)
            {
                if (move == k_QuitGame)
                {
                    m_IsGameQuit = true;
                    m_GameDisplay.DisplayQuitMessage();
                    break;
                }

                bool isValidMove = i_AllValidMoves.ValidMoves.Contains(move);
                bool isValidFormat = i_AllValidMoves.IsValidMoveFormat(move, m_Board);

                if (isValidMove)
                {
                    break;
                }
                else
                {
                    move = m_GameDisplay.GetValidMove(validMoves, move, isValidFormat);
                }
            }

            return move;
        }

        private void determineWinner()
        {
            int numberOfBlackCoins = m_Board.BlackAndWhitePointCounters().Item1;
            int numberOfWhiteCoins = m_Board.BlackAndWhitePointCounters().Item2;

            if (numberOfBlackCoins > numberOfWhiteCoins)
            {
                m_GameDisplay.DisplayWinner(m_PlayerOne.Name, numberOfBlackCoins, m_PlayerTwo.Name, numberOfWhiteCoins);
            }
            else if (numberOfBlackCoins < numberOfWhiteCoins)
            {
                m_GameDisplay.DisplayWinner(m_PlayerTwo.Name, numberOfWhiteCoins, m_PlayerOne.Name, numberOfBlackCoins);
            }
            else
            {
                m_GameDisplay.DisplayTie(numberOfBlackCoins);
            }
        }
    }
}
