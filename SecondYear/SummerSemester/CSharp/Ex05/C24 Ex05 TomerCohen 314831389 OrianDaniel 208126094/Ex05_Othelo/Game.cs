using System.Linq;

namespace Ex05_Othelo
{
    public class Game
    {
        private Player m_PlayerOne;
        private Player m_PlayerTwo;
        private Board m_Board;
        private bool m_IsGameQuit;
        private bool m_IsComputer;

        private const string k_QuitGame = "Q";

        public Game(string i_PlayerOneName, string i_PlayerTwoName, bool i_IsComputer, int i_BoardSize)
        {
            m_PlayerOne = new Player(i_PlayerOneName, 'X', false);
            m_PlayerTwo = new Player(i_PlayerTwoName, 'O', i_IsComputer);
            m_Board = new Board(i_BoardSize);
            m_IsComputer = i_IsComputer;
        }

        public Board Board => m_Board;

        public Player CurrentPlayer { get; private set; }

        public Player PlayerOne
        {  
            get { return m_PlayerOne; } 
        }

        public Player PlayerTwo
        {
            get { return m_PlayerTwo; }
        }

        public bool IsGameQuit => m_IsGameQuit;

        public void StartGameRound(AIPlayer i_AIPlayer)
        {
            int noMovesPlayerCounter = 0;
            bool isPlayerOneTurn = true;

            while (noMovesPlayerCounter < 2)
            {
                CurrentPlayer = isPlayerOneTurn ? m_PlayerOne : m_PlayerTwo;
                Moves allValidMoves = new Moves(m_Board, CurrentPlayer);

                if (allValidMoves.ValidMoves.Any())
                {
                    noMovesPlayerCounter = 0;
                }
                else
                {
                    noMovesPlayerCounter++;
                }

                isPlayerOneTurn = !isPlayerOneTurn;
            }
        }

        public string GetValidMoveForPlayer(string move, Moves allValidMoves)
        {
            while (true)
            {
                if (move == k_QuitGame)
                {
                    m_IsGameQuit = true;
                    break;
                }

                bool isValidMove = allValidMoves.ValidMoves.Contains(move);
                bool isValidFormat = allValidMoves.IsValidMoveFormat(move, m_Board);

                if (isValidMove)
                {
                    break;
                }
                else
                {
                    move = "";
                }
            }

            return move;
        }

        public (string winnerName, int winnerScore, string loserName, int loserScore, bool isTie) DetermineWinner()
        {
            int numberOfBlackCoins = m_Board.BlackAndWhitePointCounters().Item1;
            int numberOfWhiteCoins = m_Board.BlackAndWhitePointCounters().Item2;

            if (numberOfBlackCoins > numberOfWhiteCoins)
            {
                return (m_PlayerOne.Name, numberOfBlackCoins, m_PlayerTwo.Name, numberOfWhiteCoins, false);
            }
            else if (numberOfBlackCoins < numberOfWhiteCoins)
            {
                return (m_PlayerTwo.Name, numberOfWhiteCoins, m_PlayerOne.Name, numberOfBlackCoins, false);
            }
            else
            {
                return (null, numberOfBlackCoins, null, numberOfWhiteCoins, true);
            }
        }
    }
}
