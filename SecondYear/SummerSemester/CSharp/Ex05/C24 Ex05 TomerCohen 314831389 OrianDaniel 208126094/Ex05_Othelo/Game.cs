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
        public int RoundsPlayed { get; private set; } = 1;

        private const string k_QuitGame = "Q";

        public Game(string i_PlayerOneName, string i_PlayerTwoName, bool i_IsComputer, int i_BoardSize)
        {
            m_PlayerOne = new Player(i_PlayerOneName, 'X', false);
            m_PlayerTwo = new Player(i_PlayerTwoName, 'O', i_IsComputer);
            m_Board = new Board(i_BoardSize);
            m_IsComputer = i_IsComputer;
            CurrentPlayer = m_PlayerOne;
        }

        public Board Board
        {
            get { return m_Board; }
        }

        public Player CurrentPlayer { get; private set; }

        public Player PlayerOne
        {  
            get { return m_PlayerOne; } 
        }

        public Player PlayerTwo
        {
            get { return m_PlayerTwo; }
        }

        public bool IsGameQuit
        {
            get { return m_IsGameQuit; }
        }

        public void IncrementRoundsPlayed()
        {
            RoundsPlayed++;
            if (RoundsPlayed > 3)
            {
                RoundsPlayed = 1;
            }
        }

        public bool HasValidMoves(Player i_Player)
        {
            Moves moves = new Moves(m_Board, i_Player);
            return moves.ValidMoves.Any();
        }

        public bool TryMakeMove(string i_Move)
        {
            Moves validMoves = new Moves(Board, CurrentPlayer);
            if (validMoves.ValidMoves.Contains(i_Move))
            {
                CurrentPlayer.MakeMove(i_Move, Board);
                SwitchPlayer();
                return true;
            }
            return false;
        }

        public void SwitchPlayer()
        {
            CurrentPlayer = (CurrentPlayer == PlayerOne) ? PlayerTwo : PlayerOne;
        }

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

        public string GetValidMoveForPlayer(string i_Move, Moves i_AllValidMoves)
        {
            while (true)
            {
                if (i_Move == k_QuitGame)
                {
                    m_IsGameQuit = true;
                    break;
                }

                bool isValidMove = i_AllValidMoves.ValidMoves.Contains(i_Move);
                bool isValidFormat = i_AllValidMoves.IsValidMoveFormat(i_Move, m_Board);

                if (isValidMove)
                {
                    break;
                }
                else
                {
                    i_Move = "";
                }
            }

            return i_Move;
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
