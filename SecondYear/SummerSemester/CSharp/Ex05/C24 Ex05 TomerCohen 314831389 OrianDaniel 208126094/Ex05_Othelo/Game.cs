using System.Linq;

namespace Ex05_Othelo
{
    public class Game
    {
        private Player m_PlayerOne;
        private Player m_PlayerTwo;
        private AIPlayer m_AIPlayer;
        private Board m_Board;
        private bool m_IsAgainstComputer;

        public Game(string i_PlayerOneName, string i_PlayerTwoName, bool i_IsAgainstComputer, int i_BoardSize)
        {
            m_IsAgainstComputer = i_IsAgainstComputer;
            m_AIPlayer = new AIPlayer();
            InitializeGame(i_PlayerOneName, i_PlayerTwoName, i_BoardSize);
        }

        private void InitializeGame(string i_PlayerOneName, string i_PlayerTwoName, int i_BoardSize)
        {
            m_PlayerOne = new Player(i_PlayerOneName, 'X', false);
            m_PlayerTwo = new Player(i_PlayerTwoName, 'O', m_IsAgainstComputer);
            m_Board = new Board(i_BoardSize);
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

        public void Reset()
        {
            m_Board = new Board(m_Board.Grid.GetLength(0));
            CurrentPlayer = m_PlayerOne;
        }

        public bool IsGameOver()
        {
            return m_Board.IsBoardFull() ||
                   (!HasValidMoves(m_PlayerOne) && !HasValidMoves(m_PlayerTwo));
        }

        public string MakeComputerMove()
        {
            Moves validMoves = new Moves(m_Board, CurrentPlayer);

            return m_AIPlayer.GetBestMove(m_Board, CurrentPlayer, validMoves.ValidMoves);
        }

        public void HandleNextTurn()
        {
            do
            {
                if (HasValidMoves(CurrentPlayer))
                {
                    break;
                }
                SwitchPlayer();
            } while (!IsGameOver());
        }

        public bool HasValidMoves(Player i_Player)
        {
            Moves moves = new Moves(m_Board, i_Player);

            return moves.ValidMoves.Any();
        }

        public bool TryMakeMove(string i_Move)
        {
            bool madeMove = false;
            Moves validMoves = new Moves(m_Board, CurrentPlayer);
            if (validMoves.ValidMoves.Contains(i_Move))
            {
                CurrentPlayer.MakeMove(i_Move, m_Board);
                SwitchPlayer();
                madeMove = true; ;
            }

            return madeMove;
        }

        public void SwitchPlayer()
        {
            CurrentPlayer = (CurrentPlayer == m_PlayerOne) ? m_PlayerTwo : m_PlayerOne;
        }

        public void UpdateWinCounts(string i_WinnerName)
        {
            (i_WinnerName == m_PlayerOne.Name ? m_PlayerOne : m_PlayerTwo).Wins++;
        }

        public (string winnerName, int winnerScore, string loserName, int loserScore, bool isTie) DetermineWinner()
        {
            int numberOfBlackCoins = m_Board.BlackAndWhitePointCounters().Item1;
            int numberOfWhiteCoins = m_Board.BlackAndWhitePointCounters().Item2;
            string winnerName = null;
            string loserName = null;
            int winnerScore = 0;
            int loserScore = 0;
            bool isTie = false;

            if (numberOfBlackCoins > numberOfWhiteCoins)
            {
                winnerName = m_PlayerOne.Name;
                loserName = m_PlayerTwo.Name;
                winnerScore = numberOfBlackCoins;
                loserScore = numberOfWhiteCoins;
            }
            else if (numberOfBlackCoins < numberOfWhiteCoins)
            {
                winnerName = m_PlayerTwo.Name;
                loserName = m_PlayerOne.Name;
                winnerScore = numberOfWhiteCoins;
                loserScore = numberOfBlackCoins;
            }
            else
            {
                isTie = true;
                winnerScore = loserScore = numberOfBlackCoins;
            }

            return (winnerName, winnerScore, loserName, loserScore, isTie);
        }
    }
}