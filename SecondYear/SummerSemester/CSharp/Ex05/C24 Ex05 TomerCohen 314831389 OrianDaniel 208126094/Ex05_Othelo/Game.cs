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
        public int RoundsPlayed { get; private set; } = 1;

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
            CurrentPlayer = PlayerOne;
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
            InitializeGame(PlayerOne.Name, PlayerTwo.Name, Board.Grid.GetLength(0));
        }

        public bool IsGameOver()
        {
            return m_Board.IsBoardFull() ||
                   (!HasValidMoves(m_PlayerOne) && !HasValidMoves(m_PlayerTwo));
        }

        public string MakeComputerMove()
        {
            Moves validMoves = new Moves(Board, CurrentPlayer);
            return m_AIPlayer.GetBestMove(Board, CurrentPlayer, validMoves.ValidMoves);
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

        public void IncrementRoundsPlayed()
        {
            RoundsPlayed = (RoundsPlayed % 3) + 1;
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