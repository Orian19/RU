using System;
using System.Drawing;
using System.Windows.Forms;

namespace Ex05_Othelo
{
    public partial class FormOtheloBoard : Form
    {
        private Game m_Game;
        private PictureBox[,] m_BoardCells;
        private Image m_RedCoinImage;
        private Image m_YellowCoinImage;
        private const int k_MinCellSize = 25;
        private const int k_MaxCellSize = 80;
        private const int k_Margin = 20;
        private int m_CurrentCellSize;

        public FormOtheloBoard(int i_BoardSize, bool i_IsAgainstComputer)
        {
            InitializeComponent();
            loadImagesFromResources();
            initGame(i_BoardSize, i_IsAgainstComputer);
            setupFormSizeAndResize(i_BoardSize);
            createBoardCells();
            updateUI();
        }

        private void loadImagesFromResources()
        {
            try
            {
                m_RedCoinImage = Properties.Resources.CoinRed;
                m_YellowCoinImage = Properties.Resources.CoinYellow;
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error loading coin images: {ex.Message}\nPlease ensure the image files are correctly added to the resources.",
                                "Image Load Error",
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error);

                Environment.Exit(1);
            }
        }

        private void setupFormSizeAndResize(int i_BoardSize)
        {
            this.MinimumSize = new Size(i_BoardSize * k_MinCellSize + 2 * k_Margin,
                                        i_BoardSize * k_MinCellSize + 2 * k_Margin + 40);
            this.MaximumSize = new Size(i_BoardSize * k_MaxCellSize + 2 * k_Margin,
                                        i_BoardSize * k_MaxCellSize + 2 * k_Margin + 40);

            this.Resize += formOtheloBoard_Resize;
        }

        private int calculateCellSize()
        {
            int boardSize = m_Game.Board.Grid.GetLength(0);
            int maxWidth = (ClientSize.Width - 2 * k_Margin) / boardSize;
            int maxHeight = (ClientSize.Height - 2 * k_Margin) / boardSize;
            int cellSize = Math.Min(maxWidth, maxHeight);

            return Math.Max(k_MinCellSize, Math.Min(cellSize, k_MaxCellSize));
        }

        private void formOtheloBoard_Resize(object sender, EventArgs e)
        {
            m_CurrentCellSize = calculateCellSize();
            updateBoardLayout();
            updateUI();
        }

        private void updateBoardLayout()
        {
            int boardSize = m_Game.Board.Grid.GetLength(0);
            int leftMargin = (ClientSize.Width - (boardSize * m_CurrentCellSize)) / 2;
            int topMargin = (ClientSize.Height - (boardSize * m_CurrentCellSize)) / 2;

            for (int row = 0; row < boardSize; row++)
            {
                for (int col = 0; col < boardSize; col++)
                {
                    m_BoardCells[row, col].Location = new Point(leftMargin + col * m_CurrentCellSize,
                                                                topMargin + row * m_CurrentCellSize);
                    m_BoardCells[row, col].Size = new Size(m_CurrentCellSize, m_CurrentCellSize);
                }
            }
        }

        private void initGame(int i_BoardSize, bool i_IsAgainstComputer)
        {
            string playerOneName = "Black";
            string playerTwoName = i_IsAgainstComputer ? "Computer" : "White";

            m_Game = new Game(playerOneName, playerTwoName, i_IsAgainstComputer, i_BoardSize);
            m_Game.Board.CellChanged += board_CellChanged;
        }

        private void board_CellChanged(object sender, CellChangedEventArgs e)
        {
            updateCellAppearance(m_BoardCells[e.Row, e.Col], e.Value);
        }

        private void createBoardCells()
        {
            int boardSize = m_Game.Board.Grid.GetLength(0);

            m_BoardCells = new PictureBox[boardSize, boardSize];
            m_CurrentCellSize = calculateCellSize();

            for (int row = 0; row < boardSize; row++)
            {
                for (int col = 0; col < boardSize; col++)
                {
                    PictureBox pictureBox = new PictureBox
                    {
                        Size = new Size(m_CurrentCellSize, m_CurrentCellSize),
                        Tag = $"{(char)('A' + col)}{row + 1}",
                        SizeMode = PictureBoxSizeMode.StretchImage,
                        BorderStyle = BorderStyle.FixedSingle
                    };
                    pictureBox.Click += boardCell_Click;
                    m_BoardCells[row, col] = pictureBox;
                    Controls.Add(pictureBox);
                    updateCellAppearance(pictureBox, m_Game.Board.Grid[row, col]);
                }
            }

            updateBoardLayout();
        }

        private void updateUI()
        {
            updateBoardDisplay();
            highlightValidMoves();
            updateTitleBar();
        }

        private void updateBoardDisplay()
        {
            for (int row = 0; row < m_Game.Board.Grid.GetLength(0); row++)
            {
                for (int col = 0; col < m_Game.Board.Grid.GetLength(1); col++)
                {
                    updateCellAppearance(m_BoardCells[row, col], m_Game.Board.Grid[row, col]);
                }
            }
        }

        private void updateCellAppearance(PictureBox i_PictureBox, char i_CellValue)
        {
            switch (i_CellValue)
            {
                case 'X':
                    i_PictureBox.Image = m_RedCoinImage;
                    break;
                case 'O':
                    i_PictureBox.Image = m_YellowCoinImage;
                    break;
                default:
                    i_PictureBox.Image = null;
                    break;
            }

            i_PictureBox.BackColor = SystemColors.Control;
            i_PictureBox.SizeMode = PictureBoxSizeMode.Zoom;
        }

        private void highlightValidMoves()
        {
            Moves validMoves = new Moves(m_Game.Board, m_Game.CurrentPlayer);

            foreach (PictureBox pictureBox in m_BoardCells)
            {
                bool isValidMove = validMoves.ValidMoves.Contains((string)pictureBox.Tag);

                pictureBox.Enabled = isValidMove;
                if (isValidMove && pictureBox.Image == null)
                {
                    pictureBox.BackColor = Color.LightGreen;
                }
                else
                {
                    pictureBox.BackColor = SystemColors.Control;
                }
            }
        }

        private void updateTitleBar()
        {
            this.Text = $"Othello - {m_Game.CurrentPlayer.Name}'s Turn";
        }

        private void boardCell_Click(object sender, EventArgs e)
        {
            PictureBox clickedCell = (PictureBox)sender;
            string move = (string)clickedCell.Tag;

            if (m_Game.TryMakeMove(move))
            {
                updateUI();
                handleNextTurn();
            }
        }

        private void handleNextTurn()
        {
            m_Game.HandleNextTurn();
            updateUI();

            if (m_Game.IsGameOver())
            {
                showGameOverMessage();
            }
            else if (m_Game.CurrentPlayer.IsComputer)
            {
                makeComputerMove();
            }
        }

        private void makeComputerMove()
        {
            string move = m_Game.MakeComputerMove();

            if (move != null)
            {
                m_Game.TryMakeMove(move);
                updateUI();
                handleNextTurn();
            }
        }

        private void showGameOverMessage()
        {
            var (winnerName, winnerScore, loserName, loserScore, isTie) = m_Game.DetermineWinner();
            string message;

            if (isTie)
            {
                message = $"It's a tie! Both players have {winnerScore} pieces.";
            }
            else
            {
                message = $"{winnerName} Won!! ({winnerScore}/{loserScore}) ";
                m_Game.UpdateWinCounts(winnerName);
            }

            message += $"({m_Game.PlayerOne.Wins}/{m_Game.PlayerTwo.Wins})";
            message += "\nWould you like another round?";
            DialogResult result = MessageBox.Show(message, "Game Over", MessageBoxButtons.YesNo, MessageBoxIcon.Information);

            if (result == DialogResult.Yes)
            {
                restartGame();
            }
            else
            {
                Close();
            }
        }

        private void restartGame()
        {
            m_Game.Reset();
            updateUI();
        }
    }
}
