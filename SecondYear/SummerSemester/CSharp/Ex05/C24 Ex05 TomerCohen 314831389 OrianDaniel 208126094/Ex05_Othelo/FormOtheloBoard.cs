﻿using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

namespace Ex05_Othelo
{
    public partial class FormOtheloBoard : Form
    {
        private Game m_Game;
        private PictureBox[,] m_BoardCells;
        private Image m_RedCoinImage;
        private Image m_YellowCoinImage;
        private const int k_MinCellSize = 20;
        private const int k_MaxCellSize = 80;
        private const int k_Margin = 20;
        private int m_CurrentCellSize;

        public FormOtheloBoard(int i_BoardSize, bool i_IsAgainstComputer)
        {
            InitializeComponent();
            loadImages();
            initGame(i_BoardSize, i_IsAgainstComputer);
            setupFormSizeAndResize(i_BoardSize);
            createBoardCells();
            updateUI();
        }

        private void loadImages()
        {
            string projectDirectory = Directory.GetParent(Environment.CurrentDirectory).Parent.FullName;
            string redCoinPath = Path.Combine(projectDirectory, "CoinRed.png");
            string yellowCoinPath = Path.Combine(projectDirectory, "CoinYellow.png");

            try
            {
                m_RedCoinImage = Image.FromFile(redCoinPath);
                m_YellowCoinImage = Image.FromFile(yellowCoinPath);
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Error loading coin images: {ex.Message}\nPlease ensure the image files are in the correct location.",
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

            return limitCellSizeToRangelamp(cellSize, k_MinCellSize, k_MaxCellSize);
        }

        private int limitCellSizeToRangelamp(int i_Value, int i_Min, int i_Max)
        {
            return Math.Max(i_Min, Math.Min(i_Value, i_Max));
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
            if (i_CellValue == 'X')
            {
                i_PictureBox.Image = m_RedCoinImage;
            }
            else if (i_CellValue == 'O')
            {
                i_PictureBox.Image = m_YellowCoinImage;
            }
            else
            {
                i_PictureBox.Image = null;
            }
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
                message = $"It's a tie! Both players have {winnerScore} pieces. ({m_Game.RoundsPlayed}/3)";
            }
            else
            {
                message = $"{winnerName} Won!! ({winnerScore}/{loserScore}) ({m_Game.RoundsPlayed}/3)";
            }

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
            m_Game.IncrementRoundsPlayed();
            m_Game.Reset();
            updateUI();
        }
    }
}