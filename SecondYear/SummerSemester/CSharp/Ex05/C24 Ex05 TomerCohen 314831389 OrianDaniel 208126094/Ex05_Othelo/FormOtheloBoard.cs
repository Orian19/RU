using System;
using System.Drawing;
using System.Windows.Forms;
using System.Linq;

namespace Ex05_Othelo
{
    public partial class FormOtheloBoard : Form
    {
        private Game m_Game;
        private Button[,] m_BoardButtons;
        private const int k_ButtonSize = 35;
        private const int k_Margin = 30;

        public FormOtheloBoard(int i_BoardSize, bool i_IsAgainstComputer)
        {
            InitializeComponent();
            initGame(i_BoardSize, i_IsAgainstComputer);
            createBoardButtons();
            updateUI();
            adjustFormSize();
        }

        private void initGame(int i_BoardSize, bool i_IsAgainstComputer)
        {
            string playerOneName = "Black";
            string playerTwoName = i_IsAgainstComputer ? "Computer" : "White";
            m_Game = new Game(playerOneName, playerTwoName, i_IsAgainstComputer, i_BoardSize);
        }

        private void createBoardButtons()
        {
            int boardSize = m_Game.Board.Grid.GetLength(0);
            m_BoardButtons = new Button[boardSize, boardSize];

            for (int row = 0; row < boardSize; row++)
            {
                for (int col = 0; col < boardSize; col++)
                {
                    Button button = new Button
                    {
                        Size = new Size(k_ButtonSize, k_ButtonSize),
                        Tag = $"{(char)('A' + col)}{row + 1}"
                    };
                    button.Click += BoardButton_Click;
                    m_BoardButtons[row, col] = button;
                    Controls.Add(button);
                }
            }
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
                    updateButtonAppearance(m_BoardButtons[row, col], m_Game.Board.Grid[row, col]);
                }
            }
        }

        private void centerBoard()
        {
            int boardSize = m_Game.Board.Grid.GetLength(0);
            int leftMargin = (ClientSize.Width - (boardSize * k_ButtonSize)) / 2;
            int topMargin = k_Margin;

            for (int row = 0; row < boardSize; row++)
            {
                for (int col = 0; col < boardSize; col++)
                {
                    m_BoardButtons[row, col].Location = new Point(leftMargin + col * k_ButtonSize, topMargin + row * k_ButtonSize);
                }
            }
        }

        private void updateButtonAppearance(Button i_Button, char i_CellValue)
        {
            if (i_CellValue == 'X')
            {
                i_Button.BackColor = Color.Black;
                i_Button.ForeColor = Color.White;
                i_Button.Text = "O";
            }
            else if (i_CellValue == 'O')
            {
                i_Button.BackColor = Color.White;
                i_Button.ForeColor = Color.Black;
                i_Button.Text = "O";
            }
            else
            {
                i_Button.BackColor = SystemColors.Control;
                i_Button.ForeColor = SystemColors.ControlText;
                i_Button.Text = "";
            }
        }

        private void highlightValidMoves()
        {
            Moves validMoves = new Moves(m_Game.Board, m_Game.CurrentPlayer);
            foreach (Button button in m_BoardButtons)
            {
                button.Enabled = validMoves.ValidMoves.Contains((string)button.Tag);
                if (button.Enabled && m_Game.Board.Grid[button.Location.Y / k_ButtonSize, button.Location.X / k_ButtonSize] == '\0')
                {
                    button.BackColor = Color.LightGreen;
                }
            }
        }

        private void updateTitleBar()
        {
            this.Text = $"Othello - {m_Game.CurrentPlayer.Name}'s Turn";
        }

        private void adjustFormSize()
        {
            int boardPixelSize = m_Game.Board.Grid.GetLength(0) * k_ButtonSize;
            ClientSize = new Size(boardPixelSize + k_Margin * 2, boardPixelSize + k_Margin * 2);
            centerBoard();
            updateTitleBar();
        }

        private void BoardButton_Click(object sender, EventArgs e)
        {
            Button clickedButton = (Button)sender;
            string move = (string)clickedButton.Tag;

            if (m_Game.TryMakeMove(move))
            {
                updateUI();
                handleNextTurn();
            }
        }

        private void handleNextTurn()
        {
            do
            {
                if (m_Game.HasValidMoves(m_Game.CurrentPlayer))
                {
                    if (m_Game.CurrentPlayer.IsComputer)
                    {
                        makeComputerMove();
                    }
                    break;
                }
                else
                {
                    m_Game.SwitchPlayer();
                    updateUI();

                    if (!m_Game.HasValidMoves(m_Game.CurrentPlayer))
                    {
                        showGameOverMessage();
                        return;
                    }
                }
            } while (true);

            updateUI();
        }

        private bool isGameOver()
        {
            return m_Game.Board.IsBoardFull() ||
                   (!m_Game.HasValidMoves(m_Game.PlayerOne) &&
                    !m_Game.HasValidMoves(m_Game.PlayerTwo));
        }

        private void makeComputerMove()
        {
            AIPlayer aiPlayer = new AIPlayer();
            Moves validMoves = new Moves(m_Game.Board, m_Game.CurrentPlayer);
            string computerMove = aiPlayer.GetBestMove(m_Game.Board, m_Game.CurrentPlayer, validMoves.ValidMoves);

            if (computerMove != null)
            {
                m_Game.TryMakeMove(computerMove);
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
            m_Game = new Game(m_Game.PlayerOne.Name, m_Game.PlayerTwo.Name, m_Game.PlayerTwo.IsComputer, m_Game.Board.Grid.GetLength(0));
            updateUI();
        }
    }
}