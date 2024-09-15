using System;
using System.Windows.Forms;

namespace Ex05_Othelo
{
    public partial class FormGameSettings : Form
    {
        private int m_BoardSize = 6;
        private bool m_IsAgainstComputer = false;

        public FormGameSettings()
        {
            InitializeComponent();
            updateBoardSizeButton();
        }

        private void updateBoardSizeButton()
        {
            this.buttonBoardSize.Text = $"Board Size: {m_BoardSize}X{m_BoardSize}";
        }

        private void buttonBoardSize_Click(object sender, EventArgs e)
        {
            m_BoardSize += 2;
            if (m_BoardSize > 12)
            {
                m_BoardSize = 6;
            }

            updateBoardSizeButton();
        }

        private void buttonPlayComputer_Click(object sender, EventArgs e)
        {
            m_IsAgainstComputer = true;
            startGame();
        }

        private void buttonPlayFriend_Click(object sender, EventArgs e)
        {
            m_IsAgainstComputer = false;
            startGame();
        }

        private void startGame()
        {
            this.Hide();
            FormOtheloBoard formOtheloBoard = new FormOtheloBoard(m_BoardSize, m_IsAgainstComputer);
            formOtheloBoard.ShowDialog();
            this.Close();
        }
    }
}
