namespace Ex02_Othelo
{
    public class Player
    {
        private string m_Name;
        private char m_Color;
        private bool m_IsComputer;

        public Player(string i_Name, char i_Color, bool i_IsComputer)
        {
            m_Name = i_Name;
            m_Color = i_Color;
            m_IsComputer = i_IsComputer;
        }

        public string Name
        {
            get { return m_Name; }
            set { m_Name = value; }
        }

        public char Color
        {
            get { return m_Color; }
            set { m_Color = value; }
        }

        public bool IsComputer
        {
            get { return m_IsComputer; }
            set { m_IsComputer = value; }
        }

        // The input is always validated before calling MakeMove, so there's no need to use TryParse.
        // Using direct character arithmetic is more efficient and straightforward for this case.
        public void MakeMove(string i_Move, Board i_Board)
        {
            int col = i_Move[0] - 'A';
            int row = i_Move[1] - '0' - 1;
            i_Board.UpdateBoard(row, col, this);
        }
    }
}
