namespace Ex05_Othelo
{
    public class Player
    {
        private readonly string r_Name;
        private readonly char r_Color;
        private bool m_IsComputer;
        public int Wins { get; set; } = 0;

        public Player(string i_Name, char i_Color, bool i_IsComputer)
        {
            r_Name = i_Name;
            r_Color = i_Color;
            m_IsComputer = i_IsComputer;
        }

        public string Name
        {
            get { return r_Name; }
        }

        public char Color
        {
            get { return r_Color; }
        }

        public bool IsComputer
        {
            get { return m_IsComputer; }
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
