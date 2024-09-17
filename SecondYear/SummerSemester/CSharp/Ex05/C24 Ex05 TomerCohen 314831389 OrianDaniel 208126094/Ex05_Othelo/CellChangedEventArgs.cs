namespace Ex05_Othelo
{
    public class CellChangedEventArgs
    {
        private int m_Row;
        private int m_Col;
        private char m_Value;   

        public CellChangedEventArgs(int i_Row, int i_Col, char i_Value)
        {
            m_Row = i_Row;
            m_Col = i_Col;
            m_Value = i_Value;
        }

        public int Row 
        { 
            get { return m_Row; } 
        }

        public int Col
        {
            get { return m_Col; }
        }

        public char Value
        {
            get { return m_Value; }
        }
    }
}
