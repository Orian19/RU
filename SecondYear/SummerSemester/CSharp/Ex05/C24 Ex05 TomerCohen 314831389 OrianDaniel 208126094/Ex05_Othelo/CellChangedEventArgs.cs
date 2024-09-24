namespace Ex05_Othelo
{
    public class CellChangedEventArgs
    {
        public int Row { get; }
        public int Col { get; }
        public char Value { get; }

        public CellChangedEventArgs(int i_Row, int i_Col, char i_Value)
        {
            Row = i_Row;
            Col = i_Col;
            Value = i_Value;
        }
    }
}
