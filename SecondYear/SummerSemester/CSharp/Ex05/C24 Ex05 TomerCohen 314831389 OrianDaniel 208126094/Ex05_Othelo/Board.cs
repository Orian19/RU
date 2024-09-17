using System;

namespace Ex05_Othelo
{
    public class Board
    {
        private char[,] m_Grid;
        private const char k_White = 'O';
        private const char k_Black = 'X';
        public event EventHandler<CellChangedEventArgs> CellChanged;

        public Board(int i_Size)
        {
            m_Grid = new char[i_Size, i_Size];
            initBoard();
        }

        public char[,] Grid
        {
            get { return m_Grid; }
            set { m_Grid = value; }
        }
        
        private void initBoard()
        {
            int midRow = m_Grid.GetLength(0) / 2;
            int midCol = m_Grid.GetLength(1) / 2;

            SetCell(midRow - 1, midCol - 1, k_White);
            SetCell(midRow - 1, midCol, k_Black);
            SetCell(midRow, midCol - 1, k_Black);
            SetCell(midRow, midCol, k_White);
        }

        public void UpdateBoard(int i_Row, int i_Col, Player i_Player)
        {
            foreach (int[] direction in Moves.sr_ValidDirections)
            {
                if (Moves.IsValidDirection(m_Grid, i_Row, i_Col, direction, i_Player))
                {
                    SetCell(i_Row, i_Col, i_Player.Color);
                    int rowToUpdate = i_Row + direction[0];
                    int colToUpdate = i_Col + direction[1];

                    while (m_Grid[rowToUpdate, colToUpdate] != i_Player.Color)
                    {
                        SetCell(rowToUpdate, colToUpdate, i_Player.Color);
                        rowToUpdate += direction[0];
                        colToUpdate += direction[1];
                    }
                }
            }
        }

        private void SetCell(int row, int col, char value)
        {
            if (m_Grid[row, col] != value)
            {
                m_Grid[row, col] = value;
                OnCellChanged(row, col, value);
            }
        }

        protected virtual void OnCellChanged(int row, int col, char value)
        {
            CellChanged?.Invoke(this, new CellChangedEventArgs(row, col, value));
        }

        public Tuple<int, int> BlackAndWhitePointCounters()
        {
            int blackCount = 0;
            int whiteCount = 0;

            for (int row = 0; row < m_Grid.GetLength(0); row++)
            {
                for (int col = 0; col < m_Grid.GetLength(1); col++)
                {
                    if (m_Grid[row, col] == k_Black)
                    {
                        blackCount++;
                    }
                    else if (m_Grid[row, col] == k_White)
                    {
                        whiteCount++;
                    }
                }
            }

            return new Tuple<int, int>(blackCount, whiteCount);
        }

        public bool IsBoardFull()
        {
            bool isFull = true;

            foreach (char cell in m_Grid)
            {
                if (cell == '\0')
                {
                    isFull = false;
                    break;
                }
            }

            return isFull; 
        }
    }
}
