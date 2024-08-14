using System;
using System.Collections.Generic;
using System.Diagnostics.Eventing.Reader;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security.Policy;
using System.Text;
using System.Threading.Tasks;

namespace Ex02_Othelo
{
    public class Board
    {
        private char[,] m_Grid;
        private int m_WhiteCounter;
        private int m_BlackCounter;

        private const char k_White = 'O';
        private const char k_Black = 'X';

        public Board(int i_Size)
        {
            m_Grid = new char[i_Size, i_Size];
            m_WhiteCounter = 2;
            m_BlackCounter = 2;
            initBoard();
            DisplayBoard();
        }

        public char[,] Grid 
        { 
            get { return m_Grid;  } 
            set { m_Grid = value; } 
        }

        public int CountWhite 
        { 
            get { return m_WhiteCounter;  }
            set {  m_WhiteCounter = value; }
        }

        public int CountBlack
        {
            get { return m_BlackCounter; }
            set { m_BlackCounter = value; }
        }

        private void initBoard()
        {
            int midRow = m_Grid.GetLength(0) / 2;
            int midCol = m_Grid.GetLength(1) / 2;   

            m_Grid[midRow - 1, midCol - 1] = k_White;
            m_Grid[midRow - 1, midCol] = k_Black;
            m_Grid[midRow, midCol - 1] = k_Black;
            m_Grid[midRow, midCol] = k_White;
        }

        public void DisplayBoard()
        {
            StringBuilder displayBoard = new StringBuilder();
            int rows = m_Grid.GetLength(0);
            int cols = m_Grid.GetLength(1);

            displayBoard.Append(" ");
            for (int col = 0; col < cols; col++)
            {
                displayBoard.Append($"   {(char)('A' + col)}");
            }

            displayBoard.AppendLine();
            borderEqualSymbols(displayBoard, cols);

            for (int row = 0; row < rows; row++)
            {
                displayBoard.Append($"{row + 1} |");
                for (int col = 0; col < cols; col++)
                {
                    char cellValue = m_Grid[row, col] == '\0' ? ' ' : m_Grid[row, col];
                    displayBoard.Append($" {cellValue} |");
                }

                displayBoard.AppendLine();
                borderEqualSymbols(displayBoard, cols);
            }

            Console.WriteLine(displayBoard.ToString());
        }

        private void borderEqualSymbols(StringBuilder i_DisplayBoard, int i_Cols)
        {
            i_DisplayBoard.Append("  ");

            for (int col = 0; col < i_Cols; col++)
            {
                i_DisplayBoard.Append("====");
            }

            i_DisplayBoard.AppendLine("=");
        }

        public void UpdateBoard(Move i_Move, int i_Row, int i_Col, Player i_Player)
        {
            foreach (int[] direction in i_Move.r_ValidDirections)
            {
                if (i_Move.isValidDirection(m_Grid, i_Row, i_Col, direction, i_Player))
                {
                    m_Grid[i_Row, i_Col] = i_Player.Color;
                    if (i_Player.Color == k_White)
                    {
                        m_WhiteCounter++;

                    }
                    else
                    {
                        m_BlackCounter++;
                    }
                    int rowToUpdate = i_Row + direction[0];
                    int colToUpdate = i_Col + direction[1];

                    while (m_Grid[rowToUpdate, colToUpdate] != i_Player.Color)
                    {
                        m_Grid[rowToUpdate, colToUpdate] = i_Player.Color;
                        if (i_Player.Color == k_White)
                        {
                            m_WhiteCounter++;
                            m_BlackCounter--;
                        }
                        else
                        {
                            m_BlackCounter++;
                            m_WhiteCounter--;
                        }
                        rowToUpdate += direction[0];
                        colToUpdate += direction[1];
                    }
                }
            }
        }
    }
}
