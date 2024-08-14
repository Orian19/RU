using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex02_Othelo
{
    public class Move
    {
        private string m_GameMove;

        private const char k_White = 'O';
        private const char k_Black = 'X';

        public readonly int[][] r_ValidDirections = new int[][]
        {
            new int[] { -1, -1 },
            new int[] { -1,  0 },
            new int[] { -1,  1 },
            new int[] {  0, -1 },
            new int[] {  0,  1 },
            new int[] {  1, -1 },
            new int[] {  1,  0 },
            new int[] {  1,  1 }
        };

        public Move() 
        {
            
        }

        public string GameMove
        { 
            get { return m_GameMove; } 
            set {  m_GameMove = value; }
        }

        public bool IsValidMove(Board i_Board, int i_Row, int i_Col, Player i_Player)
        {
            bool isValidMove = false;
            char[,] grid = i_Board.Grid;

            if (grid[i_Row, i_Col] == k_Black || grid[i_Row, i_Col] == k_White)
            {
                isValidMove = false;
            }
            else
            {
                foreach (int[] direction in r_ValidDirections)
                {
                    if (isValidDirection(grid, i_Row, i_Col, direction, i_Player))
                    {
                        isValidMove = true;
                        break;
                    }
                }
            }

            return isValidMove;
        }

        public bool isValidDirection(char[,] i_Grid, int i_Row, int i_Col, int[] i_Direction, Player i_Player)
        {
            bool isValidDirection = true;
            int rowMovement = i_Direction[0];
            int colMovement = i_Direction[1];

            int rowToScan = i_Row + rowMovement;
            int colToScan = i_Col + colMovement;

            while (IsInBounds(i_Grid, rowToScan, colToScan) && i_Grid[rowToScan, colToScan] != i_Player.Color)
            {
                rowToScan += rowMovement;
                colToScan += colMovement;
            }

            if (!IsInBounds(i_Grid, rowToScan, colToScan) || i_Grid[rowToScan, colToScan] != i_Player.Color)
            {
                isValidDirection = false;
            }

            return isValidDirection;
        }

        private bool IsInBounds(char[,] i_Grid, int i_Row, int i_Col)
        {
            return i_Row >= 0 && i_Row < i_Grid.GetLength(0) && i_Col >= 0 && i_Col < i_Grid.GetLength(1);
        }
    }
}
