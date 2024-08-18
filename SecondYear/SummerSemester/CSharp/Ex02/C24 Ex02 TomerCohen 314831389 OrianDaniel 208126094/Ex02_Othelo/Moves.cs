using System.Collections.Generic;

namespace Ex02_Othelo
{
    public class Moves
    {
        private List<string> m_AllValidMoves;

        private const char k_White = 'O';
        private const char k_Black = 'X';

        // Using foreach - we made the direction matrix as: [][] instead of: [,](even though it's less efficient but still its O(1))
        public static readonly int[][] sr_ValidDirections = new int[][]
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

        public Moves(Board i_Board, Player i_Player)
        {
           m_AllValidMoves = new List<string>();
           allValidMovesOnBoard(i_Board, i_Player);
        }

        public List<string> ValidMoves
        {
            get { return m_AllValidMoves; }
            set { m_AllValidMoves = value; }
        }

        private void allValidMovesOnBoard(Board i_Board, Player i_Player)
        {
            for (int i = 0; i < i_Board.Grid.GetLength(0); i++)
            {
                for (int j = 0; j < i_Board.Grid.GetLength(0); j++)
                {
                    if (isValidMove(i_Board, i, j, i_Player))
                    {
                        int row = (i + 1);
                        char col = ((char)('A' + j));
                        m_AllValidMoves.Add($"{col}{row}");
                    }
                }
            }
        }

        private bool isValidMove(Board i_Board, int i_Row, int i_Col, Player i_Player)
        {
            bool isValidMove = false;
            char[,] grid = i_Board.Grid;

            if (grid[i_Row, i_Col] == k_Black || grid[i_Row, i_Col] == k_White)
            {
                isValidMove = false;
            }
            else
            {
                foreach (int[] direction in sr_ValidDirections)
                {
                    if (IsValidDirection(grid, i_Row, i_Col, direction, i_Player))
                    {
                        isValidMove = true;
                        break;
                    }
                }
            }

            return isValidMove;
        }

        public static bool IsValidDirection(char[,] i_Grid, int i_Row, int i_Col, int[] i_Direction, Player i_Player)
        {
            int rowMovement = i_Direction[0];
            int colMovement = i_Direction[1];
            int rowToScan = i_Row + rowMovement;
            int colToScan = i_Col + colMovement;
            bool isValidDirection = true;
            bool hasOpponentPieceInBetween = false;
            char opponentColor = i_Player.Color == k_White ? k_Black : k_White;

            while (IsInBounds(i_Grid, rowToScan, colToScan) && i_Grid[rowToScan, colToScan] == opponentColor)
            {
                hasOpponentPieceInBetween = true;
                rowToScan += rowMovement;
                colToScan += colMovement;
            }

            if (!hasOpponentPieceInBetween || !IsInBounds(i_Grid, rowToScan, colToScan))
            {
                isValidDirection =  false;
            }

            return isValidDirection && i_Grid[rowToScan, colToScan] == i_Player.Color;
        }

        public static bool IsInBounds(char[,] i_Grid, int i_Row, int i_Col)
        {
            return i_Row >= 0 && i_Row < i_Grid.GetLength(0) && i_Col >= 0 && i_Col < i_Grid.GetLength(1);
        }
    }
}
