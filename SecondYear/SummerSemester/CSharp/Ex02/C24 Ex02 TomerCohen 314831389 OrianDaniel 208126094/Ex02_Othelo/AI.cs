using System;
using System.Collections.Generic;

namespace Ex02_Othelo
{
    public class AIPlayer
    {
        private const int k_MaxDepth = 4; // Depth of 4 balances between performance and strategy

        public string GetBestMove(Board i_Board, Player i_AiPlayer, List<string> i_AllValidMoves)
        {
            int bestScore = int.MinValue;
            string bestMove = null;

            foreach (string move in i_AllValidMoves)
            {
                Board boardCopy = cloneBoard(i_Board);
                Player tempPlayer = new Player("AI", i_AiPlayer.Color, true);
                tempPlayer.MakeMove(move, boardCopy);
                int score = miniMax(boardCopy, k_MaxDepth, false, i_AiPlayer.Color);
                if (score >= bestScore)
                {
                    bestScore = score;
                    bestMove = move;
                }
            }

            return bestMove;
        }

        private int miniMax(Board i_Board, int i_Depth, bool i_IsMaximizing, char i_AIColor)
        {
            (int blackCount, int whiteCount) = i_Board.BlackAndWhitePointCounters();
            int result;

            if (i_Depth == 0 || i_Board.IsBoardFull())
            {
                result = i_AIColor == 'X' ? blackCount - whiteCount : whiteCount - blackCount;
            }
            else
            {
                Player tempPlayer = new Player("Temp", i_IsMaximizing ? i_AIColor : (i_AIColor == 'X' ? 'O' : 'X'), false);
                Moves validMoves = new Moves(i_Board, tempPlayer);

                if (i_IsMaximizing)
                {
                    int maxEvaluation = int.MinValue;
                    foreach (string move in validMoves.ValidMoves)
                    {
                        Board boardCopy = cloneBoard(i_Board);
                        tempPlayer.MakeMove(move, boardCopy);
                        int evaluation = miniMax(boardCopy, i_Depth - 1, false, i_AIColor);
                        maxEvaluation = Math.Max(maxEvaluation, evaluation);
                    }

                    result = maxEvaluation;
                }
                else
                {
                    int minEvaluation = int.MaxValue;

                    foreach (string move in validMoves.ValidMoves)
                    {
                        Board boardCopy = cloneBoard(i_Board);
                        tempPlayer.MakeMove(move, boardCopy);
                        int evaluation = miniMax(boardCopy, i_Depth - 1, true, i_AIColor);
                        minEvaluation = Math.Min(minEvaluation, evaluation);
                    }

                    result = minEvaluation;
                }
            }

            return result;
        }

        private Board cloneBoard(Board i_OriginalBoard)
        {
            int boardSize = i_OriginalBoard.Grid.GetLength(0);
            Board clone = new Board(boardSize);

            for (int row = 0; row < boardSize; row++)
            {
                for (int col = 0; col < boardSize; col++)
                {
                    clone.Grid[row, col] = i_OriginalBoard.Grid[row, col];
                }
            }

            return clone;
        }
    }
}