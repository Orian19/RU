using System.Linq;

namespace Ex05_Othelo
{
    public static class GameManager
    {
        public static void Run()
        {
            GameDisplay display = new GameDisplay();
            string playerOneName = display.GetPlayerName(1);
            bool isComputer = display.IsOpponentComputer();
            string playerTwoName = display.GetOpponentName(isComputer);
            int boardSize = display.GetBoardSize();

            RunGameLoop(display, playerOneName, playerTwoName, isComputer, boardSize);
        }

        private static void RunGameLoop(GameDisplay display, string playerOneName, string playerTwoName, bool isComputer, int boardSize)
        {
            bool playAgain = true;
            AIPlayer aiPlayer = new AIPlayer();

            while (playAgain)
            {
                Game game = new Game(playerOneName, playerTwoName, isComputer, boardSize);
                display.DisplayGameSetup(playerOneName, playerTwoName, boardSize);
                display.DisplayBoard(game.Board);

                PlayGameRound(display, game, aiPlayer);

                if (game.IsGameQuit)
                {
                    break;
                }

                playAgain = display.AskToPlayAgain();
                if (playAgain)
                {
                    display.DisplayNewGameMessage();
                }
                else
                {
                    display.DisplayGoodbyeMessage();
                }
            }
        }

        private static void PlayGameRound(GameDisplay display, Game game, AIPlayer aiPlayer)
        {
            bool isPlayerOneTurn = true;
            bool bothPlayersSkipped = false;

            while (!game.IsGameQuit && !bothPlayersSkipped)
            {
                Player currentPlayer = isPlayerOneTurn ? game.PlayerOne : game.PlayerTwo;
                Moves validMoves = new Moves(game.Board, currentPlayer);

                if (validMoves.ValidMoves.Any())
                {
                    bothPlayersSkipped = false;
                    string move;

                    if (currentPlayer.IsComputer)
                    {
                        move = aiPlayer.GetBestMove(game.Board, currentPlayer, validMoves.ValidMoves);
                        display.DisplayComputerMove(move);
                    }
                    else
                    {
                        string validMovesString = string.Join(", ", validMoves.ValidMoves);
                        move = display.GetPlayerMove(currentPlayer, validMovesString);

                        move = game.GetValidMoveForPlayer(move, validMoves);
                        if (game.IsGameQuit)
                        {
                            break;
                        }
                    }

                    currentPlayer.MakeMove(move, game.Board);
                    display.DisplayBoard(game.Board);
                }
                else
                {
                    display.DisplaySkipTurnMessage(currentPlayer.Name);
                 
                    if (!new Moves(game.Board, game.PlayerOne).ValidMoves.Any() &&
                        !new Moves(game.Board, game.PlayerTwo).ValidMoves.Any())
                    {
                        bothPlayersSkipped = true;
                    }
                }

                isPlayerOneTurn = !isPlayerOneTurn;
            }

            if (!game.IsGameQuit)
            {
                var (winnerName, winnerScore, loserName, loserScore, isTie) = game.DetermineWinner();
                if (isTie)
                {
                    display.DisplayTie(winnerScore);
                }
                else
                {
                    display.DisplayWinner(winnerName, winnerScore, loserName, loserScore);
                }
            }
        }
    }
}
