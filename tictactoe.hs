import Data.Char (ord)


data GameResult = XWin | OWin | Draw | Unfinished deriving Show
data Turn = Player1Turn | Player2Turn deriving Show
data Board = Grid3x3 String

instance Show Board where
  show (Grid3x3 board) =
    "---\n" ++
    (take 3 board) ++ "\n" ++
    (take 3 $ drop 3 board) ++ "\n" ++
    (take 3 $ drop 6 board) ++ "\n" ++
    "---"

initBoard = Grid3x3 "         "

place (Grid3x3 board) move row col =
  let pos = (row-1) * 3 + (col-1)
  in if (row < 1 || row > 3 || col < 1 || col > 3 || board !! pos /= ' ')
     then error "invalid input"
     else Grid3x3 (take pos board ++ [move] ++ drop (pos+1) board)

gameEnd (Grid3x3 board) =
  if verify board "XXX"
  then XWin
  else if verify board "OOO"
  then OWin
  else if done board
  then Draw
  else Unfinished
  where
    verify board result =
      (take 3 board) == result ||
      (take 3 $ drop 3 board) == result ||
      (take 3 $ drop 6 board) == result ||
      (board !! 0 : board !! 3 : [board !! 6]) == result ||
      (board !! 1 : board !! 4 : [board !! 7]) == result ||
      (board !! 2 : board !! 5 : [board !! 8]) == result ||
      (board !! 0 : board !! 4 : [board !! 8]) == result ||
      (board !! 2 : board !! 4 : [board !! 6]) == result
    done board =
      and $ map (/=' ') board

gameLoop :: IO Board -> (IO Board -> IO Board) -> (IO Board -> IO Board) -> Turn -> IO GameResult
gameLoop ioBoard player1Strategy player2Strategy turn = do
  board <- ioBoard
  print board
  print turn
  case (gameEnd board, turn) of
    (Unfinished, Player1Turn) ->
      gameLoop (player1Strategy ioBoard) player1Strategy player2Strategy Player2Turn
    (Unfinished, Player2Turn) ->
      gameLoop (player2Strategy ioBoard) player1Strategy player2Strategy Player1Turn
    (result, _) -> do
      print result
      return result

firstUnplayedStrategy :: Char -> (IO Board) -> (IO Board)
firstUnplayedStrategy move ioBoard = do
  -- putStrLn "I am first unplayed strategy"
  (Grid3x3 board) <- ioBoard
  return $ Grid3x3 (firstUnplayed board)
  where firstUnplayed (' ' : tail) = move : tail
        firstUnplayed (h   : tail) = h : firstUnplayed tail

userInputStrategy :: Char -> (IO Board) -> (IO Board)
userInputStrategy move ioBoard = do
  -- putStrLn "I am user input strategy"
  board <- ioBoard
  putStr "Play Board Position (input 2 digits, e.g. 11): "
  input <- getLine
  case input of
    r:c:[] -> let row = ord r - ord '0'
                  col = ord c - ord '0'
              in return $ place board move row col
    _ -> error "invalid input"


main = do
  putStrLn "Welcome to TicTacToe"
  gameLoop
    (return initBoard)
    (firstUnplayedStrategy 'X')
    (firstUnplayedStrategy 'O')
    -- (userInputStrategy 'O')
    Player1Turn

  putStr "\n\n"
