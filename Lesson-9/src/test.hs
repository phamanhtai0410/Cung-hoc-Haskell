data Maze = Maze
  { grid :: [[Char]],
    playerPos :: (Int, Int),
    exitPos :: (Int, Int)
  }
  deriving (Show)

data Move = LeftMove | RightMove | UpMove | DownMove deriving (Show, Eq)

move :: Maze -> Move -> Maze
move maze move
  | move == LeftMove && isValidMove (_x, _y - 1) maze = Maze _grid (_x, _y - 1) _exitPos
  | move == RightMove && isValidMove (_x, _y + 1) maze = Maze _grid (_x, _y + 1) _exitPos
  | move == UpMove && isValidMove (_x - 1, _y) maze = Maze _grid (_x - 1, _y) _exitPos
  | move == DownMove && isValidMove (_x + 1, _y) maze = Maze _grid (_x + 1, _y) _exitPos
  | otherwise = maze
  where
    _x = fst (playerPos maze)
    _y = snd (playerPos maze)
    _grid = grid maze
    _exitPos = exitPos maze

isValidMove :: (Int, Int) -> Maze -> Bool
isValidMove (x, y) maze =
  x >= 0 && x < length mazeRows && y >= 0 && y < length (head mazeRows) && (mazeRows !! x) !! y /= '#'
  where
    mazeRows = grid maze

showCurrentChoice :: Maze -> String
showCurrentChoice maze
  | playerPos maze == exitPos maze = "YOU'VE FOUND THE EXIT!!"
  | (grid maze !! fst (playerPos maze)) !! snd (playerPos maze) == '#' = "You've hit a wall!"
  | otherwise = "You're still inside the maze."

solveMaze :: Maze -> [Move] -> Maze
solveMaze = foldl move

testMaze :: Maze
testMaze =
  Maze
    { grid =
        [ "####",
          "#..#",
          "#..#",
          "####"
        ],
      playerPos = (1, 1),
      exitPos = (2, 2)
    }

testMoves :: [Move]
testMoves = [UpMove, RightMove]

main :: IO ()
main = do
  let solvedMaze = solveMaze testMaze testMoves
  putStrLn $ showCurrentChoice solvedMaze
  print $ fst (playerPos solvedMaze)
  print $ snd (playerPos solvedMaze)