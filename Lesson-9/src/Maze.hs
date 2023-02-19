{-

\**************************** IMPORTANT ****************************

This week is a two-step homework. First, you have to solve the
"Maze" challenge, and then the "Forest" challenge. The challenges
are in two separate files in both the homework and solution, so
you can check the solution for the first "Maze" challenge without
spoilers of the "Forest" one. Make sure to check the solution for
"Maze" (and only "Maze," I see you 🥸👀) before starting with the
"Forest" challenge!

\*******************************************************************

Today, you'll build the simplest and most basic game imaginable.
It'll be a maze game where the player has to write a list of moves, and the game will perform them
and tell the player where it ends up. Then, the player can change the moves and check again until it
finds the exit.

To play the game, the player will open GHCi, load this file, and run a "solveMaze" function that
takes a maze and a list of moves and returns a String with the resulting state.

It should look like this:

\*Main> solveMaze testMaze []
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
\*Main> solveMaze testMaze [GoLeft]
"You've hit a wall!"
\*Main> solveMaze testMaze [GoForward]
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
\*Main> solveMaze testMaze [GoForward, GoRight]
"You've hit a wall!"
\*Main> solveMaze testMaze [GoForward, GoLeft]
"YOU'VE FOUND THE EXIT!!"

How are you going to achieve this? You can try it on your own, but here you have a
step-by-step just in case:

1. Write two data types. One for the moves (Move) you can make, and another for the maze (Maze).
(Use the example above to figure them out.)

2. Write a function called "move" that takes a maze and a move and returns the maze after the move.

3. Write a "testMaze" value of type "Maze" and test the "move" function in GHCi.

4. Write the "solveMaze" function that will take a maze and a list of moves and returns the maze
after making those moves.

5. If you test the "solveMaze" function, you'll see that each time you try to solve the maze,
it'll print the whole maze for the player to see. To avoid that, write a "showCurrentChoice" function
that takes a maze and returns a different string depending on if you hit a wall, found the exit, or
still need to make another choice.

6. Adapt adapt "solveMaze" function to use "showCurrentChoice" and play with your new game using GHCi! :D
-}
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