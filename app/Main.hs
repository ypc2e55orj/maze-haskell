module Main where

import Control.Monad.State
import qualified Maze.Board as Board
import Maze.Generator (generator)
import System.Random (getStdGen)

main :: IO ()
main = do
  gen <- getStdGen
  let (maze, _) = runState (generator 50 50) gen
  Board.print
    "  "
    "##"
    "@@"
    "  "
    "  "
    maze

{-
  Board.print
    "\x1b[47m  \x1b[m"
    "\x1b[42m  \x1b[m"
    "@@"
    " "
    " "
    maze
-}
