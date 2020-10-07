{-# OPTIONS -Wall #-}

module Maze.Solver where

import qualified AATree as AATree
import Control.Monad.State (MonadState (state), State)
import qualified Maze.Board as Board
import System.Random (Random (randomR), StdGen)

start :: Board.Board -> State StdGen (Board.Coord, Board.Board)
start board = do
  let roads = filter (\(_, s) -> s == Board.Road) $ AATree.toList board
      len = length roads
  r <- state $ randomR (0, len - 1)
  let (s, _) = roads !! r
      boardWithStart = Board.apply [s] Board.Start board
  return (s, boardWithStart)
