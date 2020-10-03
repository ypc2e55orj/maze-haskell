{-# OPTIONS -Wall #-}

module Maze.Generator (generator) where

-- 参考:
-- https://algoful.com/Archive/Algorithm/MazeExtend

import qualified AATree as AATree
import Control.Monad.State (MonadState (state), State)
import qualified Maze.Board as Board
import System.Random (Random (random), StdGen)

evens :: Board.Height -> Board.Width -> [Board.Coord]
evens h w =
  [ Board.coord y x | y <- [2, 4 .. h - 1], x <- [2, 4 .. w - 1]
  ]

extend :: [Board.Coord] -> Board.Board -> State StdGen [Board.Coord]
extend [] _ = return []
extend prevs@(prev : _) board = do
  direction <- state random
  let current1 = Board.move direction prev
      current2 = Board.move direction current1
      current = current2 : current1 : prevs
  if current2 `elem` prevs
    then let (_ : _ : back) = prevs in extend back board
    else case AATree.search current2 board of
      Just Board.Road -> extend current board
      _ -> return current

process :: [Board.Coord] -> Board.Board -> State StdGen Board.Board
process [] board = return board
process (e : es) board =
  case AATree.search e board of
    Just Board.Road -> do
      wall <- extend [e] board
      process es $ Board.apply wall Board.Wall board
    _ -> process es board

generator :: Board.Height -> Board.Width -> State StdGen Board.Board
generator h w = process (evens h w) (Board.empty h w)
