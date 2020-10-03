{-# OPTIONS -Wall #-}

module Maze.Board
  ( Height,
    Width,
    Coord,
    fromCoord,
    coord,
    coords,
    CoordState (..),
    Board,
    empty,
    apply,
    Direction (..),
    move,
    Maze.Board.print,
  )
where

import qualified AATree
import System.Random (Random (random, randomR))

type Height = Int

type Width = Int

data Coord = Coord {getY :: Height, getX :: Width} deriving (Show)

instance Eq Coord where
  Coord y1 x1 == Coord y2 x2 = y1 == y2 && x1 == x2

instance Ord Coord where
  compare (Coord y1 x1) (Coord y2 x2) = compare y1 y2 <> compare x1 x2

up :: Coord -> Coord
up (Coord y x) = Coord (y + 1) x

down :: Coord -> Coord
down (Coord y x) = Coord (y - 1) x

left :: Coord -> Coord
left (Coord y x) = Coord y (x - 1)

right :: Coord -> Coord
right (Coord y x) = Coord y (x + 1)

fromCoord :: Coord -> (Height, Width)
fromCoord c = (getY c, getX c)

coord :: Height -> Width -> Coord
coord y x = Coord y x

coords :: Height -> Width -> [Coord]
coords h w = [coord y x | y <- [0 .. h], x <- [0 .. w]]

data CoordState = Road | Wall | Ans | Start | Goal deriving (Show, Eq, Ord)

type Board = AATree.AATree Coord CoordState

empty :: Height -> Width -> Board
empty h w = AATree.fromList [(c, edge c) | c <- coords h w]
  where
    edge (Coord y x)
      | y == 0 || x == 0 || y == h || x == w = Wall
      | otherwise = Road

apply :: [Coord] -> CoordState -> Board -> Board
apply [] _ board = board
apply (c : cs) s board = apply cs s $ AATree.edit c s board

data Direction = Up | Down | Left | Right
  deriving (Eq, Show, Enum, Bounded)

instance Random Direction where
  randomR (mn, mx) gen =
    let (a, g) = randomR (fromEnum mn, fromEnum mx) gen
     in (toEnum a, g)
  random g = randomR (minBound, maxBound) g

move :: Direction -> Coord -> Coord
move Maze.Board.Up = up
move Maze.Board.Down = down
move Maze.Board.Left = left
move Maze.Board.Right = right

print ::
  String ->
  String ->
  String ->
  String ->
  String ->
  Board ->
  IO ()
print road wall ans start goal board = put $ AATree.toList board
  where
    put [] = return ()
    put ((_, s) : []) = putStrLn $ state s
    put (((Coord y _), s) : c@((Coord y' _), _) : cs) = do
      if y == y'
        then putStr $ state s
        else putStrLn $ state s
      put (c : cs)
    state Road = road
    state Wall = wall
    state Ans = ans
    state Start = start
    state Goal = goal
