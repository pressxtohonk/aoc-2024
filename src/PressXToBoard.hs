module PressXToBoard where

import Data.Map (Map)
import qualified Data.Map as Map

type Move = (Pos, Dir)

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Show, Ord, Eq)

turn :: Move -> Move
turn (pos, U) = (pos, R)
turn (pos, R) = (pos, D)
turn (pos, D) = (pos, L)
turn (pos, L) = (pos, U)

step :: Move -> Move
step ((r, c), U) = ((r - 1, c), U)
step ((r, c), D) = ((r + 1, c), D)
step ((r, c), L) = ((r, c - 1), L)
step ((r, c), R) = ((r, c + 1), R)

data Board a = Board
  { nrow :: Int,
    ncol :: Int,
    filled :: Map (Int, Int) a
  }
  deriving (Show)

fromLists :: [[a]] -> Board a
fromLists [] = Board 0 0 Map.empty
fromLists grid@(h:t)
  | all ((length h ==) . length) t = Board nrow ncol filled
  | otherwise = error "sublists have inconsistent width"
  where
    nrow = length grid
    ncol = length (head grid)
    filled = Map.fromAscList $ do
      (i, row) <- zip [0..] grid
      (j, val) <- zip [0..] row
      [((i, j), val)]

hasCell :: Board a -> Pos -> Bool
hasCell (Board nrow ncol _) (r, c)
  | (r < 1) || (nrow < r) = False
  | (c < 1) || (ncol < c) = False
  | otherwise = True

filledAt :: Board a -> Pos -> Bool
filledAt board pos = Map.member pos (filled board)

fill :: Board a -> Pos -> a -> Board a
fill board pos key = board { filled = Map.insert pos key (filled board) }
