module PressXToBoard where

import Data.Set (Set)
import qualified Data.Set as Set

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

data Board = Board
  { nrow :: Int,
    ncol :: Int,
    filled :: Set (Int, Int)
  }
  deriving (Show)

hasCell :: Board -> Pos -> Bool
hasCell (Board nrow ncol _) (r, c)
  | (r < 1) || (nrow < r) = False
  | (c < 1) || (ncol < c) = False
  | otherwise = True

filledAt :: Board -> Pos -> Bool
filledAt board pos = Set.member pos (filled board)

fill :: Board -> Pos -> Board
fill board pos = board { filled = Set.insert pos (filled board) }
