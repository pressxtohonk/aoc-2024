module PressXToBoard where

import Control.Applicative (asum)
import Data.Map (Map)
import qualified Data.Map as Map

type Move = (Pos, Dir)

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Show, Ord, Eq)

pos :: Move -> Pos
pos = fst

dir :: Move -> Dir
dir = snd

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

(!) :: Board a -> Pos -> a
board ! (r, c)
  | (0 > r) || (r >= nrow board) = error $ "row out of bounds: " ++ show r
  | (0 > c) || (c >= ncol board) = error $ "col out of bounds: " ++ show c
  | otherwise = case board ? (r, c) of
      Nothing -> error $ "board has no index " ++ show (r, c)
      Just x -> x

(?) :: Board a -> Pos -> Maybe a
board ? pos = Map.lookup pos (filled board)

hasCell :: Board a -> Pos -> Bool
hasCell (Board nrow ncol _) (r, c)
  | (r < 1) || (nrow < r) = False
  | (c < 1) || (ncol < c) = False
  | otherwise = True

filledAt :: Board a -> Pos -> Bool
filledAt board pos = Map.member pos (filled board)

fill :: Board a -> Pos -> a -> Board a
fill board pos key = board { filled = Map.insert pos key (filled board) }

data WalkResult
  = Completed [Move]
  | CycleError [Move]
  | OutOfBounds Move
  deriving (Show)

-- Walk a single path until an end condition is met or an error occurs
walkUntil :: (Move -> Move) -> (Move -> Bool) -> Board a -> Move -> WalkResult
walkUntil next done = walk
  where
    walk board = go []
      where
        go :: [Move] -> Move -> WalkResult
        go path move
          | done move = Completed (move:path)
          | move `elem` path = CycleError path
          | null (board ? pos move)= OutOfBounds move
          | otherwise = go (move:path) (next move)

-- Explore paths until a condition is met or all paths are exhausted.
searchFor :: (Move -> [Move]) -> (Move -> Bool) -> Board a -> Move -> Maybe [Move]
searchFor next done = search
  where
    search board = go []
      where
        go :: [Move] -> Move -> Maybe [Move]
        go path move 
          | done move = Just (move:path)
          | move `elem` path = Nothing
          | null (board ? pos move) = Nothing
          | otherwise = asum (go (move:path) <$> next move)
