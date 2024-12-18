module PressXToBoard where

import Control.Applicative (asum)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

type Move = (Pos, Dir)

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Show, Ord, Eq)

adj :: Pos -> [Pos]
adj (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

pos :: Move -> Pos
pos = fst

dir :: Move -> Dir
dir = snd

turnL :: Move -> Move
turnL (pos, U) = (pos, L)
turnL (pos, R) = (pos, U)
turnL (pos, D) = (pos, R)
turnL (pos, L) = (pos, D)

turnR :: Move -> Move
turnR (pos, U) = (pos, R)
turnR (pos, R) = (pos, D)
turnR (pos, D) = (pos, L)
turnR (pos, L) = (pos, U)

step :: Move -> Move
step ((r, c), U) = ((r - 1, c), U)
step ((r, c), D) = ((r + 1, c), D)
step ((r, c), L) = ((r, c - 1), L)
step ((r, c), R) = ((r, c + 1), R)

data Board a = Board
  { nrow :: Int,
    ncol :: Int,
    filled :: Map.Map (Int, Int) a
  }
  deriving (Show)

fromLists :: [[a]] -> Board a
fromLists [] = Board 0 0 Map.empty
fromLists grid@(h : t)
  | all ((length h ==) . length) t = Board nrow ncol filled
  | otherwise = error "sublists have inconsistent width"
  where
    nrow = length grid
    ncol = length (head grid)
    filled = Map.fromAscList $ do
      (i, row) <- zip [0 ..] grid
      (j, val) <- zip [0 ..] row
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
  | (0 > r) || (r >= nrow) = False
  | (0 > c) || (c >= ncol) = False
  | otherwise = True

filledAt :: Board a -> Pos -> Bool
filledAt board pos = Map.member pos (filled board)

emptyAt :: Board a -> Pos -> Bool
emptyAt board pos = Map.notMember pos (filled board)

canFill :: Board a -> Pos -> Bool
canFill board pos = (board `hasCell` pos) && (board `emptyAt` pos)

fill :: Board a -> Pos -> a -> Board a
fill board pos key = board {filled = Map.insert pos key (filled board)}

peers :: Board a -> Pos -> [Pos]
peers board pos = filter (board `canFill`) (adj pos)

-- Move based pathfinding

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
          | done move = Completed (move : path)
          | move `elem` path = CycleError path
          | null (board ? pos move) = OutOfBounds move
          | otherwise = go (move : path) (next move)

-- Explore paths until a condition is met or all paths are exhausted.
searchFor :: (Move -> [Move]) -> (Move -> Bool) -> Board a -> Move -> Maybe [Move]
searchFor next done = search
  where
    search board = go []
      where
        go :: [Move] -> Move -> Maybe [Move]
        go path move
          | done move = Just (move : path)
          | move `elem` path = Nothing
          | null (board ? pos move) = Nothing
          | otherwise = asum (go (move : path) <$> next move)

-- Position based pathfinding

type PosPath = [Pos]

type PosState = State (Set.Set Pos)

-- Monadic action to branch out from a board position to unvisited adjacent positions
explore :: Board () -> PosPath -> PosState (PosPath, [PosPath])
explore board path = case path of
  (pos : _) -> do
    seen <- get
    let conditions = [(`Set.notMember` seen), (board `hasCell`), (board `emptyAt`)]
        allPeers = peers board pos
        newPeers = foldr filter allPeers conditions
        visited = pos : newPeers
    put (foldr Set.insert seen visited)
    return (path, [peer : path | peer <- newPeers])
  _ -> return ([], [])

-- Builds a tree where level `n` contains all non-overlapping paths at BFS depth `n
-- Runs in O(V) as each node is only visited once
-- FIXME: Collect more information to allow for discovering multiple shortest paths.
bfsTree :: Board () -> Pos -> Tree.Tree PosPath
bfsTree board pos = evalState (Tree.unfoldTreeM_BF (explore board) [pos]) Set.empty

-- Given a board, source position and target position, returns all shortest paths.
-- NOTE: If multiple paths exist with the min length, only returns the first 
shortestPath :: Board () -> Pos -> Pos -> [PosPath]
shortestPath board start target = search [bfsTree board start]
  where
    isSoln = (target==) . head
    search [] = []
    search forest
      | null soln = search (forest >>= Tree.subForest)
      | otherwise = soln
      where
        soln = [path | Tree.Node path _ <- forest, isSoln path]
