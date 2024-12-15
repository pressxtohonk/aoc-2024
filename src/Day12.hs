module Main where

import Control.Applicative (asum)
import Control.Monad.State (StateT, State, get, modify, lift, execState, runStateT)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import PressXToBoard (Board(Board), Move, Dir(R), turnL, turnR, step)
import qualified PressXToBoard as Board
import PressXToGrids (Grid, (?))
import qualified PressXToGrids as Grid
import PressXToParse
import PressXToSolve (Solver, runCLI)

type B = Board ()

-- Labels each grid cell with the number of differing neighbours
numAdjDiff :: Eq a => [[a]] -> [[Int]]
numAdjDiff grid = foldl add zeros diffs
  where
    add = zipWith (zipWith (+))
    zeros = map (map (const 0)) grid
    diffs = 
      [ map diff grid
      , Grid.r3 . map diff . Grid.r1 $ grid
      , Grid.r2 . map diff . Grid.r2 $ grid
      , Grid.r1 . map diff . Grid.r3 $ grid
      ]

-- Returns a same sized list indicating if each element is identical to the previous
diff :: Eq a => [a] -> [Int]
diff = ((\x -> if x then 1 else 0) <$>) . go Nothing
  where
    go :: Eq a => Maybe a -> [a] -> [Bool]
    go prev [] = []
    go prev (x:xs) = let curr = Just x in (prev /= curr) : go curr xs

regions :: Grid Char -> Grid Int
regions grid = [[region ! (r-1, c-1) | c <- [1..ncol]] | r <- [1..nrow]]
  where
    nrow = Grid.nrow grid
    ncol = Grid.ncol grid
    region = regionMap grid

type RegionState = State (Map (Pair Int) Int)

regionMap :: Grid Char -> Map (Pair Int) Int
regionMap grid = execState update Map.empty
  where
    nrow = Grid.nrow grid
    ncol = Grid.ncol grid
    coords = [(r-1, c-1) | r <- [1..nrow], c <- [1..ncol]]
    update = traverse (uncurry $ fill grid) (zip [0..] coords)

fill :: Grid Char -> Int -> Pair Int -> RegionState ()
fill grid region pos = do
  regions <- get
  case (grid ? pos, Map.lookup pos regions) of
    (Just label, Nothing) -> sequence_ (fillPos : fillPeers)
      where
        peers = filter ((Just label==) . (grid ?)) (adj pos)
        fillPos = modify (Map.insert pos region)
        fillPeers = fill grid region <$> peers
    _ -> return ()

adj :: Pair Int -> [Pair Int]
adj (r, c) = [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]

numSides :: Grid Char -> Map Int Int
numSides grid = Map.map numTurns boards
  where
    boards :: Map Int B
    boards = Map.map (Board 0 0) 
           . Map.fromListWith Map.union
           . map (\(pos, region) -> (region, Map.singleton pos ()))
           $ Map.toList (regionMap grid)

numTurns :: B -> Int
numTurns board = go Set.empty moves
  where
    moves = (, R) . fst <$> Map.toList (Board.filled board)
    go seen [] = 0
    go seen (x:xs) = case walk board x seen of
      Nothing -> go seen xs
      Just (n, seen') -> n + go seen' xs

type WalkState = StateT (Set Move) Maybe

walk :: B -> Move -> Set Move -> Maybe (Int, Set Move)
walk board move@(pos, _) = runStateT (go move)
  where
    inBounds = Board.filledAt board . fst
    leftWall = step . turnL
    onPath move = inBounds move && (not . inBounds . step . turnL $ move)

    go :: Move -> WalkState Int
    go move = do
      seen <- get
      if Set.member move seen 
        then return 0
        else if onPath move 
          then modify (Set.insert move) >> asum nexts
          else lift Nothing
      where
        nexts =
          [ (+1) <$> go (step . turnL . step $ move)
          , go (step move)
          , (+1) <$> go (turnR move)
          ]

solve1 :: Solver
solve1 input = show $ sum costs
-- solve1 input = unlines ["", unlines plots, showGrid labels, showGrid counts]
  where
    plots = Grid.fromLists $ mustParse block input
    labels = concat (regions plots)
    counts = concat (numAdjDiff plots)
    areas = Map.fromListWith (+) (map (, 1) labels)
    dists = Map.fromListWith (+) (zip labels counts)
    costs = Map.unionWith (*) areas dists

solve2 :: Solver
solve2 input = show $ sum costs
  where
    plots = Grid.fromLists $ mustParse block input
    labels = concat (regions plots)
    areas = Map.fromListWith (+) (map (, 1) labels)
    dists = numSides plots
    costs = Map.unionWith (*) areas dists

main :: IO ()
main = runCLI solve1 solve2

showGrid :: Show a => [[a]] -> String
showGrid = unlines . map (unwords . map show)
