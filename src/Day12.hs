module Main where

import Control.Monad.State (State, get, modify, execState)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (isJust, maybeToList, fromMaybe, catMaybes, mapMaybe)
import PressXToGrids (Grid, (?))
import qualified PressXToGrids as Grid
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Data.List (uncons)

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
regions grid = [[regionMap ! (r-1, c-1) | c <- [1..ncol]] | r <- [1..nrow]]
  where
    nrow = Grid.nrow grid
    ncol = Grid.ncol grid
    coords = [(r-1, c-1) | r <- [1..nrow], c <- [1..ncol]]
    update = traverse (uncurry $ fill grid) (zip [0..] coords)
    regionMap = execState update Map.empty

type RegionState = State (Map (Pair Int) Int)

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
solve2 = show

main :: IO ()
main = runCLI solve1 solve2

showGrid :: Show a => [[a]] -> String
showGrid = unlines . map (unwords . map show)
