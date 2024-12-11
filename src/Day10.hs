module Main where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Data.List (nub)

type Loc = (Int, Int) -- row, col
type Cell = (Int, Int) -- label, score
type CellMap = Map Loc Cell

cell :: Parser (Loc, Cell)
cell = do
  (r, c, n) <- withCoord digit
  return ((r, c), (read [n], 0))

adjLocs :: CellMap -> Loc -> [Loc]
adjLocs cells (r, c) = filter (`Map.member` cells) peers
  where 
    peers = [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]

step :: CellMap -> Loc -> [Loc]
step cells loc = filter ((label loc+1==) . label) (adjLocs cells loc)
  where 
    label = fst . (cells !)

trailEnds :: CellMap -> Loc -> [Loc]
trailEnds cells loc = foldr ($) [loc] [(>>= step cells) | _ <- [1..9]]
--  [loc] -- start at 0
--    >>= step cells -- walk to 1
--    >>= step cells -- walk to 2
--    >>= step cells -- walk to 3
--    >>= step cells -- walk to 4
--    >>= step cells -- walk to 5
--    >>= step cells -- walk to 6
--    >>= step cells -- walk to 7
--    >>= step cells -- walk to 8
--    >>= step cells -- walk to 9

distinct :: Eq a => [a] -> [a]
distinct = nub -- O[n²]
-- distinct = Set.toList . Set.fromList -- O[n*log(n)]

solve1 :: Solver
solve1 input = show $ sum (length . distinct . trailEnds cells <$> trailHeads)
  where
    board = mustParse (many $ try cell) input
    cells = Map.fromList board
    trailHeads = [loc | (loc, (label, _)) <- board, label == 0]

solve2 :: Solver
solve2 input = show $ sum (length . trailEnds cells <$> trailHeads)
  where
    board = mustParse (many $ try cell) input
    cells = Map.fromList board
    trailHeads = [loc | (loc, (label, _)) <- board, label == 0]

-- Optimized part 2 solution with "memoization"
update :: CellMap -> Loc -> CellMap
update cells (r, c) = case Map.lookup (r, c) cells of
  Just (label, _) 
    | label == 9 -> Map.insert (r, c) (label, 1) cells
    | otherwise -> Map.insert (r, c) (label, score) cells
    where
      peers = (cells !) <$> adjLocs cells (r, c)
      score = sum [score | (label', score) <- peers, label'==label+1 ]
  _ -> cells

solve2' :: Solver
solve2' input = show $ sum [score | (_, score) <- (board' !) <$> locs ! 0]
  where
    board = mustParse (many $ try cell) input
    cells = Map.fromList board
    locs = Map.fromListWith (++) [(label, [loc]) | (loc, (label, _)) <- board]
    bulkUpdate = foldl update
    board' = foldl bulkUpdate cells $ (locs !) <$> reverse [0..9]

main :: IO ()
main = runCLI solve1 solve2
