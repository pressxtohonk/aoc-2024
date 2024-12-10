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

score :: CellMap -> Loc -> Int
score cells loc = length (nub bfsEnd)
  where
    bfsStart = [loc]
    bfsEnd = foldr ($) bfsStart (replicate 9 (>>= step cells))

update :: CellMap -> Loc -> CellMap
update cells (r, c) = case Map.lookup (r, c) cells of
  Just (label, _) 
    | label == 9 -> Map.insert (r, c) (label, 1) cells
    | otherwise -> Map.insert (r, c) (label, score) cells
    where
      peers = (cells !) <$> adjLocs cells (r, c)
      score = sum [score | (label', score) <- peers, label'==label+1 ]
  _ -> cells

solve1 :: Solver
solve1 input = show $ sum (score cells <$> locs ! 0)
  where
    board = mustParse (many $ try cell) input
    cells = Map.fromList board
    locs = Map.fromListWith (++) [(label, [loc]) | (loc, (label, _)) <- board]

solve2 :: Solver
solve2 input = show $ sum [score | (_, score) <- (board' !) <$> locs ! 0]
  where
    board = mustParse (many $ try cell) input
    cells = Map.fromList board
    locs = Map.fromListWith (++) [(label, [loc]) | (loc, (label, _)) <- board]
    bulkUpdate = foldl update
    board' = foldl bulkUpdate cells $ (locs !) <$> reverse [0..9]

main :: IO ()
main = runCLI solve1 solve2
