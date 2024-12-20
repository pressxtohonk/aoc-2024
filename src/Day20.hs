module Main where

import qualified Data.Map as Map
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, manyTill)

data Tile
  = Start
  | Floor
  | Wall
  | End
  deriving (Show, Eq)

tile :: Parser Tile
tile =
  anyOf
    [ Start <$ char 'S',
      Floor <$ char '.',
      Wall <$ char '#',
      End <$ char 'E'
    ]

maze :: Parser (Board.Board Tile)
maze = Board.fromLists <$> ((tile `manyTill` eol) `manyTill` end)

distL1 :: Board.Pos -> Board.Pos -> Int
distL1 (r1, c1) (r2, c2) = abs (r2 - r1) + abs (c2 - c1)

minCostPaths :: Board.Board Tile -> [[Board.Pos]]
minCostPaths (Board.Board nrow ncol cells) = Board.shortestPaths maze source target
  where
    source = head . Map.keys . Map.filter (== Start) $ cells
    target = head . Map.keys . Map.filter (== End) $ cells
    walls = Map.map (const ()) . Map.filter (== Wall) $ cells
    maze = Board.Board nrow ncol walls

solve1 :: Solver
solve1 input = show . length $ filter (>= 100) savings
  where
    board = mustParse maze input
    soln = head (minCostPaths board)
    path = zip [0 ..] soln
    savings = [t2 - t1 - d | (t1, i) <- path, (t2, j) <- path, t1 < t2, let d = distL1 i j, d <= 2]

solve2 :: Solver
solve2 input = show . length $ filter (>= 100) savings
  where
    board = mustParse maze input
    soln = head (minCostPaths board)
    path = zip [0 ..] soln
    savings = [t2 - t1 - d | (t1, i) <- path, (t2, j) <- path, t1 < t2, let d = distL1 i j, d <= 20]

main :: IO ()
main = runCLI solve1 solve2
