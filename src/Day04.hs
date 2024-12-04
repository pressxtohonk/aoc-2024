module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import PressXToGrids (Grid, (?))

import qualified PressXToGrids as Grid
import Data.List (transpose, isPrefixOf, tails)
import Data.Bifunctor (Bifunctor(second, first))

rotations :: [[String] -> [String]]
rotations =
  [ id      -- 0°
  , Grid.r1 -- 90°
  , Grid.r2 -- 180°
  , Grid.r3 -- 270°
  ]

transforms :: [[String] -> [String]]
transforms = rotations ++ map (skew .) rotations

-- 45° rotation
skew :: [String] -> [String]
skew = transpose . zipWith (++) padding . transpose
  where
    padding = [replicate n ' ' | n <- [0..]]

-- number of times a word appears in a line (overlaps allowed)
count :: String -> String -> Int
count word line = length $ filter (word `isPrefixOf`) (tails line)

-- counts X-mas pattern in the top left corner (0 or 1)
isXmas :: Grid Char -> Bool
isXmas xs = case xs of
  [['M',_,'S'],[_,'A',_],['M',_,'S']] -> True
  [['M',_,'M'],[_,'A',_],['S',_,'S']] -> True
  [['S',_,'M'],[_,'A',_],['S',_,'M']] -> True
  [['S',_,'S'],[_,'A',_],['M',_,'M']] -> True
  _ -> False

type Step = Pair Int -> Pair Int

u = first succ
d = first pred
l = second pred
r = second succ

search :: Grid Char -> Step -> Pair Int -> String -> Bool
search grid next = search'
  where
    search' pos word = case (grid ? pos, word) of
      (_, "")      -> True  -- word found!
      (Nothing, _) -> False -- pos out of bounds
      (Just actual, expected:rest) ->
        (actual == expected) && search' (next pos) rest

-- alternate naive grid traversal solution
-- (See solve2 for generic pattern search)
solve1' :: Solver
solve1' input = show $ length hits
  where
    grid = Grid.fromLists $ mustParse block input
    nrow = Grid.nrow grid
    ncol = Grid.ncol grid
    hits = filter id $ do
      direction <- [u, d, l, r, u.l, u.r, d.l, d.r]
      i <- [0..nrow-1]
      j <- [0..ncol-1]
      return $ search grid direction (i, j) "XMAS"

-- hacky (: search is fixed from left to right, but the grid rotates
solve1 :: Solver
solve1 input = show $ sum [ count "XMAS" line | f <- transforms, line <- f grid ]
  where
    grid = mustParse block input

solve2 :: Solver
solve2 input = show $ length (filter isXmas _3x3)
  where
    grid = Grid.fromLists $ mustParse block input
    _3x3 = Grid.take 3 3 <$> Grid.tails grid

main :: IO ()
main = runCLI solve1 solve2
