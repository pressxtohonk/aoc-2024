module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import PressXToGrids (Grid)

import qualified PressXToGrids as Grid
import Data.List (transpose, isPrefixOf, tails)

-- Each function reads 
transforms :: [[String] -> [String]]
transforms =
  [ id                         -- left to right
  , transpose                  -- top to bottom
  , transpose . reverse        -- bottom to top
  , fmap reverse               -- right to left
  , skew                       -- top right to bottom left
  , skew . transpose           -- bottom left to top right
  , skew . transpose . reverse -- bottom right to top left
  , skew . reverse . transpose -- top left to bottom right
  ]

-- Map diagonals onto rows
skew :: [String] -> [String]
skew = transpose . zipWith (++) padding
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
