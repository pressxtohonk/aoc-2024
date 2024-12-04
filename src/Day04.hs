module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
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

-- r×c sub-grid located at the top left of the original grid
subgrid :: Int -> Int -> [String] -> [String]
subgrid r c xs = take c <$> take r xs

-- counts X-mas pattern in the top left corner (0 or 1)
countXmas'' :: [String] -> Int
countXmas'' xs = case subgrid 3 3 xs of
  [['M',_,'S'],[_,'A',_],['M',_,'S']] -> 1
  [['M',_,'M'],[_,'A',_],['S',_,'S']] -> 1
  [['S',_,'M'],[_,'A',_],['S',_,'M']] -> 1
  [['S',_,'S'],[_,'A',_],['M',_,'M']] -> 1
  _ -> 0

-- counts crosses in the first col
countXmas' :: [String] -> Int
countXmas' xs = sum $ map countXmas'' (subgrids xs)
  where subgrids = tails

-- counts crosses in the grid
countXmas :: [String] -> Int
countXmas xs = sum $ map countXmas' (subgrids xs)
  where subgrids = fmap transpose . tails . transpose

solve1 :: Solver
solve1 input = show $ sum [ count "XMAS" line | f <- transforms, line <- f grid ]
  where
    grid = mustParse block input

solve2 :: Solver
solve2 = show . countXmas . mustParse block

main :: IO ()
main = runCLI solve1 solve2
