module Main where

import Data.Bits
import Data.List (tails)
import PressXToParse
import PressXToSolve (Solver, runCLI)

evolve :: Int -> Int
evolve = step3 . step2 . step1
  where
    step1 x = prune $ mix x (x * 64)
    step2 x = prune $ mix x (x `div` 32)
    step3 x = prune $ mix x (x * 2048)

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (`mod` 16777216)

rngFromSeed :: Int -> [Int]
rngFromSeed = iterate evolve

signal :: [Int] -> [Int]
signal xs = zipWith (-) (tail xs) xs

solve1 :: Solver
solve1 = show . sum . map (\x -> rngFromSeed x !! 2000) . mustParse (linesOf int)

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
