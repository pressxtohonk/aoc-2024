module Main where

import Data.Bits
import Data.List (tails)
import qualified Data.Map as Map
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

pricesFromSeed :: Int -> [Int]
pricesFromSeed = map (`mod` 10) . rngFromSeed

getSignals :: [Int] -> Map.Map [Int] Int
getSignals = foldr update Map.empty . runsOf 5
  where
    runsOf n = filter ((==n) . length) . map (take n) . tails
    update xs = Map.insert (signal xs) (last xs)
    signal xs = zipWith (-) (tail xs) xs

solve1 :: Solver
solve1 = show
       . sum
       . map (\x -> rngFromSeed x !! 2000)
       . mustParse (linesOf int)

solve2 :: Solver
solve2 = show 
       . maximum . Map.unionsWith (+)
       . map (getSignals . take 2001 . pricesFromSeed)
       . mustParse (linesOf int)

main :: IO ()
main = runCLI solve1 solve2
