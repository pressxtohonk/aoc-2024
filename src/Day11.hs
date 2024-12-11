module Main where

import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Map (Map, (!))
import qualified Data.Map as Map
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (sepBy, char)

nums :: Parser [Int]
nums = int `sepBy` char ' '

change :: Int -> [Int]
change n
  | n == 0 = [1]
  | evenDigits n = halves n
  | otherwise = [2024 * n]

evenDigits :: Int -> Bool
evenDigits = even . length . show

halves :: Int -> [Int]
halves x = [read (take n sx), read (drop n sx)]
  where
    sx = show x
    n = length sx `div` 2

count :: Int -> Int -> State (Map (Int, Int) Int) Int
count n x = do
  cache <- State.get
  let
    tryCache
      | Map.member (n, x) cache = return (cache ! (n, x))
      | otherwise = computed
    computed
      | n == 0 = return 1
      | x == 0 = count (n-1) 1
      | evenDigits x = sum <$> traverse (count (n-1)) (halves x)
      | otherwise = count (n-1) (2024*x)
  y <- tryCache
  State.modify (Map.insert (n, x) y)
  return y

solve1 :: Solver
solve1 input = show (length stones')
  where
    stones = mustParse nums input
    stones' = foldr ($) stones (replicate 25 (>>= change))

solve2 :: Solver
solve2 input = show . sum $ State.evalState (traverse (count 75) stones) Map.empty
  where
    stones = mustParse nums input

main :: IO ()
main = runCLI solve1 solve2
