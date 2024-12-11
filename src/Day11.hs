module Main where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (sepBy, char)

type K = (Int, Int) -- flips, number
type V = Int -- num final stones

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

digits :: Int -> [Int]
digits x = [read [c] | c <- show x]

count :: Map K V -> Int -> Int -> Int
count cache n x
  | n < 0 = error $ "negative n in: count ... " ++ show (n, x)
  | n == 0 = 1
  | Map.member (n, x) cache = cache ! (n, x)
  | x >= 10 = sum (count cache (n-1) <$> change x)
  | otherwise = error $ "digit should be in cache: " ++ show (n, x)

put :: K -> Map K V -> Map K V
put k@(n, x) dp = Map.insert k v dp
  where
    v | n < 0 = error $ "negative n in: put " ++ show k
      | n == 0 = 1
      | x == 0 = dp ! (n-1, 1)
      | x `elem` [1, 2, 3, 4] = case n of
          1 -> 1 -- x * 2024
          2 -> 2 -- [xx, xx]
          3 -> 4 -- [x, x, x, x]
          _ -> sum [dp ! (n-3, x') | x' <- digits (x * 2024)]
      | x `elem` [5, 6, 7, 9] = case n of
          1 -> 1 -- x * 2024
          2 -> 1 -- x * 2024 * 2024
          3 -> 2 -- [xxxx, xxxx]
          4 -> 4 -- [xx, xx, xx, xx]
          5 -> 8 -- [x, x, x, x, x, x, x, x]
          _ -> sum [dp ! (n-5, x') | x' <- digits (x * 2024 * 2024)]
      | x == 8 = case n of
          1 -> 1 -- x * 2024
          2 -> 1 -- x * 2024 * 2024
          3 -> 2 -- [xxxx, xxxx]
          4 -> 4 -- [xx, xx, xx, 8]
          _ -> sum [dp ! (n-5, x') | x' <- [3, 2, 7, 7, 2, 6]] + dp ! (n-4, 8)
      | otherwise =
        error $ "can only cache single digits, got: " ++ show k

cache = foldr ($) Map.empty [put (n, x) | n <- reverse [0..75], x <- [0..9]]

solve1 :: Solver
solve1 input = show (length stones')
  where
    stones = mustParse nums input
    stones' = foldr ($) stones (replicate 25 (>>= change))

solve2 :: Solver
solve2 input = show $ sum [count cache 75 x | x <- stones]
  where
    stones = mustParse nums input
    cache = foldr ($) Map.empty [put (n, x) | n <- reverse [0..75], x <- [0..9]]

main :: IO ()
main = runCLI solve1 solve2
