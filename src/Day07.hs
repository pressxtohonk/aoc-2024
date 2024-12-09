module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

-- parse input
equation :: Parser (Int, [Int])
equation = do
  y <- int
  string ": "
  xs <- int `sepBy` char ' '
  return (y, xs)

-- why do hard when simple do trick
search :: [Int -> Int -> Int] -> [Int] -> [Int]
search ops = go . reverse
  where
    go (x:xs@(_:_)) = ops >>= (`map` go xs) . ($ x)
    go xs = xs

-- integer concatenation
(~) :: Int -> Int -> Int
x ~ y = head [n*x+y | n <- offsets, n > y]
  where offsets = 10 : fmap (10 *) offsets

solve1 :: Solver
solve1 input = show $ sum [y | (y, xs) <- equations, y `elem` tree xs]
  where
    equations = mustParse (linesOf equation) input
    tree = search [(+), (*)]

solve2 :: Solver
solve2 input = show $ sum [y | (y, xs) <- equations, y `elem` tree xs]
  where
    equations = mustParse (linesOf equation) input
    tree = search [(+), (*), flip (~)]

main :: IO ()
main = runCLI solve1 solve2
