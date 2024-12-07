module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

equation :: Parser (Int, [Int])
equation = do
  y <- int
  string ": "
  xs <- int `sepBy` char ' '
  return (y, xs)

hasSoln :: Int -> [Int] -> Bool
hasSoln y = search 0
  where
    search y' xs
      | y == y' = True
      | y < y' = False
      | otherwise = case xs of
        [] -> False
        (x:xs') -> search (y'*x) xs' || search (y'+x) xs'

hasSoln' :: Int -> [Int] -> Bool
hasSoln' y = search 0
  where
    search y' nums
      | y' > y = False
      | otherwise = case nums of
        [] -> y' == y
        (x:xs) -> search (cat y' x) xs
               || search ((*) y' x) xs
               || search ((+) y' x) xs

cat :: Int -> Int -> Int
cat a b = read (show a ++ show b)

solve1 :: Solver
solve1 input = show $ sum [ y | (y, xs) <- equations, hasSoln y xs ]
  where
    equations = mustParse (linesOf equation) input

solve2 :: Solver
solve2 input = show $ sum [ y | (y, xs) <- equations, hasSoln' y xs ]
  where
    equations = mustParse (linesOf equation) input

main :: IO ()
main = runCLI solve1 solve2
