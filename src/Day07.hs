module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Control.Applicative (asum)
import Data.Maybe (isJust)

type Op = (Int -> Int -> Int)

equation :: Parser (Int, [Int])
equation = do
  y <- int
  string ": "
  xs <- int `sepBy` char ' '
  return (y, xs)

-- given a set of left associative binary operations, find a sequence that, when applied to a list of inputs, produces the target output.
solveWith :: [Op] -> Int -> [Int] -> Maybe [Op]
solveWith ops y []     = Nothing
solveWith ops y (x:xs) = go [] x xs
  where
    go acc y' inputs = case inputs of
      (x':xs') | y' <= y -> asum [go (op:acc) (op y' x') xs' | op <- ops]
      []       | y' == y -> Just (reverse acc)
      _ -> Nothing

-- concatenates two integers
cat :: Op
cat a b = read (show a ++ show b)

solve1 :: Solver
solve1 input = show $ sum [ y | (y, xs) <- equations, hasSoln y xs ]
  where
    equations = mustParse (linesOf equation) input
    hasSoln y = isJust . solveWith [(+), (*)] y

solve2 :: Solver
solve2 input = show $ sum [ y | (y, xs) <- equations, hasSoln' y xs ]
  where
    equations = mustParse (linesOf equation) input
    hasSoln' y = isJust . solveWith [(+), (*), cat] y

main :: IO ()
main = runCLI solve1 solve2
