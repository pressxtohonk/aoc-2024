module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Control.Applicative (asum)
import Data.Maybe (isJust)
import Control.Monad (guard)
import Data.List (stripPrefix)

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

data OpCode = Add | Mul | Cat deriving (Show, Eq)

-- The previous solver finds a function `f` that satisfies `f(xs) = y`.
-- This solver finds its inverse `f'` that satisfies `f'(y) = xs`
-- This works well as the inverses of integer multiplication (integer division) and string concatenation (string decomposition) have much stricter conditions than the originals.
solveFromBack :: Int -> [Int] -> Maybe [OpCode]
solveFromBack y xs = go [] y (reverse xs)
  where
    go :: [OpCode] -> Int -> [Int] -> Maybe [OpCode]
    go acc y' [] = Nothing
    go acc y' (x':xs')
      | null xs' && (y' == x') = Just acc
      | otherwise = asum [undoAdd, undoMul, undoCat]
      where
        undoAdd = do
          let y'' = y' - x'
          guard (y'' >= 0) -- ensure non-negative value after subtraction
          go (Add:acc) y'' xs'
        undoMul = do
          let (y'', r) = divMod y' x'
          guard (r == 0) -- ensure integer value after division
          go (Mul:acc) y'' xs'
        undoCat = do
          guard (x' /= y') -- ensure non-empty value after deconstruction
          let sx' = reverse (show x')
          let sy' = reverse (show y')
          y'' <- read . reverse <$> stripPrefix sx' sy'
          go (Cat:acc) y'' xs'

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
    hasSoln y = isJust . solveWith [(+), (*), cat] y
    hasSoln' y = isJust . solveFromBack y

main :: IO ()
main = runCLI solve1 solve2
