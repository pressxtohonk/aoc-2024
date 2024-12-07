module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec
import Control.Applicative (asum)
import Data.Maybe (isJust, fromJust)
import Control.Monad (guard)
import Data.List (find)

equation :: Parser (Int, [Int])
equation = do
  y <- int
  string ": "
  xs <- int `sepBy` char ' '
  return (y, xs)

-- lowest power of ten greater than x
-- NOTE: Hack to help to avoid *slow* string operations in `cat` and `cat'`
-- These operations run at every branch of the search tree in part 2, so the
-- performance boost from avoiding string manipulation is significant.
offset :: Int -> Int
offset x = fromJust $ find (x <) tens where tens = 10 : fmap (10 *) tens

-- Binary operation that can fail on invalid inputs
type Op = (Int -> Int -> Maybe Int)

-- Forward operations
add :: Op
add a b = Just (a + b)

mul :: Op
mul a b = Just (a * b)

cat :: Op
cat a b = Just (offset b * a + b)

-- Backward operations (inverses)

-- subtraction
add' :: Op
add' y x = do
  let diff = y - x
  guard (diff >= 0)
  return diff

-- "Perfect" integer division
mul' :: Op
mul' y x = do
  let (q, r) = quotRem y x
  guard (r == 0)
  return q

-- "Perfect" suffix removal
cat' :: Op
cat' y x = do
  let (q, r) = divMod (y - x) (offset x)
  guard (r == 0)
  guard (q > 0)
  return q

-- Given a set of left associative binary operations, returns a list that folds
-- an initial value and a list of intermediate values into a target value.
-- i.e. the following identity is true:
--   fs = solveWith ops y x0 xs
--   y' = foldl (flip ($)) x0 (zipWith ($) fs xs)
--   y = y'
solveWith :: [Op] -> Int -> Int -> ([Int] -> Maybe [Op])
solveWith ops target initial = go [] (Just initial)
  where
    go :: [Op] -> Maybe Int -> [Int] -> Maybe [Op]
    go acc my xs = do
      y <- my
      case xs of
        [] -> if y == target then Just (reverse acc) else Nothing
        (x:xs) -> asum [ go (op:acc) (y `op` x) xs | op <- ops]

solve1 :: Solver
solve1 input = show $ sum [ y | (y, x:xs) <- equations, hasSoln y x xs ]
  where
    equations = mustParse (linesOf equation) input
    hasSoln y x xs = isJust $ solveWith [add, mul] y x xs

solve2 :: Solver
solve2 input = show $ sum [ y | (y, x:xs) <- equations, hasSoln' y x xs ]
  where
    equations = mustParse (linesOf equation) input
    hasSoln y x xs = isJust $ solveWith [add, mul, cat] y x xs
    -- NOTE: Part 2 demonstrates a nifty mathematical optimization (:
    -- The order of x, xs, y is reversed and inversed operators are used.
    -- Solving this "dual input" turns out to yield a decent speedup. This is
    -- mainly because integer division and "un-concatenation" are much stricter
    -- than their non-reversed counterparts.These additional constraints lead
    -- to much more aggressive pruning of the search tree.
    hasSoln' y x xs = isJust $ solveWith [add', mul', cat'] x y (reverse xs)

main :: IO ()
main = runCLI solve1 solve2
