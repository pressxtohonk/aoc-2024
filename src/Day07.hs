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
data Tree a = Leaf a | Tree a (Tree a) (Tree a) (Tree a)

-- check if a predicate matches any leaf values
anyLeaf :: (a -> Bool) -> Tree a -> Bool
anyLeaf p (Leaf x)       = p x
anyLeaf p (Tree _ l m r) = any (anyLeaf p) [l, m, r]

-- integer concatenation
(~) :: Int -> Int -> Int
x ~ y = head [n*x+y | n <- offsets, n > y]
  where offsets = 10 : fmap (10 *) offsets

-- note the identical middle and right branches, this is logically a binary tree
solve1 input = show $ sum [y | (y, x:xs) <- equations, anyLeaf (==y) (tree x xs)]
  where
    equations = mustParse (linesOf equation) input
    tree x []     = Leaf x
    tree x (y:ys) = Tree x (tree (x+y) ys) (tree (x*y) ys) (tree (x*y) ys)

-- this soln differs only in structure, the right branch is now for concatenation
solve2 input = show $ sum [y | (y, x:xs) <- equations, anyLeaf (==y) (tree x xs)]
  where
    equations = mustParse (linesOf equation) input
    tree x []     = Leaf x
    tree x (y:ys) = Tree x (tree (x+y) ys) (tree (x*y) ys) (tree (x~y) ys)

main :: IO ()
main = runCLI solve1 solve2
