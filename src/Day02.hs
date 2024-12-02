module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

input :: Parser [[Int]]
input = linesOf (int `sepBy1` char ' ')

diff :: [Int] -> [Int]
diff [] = error "cannot diff empty list"
diff xs = case xs of
  (a:b:rest) -> (b-a) : diff (b:rest)
  [_] -> []

drop1 :: [Int] -> [[Int]]
drop1 [] = []
drop1 xs@(h:t) = t : [ h:t' | t' <- drop1 t ]

monotonic :: [Int] -> Bool
monotonic xs = all (> 0) xs || all (< 0) xs

gradual :: [Int] -> Bool
gradual xs = all (\x -> abs x `elem` [1..3]) xs

valid1 :: [Int] -> Bool
valid1 xs = let dx = diff xs in monotonic dx && gradual dx

valid2 :: [Int] -> Bool
valid2 xs = valid1 xs || any valid1 (drop1 xs)

solve1 :: Solver
solve1 = show . length . filter valid1 . mustParse input

solve2 :: Solver
solve2 = show . length . filter valid2 . mustParse input

main :: IO ()
main = runCLI solve1 solve2
