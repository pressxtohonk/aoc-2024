module Main where

import PressXToParse
import PressXToSolve (Solver, runCLI)

solve1 :: Solver
solve1 = show

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
