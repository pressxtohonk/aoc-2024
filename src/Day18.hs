module Main where

import qualified Data.Map as Map
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char)

queueP :: Parser [Pair Int]
queueP = linesOf $ (,) <$> int <* char ',' <*> int

-- NOTE: Binary search would be faster, but our search space only has 3450 elements
firstFatalByte :: Board.Board () -> [Board.Pos] -> Board.Pos
firstFatalByte board [] = error "no fatal bytes"
firstFatalByte board (pos : rest)
  | null soln = pos
  | otherwise = firstFatalByte board' rest
  where
    nrow = Board.nrow board
    ncol = Board.ncol board
    board' = Board.fill board pos ()
    soln = Board.shortestPath board' (0, 0) (nrow - 1, ncol - 1)

solve1 :: Solver
solve1 input = show $ minimum (length <$> paths) - 1 -- count moves, not nodes
  where
    nrow = 71 -- 7 for tests, 71 for solve
    ncol = 71 -- 7 for tests, 71 for solve
    nbits = 1024  -- 12 for tests, 1024 for solve
    queue = mustParse queueP input
    board = Board.Board nrow ncol (Map.fromList $ map (,()) (take nbits queue))
    paths = Board.shortestPath board (0, 0) (nrow - 1, ncol - 1)

solve2 :: Solver
solve2 input = show r ++ "," ++ show c
  where
    nrow = 71 -- 7 for tests, 71 for solve
    ncol = 71 -- 7 for tests, 71 for solve
    board = Board.Board nrow ncol Map.empty
    queue = mustParse queueP input
    (r, c) = firstFatalByte board queue

main :: IO ()
main = runCLI solve1 solve2
