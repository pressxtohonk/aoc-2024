module Main where

import qualified Data.Map as Map
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char)

queueP :: Parser [Pair Int]
queueP = linesOf $ (,) <$> int <* char ',' <*> int

firstFatalByte :: Board.Board () -> [Board.Pos] -> Board.Pos
firstFatalByte board queue = case binarySearch noSoln 0 (length queue) of
  Nothing -> error "no fatal bytes"
  Just (_, i) -> queue !! (i - 1)
  where
    noSoln nbits = null soln
      where
        nrow = Board.nrow board
        ncol = Board.ncol board
        board' = Board.Board nrow ncol (Map.fromList $ map (,()) (take nbits queue))
        soln = Board.shortestPath board' (0, 0) (nrow - 1, ncol - 1)

-- Find the first (i, i+1) in [lb .. ub] such that f <$> [i, i+1] = [False, True]
binarySearch :: (Int -> Bool) -> Int -> Int -> Maybe (Int, Int)
binarySearch f lb ub
  | lb > ub = Nothing
  | ub - lb == 1 = Just (lb, ub)
  | otherwise =
      let mid = (lb + ub) `div` 2
       in if f mid
            then binarySearch f lb mid
            else binarySearch f mid ub

solve1 :: Solver
solve1 input = show $ minimum (length <$> paths) - 1 -- count moves, not nodes
  where
    nrow = 71 -- 7 for tests, 71 for solve
    ncol = 71 -- 7 for tests, 71 for solve
    nbits = 1024 -- 12 for tests, 1024 for solve
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
