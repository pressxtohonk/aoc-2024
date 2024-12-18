module Main where

import Control.Monad (guard)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char)

queueP :: Parser [Pair Int]
queueP = linesOf $ (,) <$> int <* char ',' <*> int

type Seed = (Board.Board (), Path)

type Path = [Board.Pos]

type WithSeen = State (Set.Set Board.Pos)

explore :: Board.Board () -> Path -> WithSeen (Path, [Path])
explore board path = case path of
  (pos : _) -> do
    seen <- get
    let peers = filter (`Set.notMember` seen) (Board.peers board pos)
    put (foldr Set.insert seen (pos : peers))
    return (path, [peer : path | peer <- peers])
  _ -> return ([], [])

shortestPathsTo :: Board.Pos -> Tree.Tree Path -> [Path]
shortestPathsTo target tree = go Set.empty [tree]
  where
    go seen forest
      | null forest = []
      | not (null soln) = soln
      | otherwise = go seen' forest'
      where
        paths = Tree.rootLabel <$> forest
        soln = filter ((== target) . head) paths
        seen' = foldr (Set.insert . head) seen paths
        forest' = do
          Tree.Node (h : _) subForest <- forest
          guard (Set.notMember h seen)
          subForest

-- NOTE: Binary search would be faster, but our search space only has 3450 elements
firstFatalByte :: Board.Board () -> [Board.Pos] -> Board.Pos
firstFatalByte board [] = error "no fatal bytes"
firstFatalByte board (pos : rest)
  | null (shortestPathsTo (nrow - 1, ncol - 1) tree) = pos
  | otherwise = firstFatalByte board' rest
  where
    board' = Board.fill board pos ()
    nrow = Board.nrow board
    ncol = Board.ncol board
    tree = evalState (Tree.unfoldTreeM_BF (explore board') [(0, 0)]) Set.empty

solve1 :: Solver
solve1 input = show $ minimum (length <$> shortestPathsTo (nrow - 1, ncol - 1) tree')
  where
    nrow = 71
    ncol = 71
    nbits = 1024
    board = Board.Board nrow ncol (Map.fromList $ map (,()) (take nbits queue))
    queue = mustParse queueP input
    tree' = evalState (Tree.unfoldTreeM_BF (explore board) [(0, 0)]) Set.empty

solve2 :: Solver
solve2 input = show $ firstFatalByte board queue
  where
    nrow = 71
    ncol = 71
    board = Board.Board nrow ncol Map.empty
    queue = mustParse queueP input

main :: IO ()
main = runCLI solve1 solve2
