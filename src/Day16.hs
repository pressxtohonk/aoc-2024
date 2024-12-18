module Main where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import PressXToBoard (Board, (?))
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, manyTill)

data Tile
  = Start
  | Floor
  | Wall
  | End
  deriving (Show, Eq)

tile :: Parser Tile
tile =
  anyOf
    [ Start <$ char 'S',
      Floor <$ char '.',
      Wall <$ char '#',
      End <$ char 'E'
    ]

maze :: Parser (Board Tile)
maze = Board.fromLists <$> ((tile `manyTill` eol) `manyTill` eol)

type Branch = (Int, Board.Move, Board.Move)

extend :: (Int -> Int) -> (Board.Move -> Board.Move) -> Branch -> Branch
extend f g (cost, _, move) = (f cost, move, g move)

step :: Branch -> Branch
step = extend (+ 1) Board.step

spin :: Branch -> Branch
spin = extend (+ 2000) (Board.turnR . Board.turnR)

turnL :: Branch -> Branch
turnL = extend (+ 1000) Board.turnL

turnR :: Branch -> Branch
turnR = extend (+ 1000) Board.turnR

type PrevMovesByCost = Map Board.Move [(Int, Board.Move)]

-- TODO: tidy this up! it's horrendous,,,
escape :: Board Tile -> Maybe PrevMovesByCost
escape board = go board (Set.singleton tmpBranch) Map.empty
  where
    startPos = head . Map.keys $ Map.filter (== Start) (Board.filled board)
    startDir = Board.R
    startMove = (startPos, startDir)
    board' = Board.fill board startPos Wall
    tmpBranch = (0, startMove, startMove)
    branches = Set.fromList (map ($ tmpBranch) [step, turnL, turnR, spin])
    go :: Board Tile -> Set (Int, Board.Move, Board.Move) -> PrevMovesByCost -> Maybe PrevMovesByCost
    go board branches acc
      | null branches = Just acc
      | otherwise = do
          branch@(cost, prev, move@(pos, dir)) <- Set.lookupMin branches
          let acc' = Map.insertWith (++) move [(cost, prev)] acc
              board' = Board.fill board pos Wall
              branches' = Set.deleteMin branches
              updates = [Set.insert (f branch) | f <- [step, turnL, turnR, spin]]
          tile <- board ? pos
          if Map.member move acc
            then go board branches' acc'
            else case tile of
              End -> go board branches' acc'
              Wall -> go board branches' acc
              _ -> go board (foldr ($) branches' updates) acc'

-- TODO: tidy this up! it's horrendous,,,
getPaths :: Board Tile -> PrevMovesByCost -> [[Board.Move]]
getPaths board soln = concatMap (go [[]]) [move | (c, move) <- endSolns, c == minCost]
  where
    endPos = head . Map.keys $ Map.filter (== End) (Board.filled board)
    endMoves = (endPos,) <$> [Board.U, Board.D, Board.L, Board.R]
    endSolns = concat $ mapMaybe (($ soln) . Map.lookup) endMoves
    minCost = minimum (fst <$> endSolns)

    go :: [[Board.Move]] -> Board.Move -> [[Board.Move]]
    go acc move@(pos, dir) = case (board ? pos, Map.lookup move soln) of
      (Just Wall, _) -> error $ "unexpected wall in solution @ " ++ show pos
      (Just Start, _) -> map (move :) acc
      (Just _, Just prevs) -> concatMap (go acc') nexts
        where
          acc' = map (move :) acc
          minCost = minimum (fst <$> prevs)
          nexts = [move | (c, move) <- prevs, c == minCost]
      (Nothing, _) -> error $ "out of bounds @ " ++ show pos
      (_, Nothing) -> []

getCosts :: Board Tile -> PrevMovesByCost -> [Int]
getCosts board soln = fst <$> endSolns
  where
    endPos = head . Map.keys $ Map.filter (== End) (Board.filled board)
    endMoves = (endPos,) <$> [Board.U, Board.D, Board.L, Board.R]
    endSolns = concat $ mapMaybe (($ soln) . Map.lookup) endMoves

costs :: Board Tile -> [Int]
costs board = getCosts board (fromJust (escape board))

paths :: Board Tile -> [[Board.Move]]
paths board = getPaths board (fromJust (escape board))

solve1 :: Solver
solve1 = show . minimum . costs . mustParse maze

-- TODO: tidy this up! it's horrendous,,,
solve2 :: Solver
solve2 = show . (+ 1) . length . nub . map fst . concat . paths . mustParse maze

main :: IO ()
main = runCLI solve1 solve2
