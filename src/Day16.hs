module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import PressXToBoard (Board, (?))
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (char, manyTill)
import Data.Maybe (fromJust)

data Tile
  = Start
  | Floor
  | Wall
  | End
  deriving (Show, Eq)

tile :: Parser Tile
tile = anyOf
  [ Start <$ char 'S'
  , Floor <$ char '.'
  , Wall <$ char '#'
  , End <$ char 'E'
  ]

maze :: Parser (Board Tile)
maze = Board.fromLists <$> ((tile `manyTill` eol) `manyTill` eol)

type Branch = (Int, [Board.Move])

extend :: (Int -> Int) -> (Board.Move -> Board.Move) -> Branch -> Branch
extend _ _ (cost, []) = error "cannot extend empty branch"
extend f g (cost, xs) = (f cost, g (head xs) : xs)

step :: Branch -> Branch
step = extend (+1) Board.step

spin :: Branch -> Branch
spin = extend (+2001) (Board.step . Board.turnR . Board.turnR)

turnL :: Branch -> Branch
turnL = extend (+1001) (Board.step . Board.turnL)

turnR :: Branch -> Branch
turnR = extend (+1001) (Board.step . Board.turnR)

-- Optimized to never walk over the same tile twice
shortestPath :: Board Tile -> Board.Move -> Maybe Int
shortestPath board start = go board branches
  where
    board' = Board.fill board (Board.pos start) Wall
    branches = Set.fromList $ map ($ (0, [start])) [step, turnL, turnR, spin]
    go :: Board Tile -> Set Branch -> Maybe Int
    go board paths = do
      branch@(cost, move@(pos, _):rest) <- Set.lookupMin paths
      let
        board' = Board.fill board pos Wall
        paths' = Set.deleteMin paths
        updates =
          [ Set.insert (step branch)
          , Set.insert (turnL branch)
          , Set.insert (turnR branch)
          ]
      tile <- board ? pos
      case tile of
        End -> Just cost
        Floor -> go board' (foldr ($) paths' updates)
        _ -> go board' paths'

escapeCost :: Board Tile -> Maybe Int
escapeCost board = shortestPath board (startPos, east)
  where
    startPos = head . Map.keys $ Map.filter (==Start) (Board.filled board)
    east = Board.R

solve1 :: Solver
solve1 = show . fromJust . escapeCost . mustParse maze

solve2 :: Solver
solve2 = show . mustParse maze

main :: IO ()
main = runCLI solve1 solve2
