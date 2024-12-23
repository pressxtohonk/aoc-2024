module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import PressXToBoard
import qualified PressXToGrids as Grid
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

type B = Board ()

-- input parsing
antennaP :: Parser (Int, Int, Char)
antennaP = do
  (r, c, x) <- withCoord alphaNum
  return (r - 1, c - 1, x)

antennasP :: Parser (Map Char [Pair Int])
antennasP = do
  coords <- many (try antennaP)
  return $ Map.fromListWith (++) [(k, [(r, c)]) | (r, c, k) <- coords]

boardP :: Parser B
boardP = do
  grid <- Grid.fromLists <$> block
  let nrow = Grid.nrow grid
  let ncol = Grid.ncol grid
  return $ Board nrow ncol Map.empty

extrapolate :: Pos -> Pos -> Pos
extrapolate (r, c) (r', c') = (r' + r' - r, c' + c' - c)

lineFrom :: Pos -> Pos -> [Pos]
lineFrom src dst = src : lineFrom dst (extrapolate src dst)

formsLine :: Pos -> [Pos] -> Bool
formsLine pos nodes = any check nodes
  where
    check node = (pos /= node) && (extrapolate pos node `elem` nodes)

isAntiNode :: Map Char [Pos] -> Pos -> Bool
isAntiNode antennas pos = Map.foldrWithKey (\k nodes acc -> formsLine pos nodes || acc) False antennas

allLines :: B -> [Pos] -> [Pos]
allLines board nodes = do
  i <- nodes
  j <- nodes
  if i == j
    then []
    else takeWhile (board `hasCell`) (lineFrom i j)

antiNodes :: B -> Map Char [Pos] -> [Pos]
antiNodes board = Map.foldrWithKey (\k nodes acc -> allLines board nodes ++ acc) []

distinct :: (Ord a) => [a] -> [a]
distinct = Set.toList . Set.fromList

solve1 :: Solver
solve1 input = show $ length [(r, c) | r <- [0 .. nrow - 1], c <- [0 .. ncol - 1], isAntiNode antennas (r, c)]
  where
    grid = Grid.fromLists $ mustParse block input
    nrow = Grid.nrow grid
    ncol = Grid.ncol grid
    board = Board nrow ncol Map.empty
    antennas = mustParse antennasP input

solve2 :: Solver
solve2 input = show . length . distinct $ antiNodes board antennas
  where
    grid = Grid.fromLists $ mustParse block input
    nrow = Grid.nrow grid
    ncol = Grid.ncol grid
    board = Board nrow ncol Map.empty
    antennas = mustParse antennasP input

main :: IO ()
main = runCLI solve1 solve2
