module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified PressXToGrids as Grid
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

-- TODO: This is shared with day 6, move to shared module.
data Board = Board
  { nrow :: Int,
    ncol :: Int,
    filled :: Set (Int, Int)
  }
  deriving (Show)

type Pos = Pair Int

hasCell :: Board -> Pos -> Bool
hasCell (Board nrow ncol _) (r, c)
  | (r < 1) || (nrow < r) = False
  | (c < 1) || (ncol < c) = False
  | otherwise = True

filledAt :: Board -> Pos -> Bool
filledAt board pos = Set.member pos (filled board)

fill :: Board -> Pos -> Board
fill board pos = board {filled = Set.insert pos (filled board)}

-- input parsing
antennaP :: Parser (Int, Int, Char)
antennaP = withCoord alphaNum

antennasP :: Parser (Map Char [Pair Int])
antennasP = do
  coords <- many (try antennaP)
  return $ Map.fromListWith (++) [(k, [(r, c)]) | (r, c, k) <- coords]

boardP :: Parser Board
boardP = do
  grid <- Grid.fromLists <$> block
  let nrow = Grid.nrow grid
  let ncol = Grid.ncol grid
  return $ Board nrow ncol Set.empty

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

allLines :: Board -> [Pos] -> [Pos]
allLines board nodes = do
  i <- nodes
  j <- nodes
  if i == j
    then []
    else takeWhile (board `hasCell`) (lineFrom i j)

antiNodes :: Board -> Map Char [Pos] -> [Pos]
antiNodes board antennas = Map.foldrWithKey (\k nodes acc -> allLines board nodes ++ acc) [] antennas

distinct :: (Ord a) => [a] -> [a]
distinct = Set.toList . Set.fromList

solve1 :: Solver
solve1 input = show $ length [(r, c) | r <- [1 .. nrow], c <- [1 .. ncol], isAntiNode antennas (r, c)]
  where
    grid = Grid.fromLists $ mustParse block input
    nrow = Grid.nrow grid
    ncol = Grid.ncol grid
    board = Board nrow ncol Set.empty
    antennas = mustParse antennasP input

solve2 :: Solver
solve2 input = show . length . distinct $ antiNodes board antennas
  where
    grid = Grid.fromLists $ mustParse block input
    nrow = Grid.nrow grid
    ncol = Grid.ncol grid
    board = Board nrow ncol Set.empty
    antennas = mustParse antennasP input

main :: IO ()
main = runCLI solve1 solve2
