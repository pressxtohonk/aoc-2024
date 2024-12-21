module Main where

import Data.Map ((!))
import qualified Data.Map as Map
import PressXToAlgos (dijkstra)
import PressXToBoard (shortestPath, (?))
import qualified PressXToBoard as Board
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (alphaNum, char, many1)

data NumPad = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA | N_
  deriving (Eq, Ord, Show)

numPadP :: Parser NumPad
numPadP =
  anyOf
    [ N0 <$ char '0',
      N1 <$ char '1',
      N2 <$ char '2',
      N3 <$ char '3',
      N4 <$ char '4',
      N5 <$ char '5',
      N6 <$ char '6',
      N7 <$ char '7',
      N8 <$ char '8',
      N9 <$ char '9',
      NA <$ char 'A'
    ]

data DirPad = DU | DD | DL | DR | DA | D_
  deriving (Eq, Ord, Show)

encode :: DirPad -> Board.Dir
encode DU = Board.U
encode DD = Board.D
encode DL = Board.L
encode DR = Board.R
encode x = error $ unwords [show x, "is not a move"]

decode :: Board.Dir -> DirPad
decode Board.U = DU
decode Board.D = DD
decode Board.L = DL
decode Board.R = DR

type State = (Board.Pos, DirPad) -- (arm coords, dirpad button)

step :: DirPad -> State -> State
step newDir (lastPos, lastDir) = case newDir of
  DA -> (lastPos, DA)
  _ -> (newPos, newDir)
  where
    (newPos, _) = Board.step (lastPos, encode newDir)

type DistMat a = Map.Map (Pair a) Int

-- As a human, each dirpad transition only takes one move
-- Robots move step by step and have non-uniform transitions
distUniform :: (Ord a) => Board.Board a -> DistMat a
distUniform board = Map.fromList $ (,1) <$> ((,) <$> dirs <*> dirs)
  where
    dirs = Map.elems (Board.filled board)

distWith :: (Ord a) => DistMat DirPad -> Board.Board a -> DistMat a
distWith cost board = Map.fromList transitions
  where
    cells = Map.toList (Board.filled board)
    transitions = [((xi, xj), dist i j) | (i, xi) <- cells, (j, xj) <- cells]

    -- the cost of (i -> j) is the cost of the shortest path
    dist :: Board.Pos -> Board.Pos -> Int
    dist i j
      | i == j = cost ! (DA, DA)
      | otherwise = fst . last . head $ shortestPaths cost board i j

shortestPaths :: (Ord a) => DistMat DirPad -> Board.Board a -> Board.Pos -> Board.Pos -> [[(Int, State)]]
shortestPaths cost board i j = dijkstra done next start
  where
    start :: State
    start = (i, DA)

    next :: State -> [(Int, State)]
    next state@(_, lastDir) =
      filter
        (\(_, (pos, _)) -> Board.filledAt board pos)
        [ (cost ! (lastDir, DU), step DU state),
          (cost ! (lastDir, DD), step DD state),
          (cost ! (lastDir, DL), step DL state),
          (cost ! (lastDir, DR), step DR state),
          (cost ! (lastDir, DA), step DA state)
        ]

    done :: State -> Bool
    done = (== (j, DA))

numPad :: Board.Board NumPad
numPad = Board.Board nrow ncol (Map.filter (/= N_) cells)
  where
    Board.Board nrow ncol cells =
      Board.fromLists
        [ [N7, N8, N9],
          [N4, N5, N6],
          [N1, N2, N3],
          [N_, N0, NA]
        ]

dirPad :: Board.Board DirPad
dirPad = Board.Board nrow ncol (Map.filter (/= D_) cells)
  where
    Board.Board nrow ncol cells =
      Board.fromLists
        [ [D_, DU, DA],
          [DL, DD, DR]
        ]

d1 :: DistMat DirPad
d1 = distUniform dirPad -- One directional keypad that you are using

d2 :: DistMat DirPad
d2 = distWith d1 dirPad -- 1/2 directional keypads that robots are using

d3 :: DistMat DirPad
d3 = distWith d2 dirPad -- 2/2 directional keypads that robots are using

d4 :: DistMat NumPad
d4 = distWith d3 numPad -- One numeric keypad (on a door) that a robot is using

minLenWith :: (Ord a) => DistMat a -> a -> [a] -> Int
minLenWith dist start steps =
  sum ((dist !) <$> zip (start : steps) steps)

asNumeric :: [NumPad] -> Int
asNumeric = read . concatMap show'
  where
    show' N0 = "0"
    show' N1 = "1"
    show' N2 = "2"
    show' N3 = "3"
    show' N4 = "4"
    show' N5 = "5"
    show' N6 = "6"
    show' N7 = "7"
    show' N8 = "8"
    show' N9 = "9"
    show' _ = ""

complexity :: [NumPad] -> Int
complexity code = minLenWith d4 NA code * asNumeric code

solve1 :: Solver
solve1 input = show $ sum (complexity <$> codes)
  where
    codes = mustParse (linesOf (many1 numPadP)) input

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2