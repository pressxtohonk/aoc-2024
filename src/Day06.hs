module Main where

-- TODO: Compare performance with Data.HashSet vs Data.Set
import Data.Set (Set)
import qualified Data.Set as Set
import qualified PressXToGrids as Grid
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec

-- Domain model
type Move = (Pos, Dir)

type Pos = Pair Int

data Dir = U | D | L | R deriving (Show, Ord, Eq)

turn :: Move -> Move
turn (pos, U) = (pos, R)
turn (pos, R) = (pos, D)
turn (pos, D) = (pos, L)
turn (pos, L) = (pos, U)

step :: Move -> Move
step ((r, c), U) = ((r - 1, c), U)
step ((r, c), D) = ((r + 1, c), D)
step ((r, c), L) = ((r, c - 1), L)
step ((r, c), R) = ((r, c + 1), R)

data Board = Board
  { nrow :: Int,
    ncol :: Int,
    blocked :: Set (Int, Int)
  }
  deriving (Show)

hasCell :: Board -> Pos -> Bool
hasCell (Board nrow ncol _) (r, c)
  | (r < 1) || (nrow < r) = False
  | (c < 1) || (ncol < c) = False
  | otherwise = True

filledAt :: Board -> Pos -> Bool
filledAt (Board _ _ blocked) pos = Set.member pos blocked

emptyAt :: Board -> Pos -> Bool
emptyAt b = not . filledAt b

-- Parsers
posP :: Parser Pos
posP = do
  pos <- getPosition
  let r = sourceLine pos
  let c = sourceColumn pos
  return (r, c)

startP :: Parser Pos
startP = do
  skipTill (char '^')
  (r, c) <- posP
  return (r, c - 1)

obstruction :: Parser Pos
obstruction = do
  skipTill (char '#')
  (r, c) <- posP
  return (r, c - 1)

boardP :: Parser Board
boardP = do
  grid <- lookAhead block
  let nrow = Grid.nrow grid
  let ncol = Grid.ncol grid
  blocked <- Set.fromList <$> many (try obstruction)
  return $ Board nrow ncol blocked

-- given a board and a starting move, return a sequence of moves to exit the board if it exists
data PathError = InvalidState | CycleDetected deriving (Show, Eq)

exitPath :: Board -> Move -> Either PathError [Move]
exitPath board = walkBoard Set.empty []
  where
    walkBoard :: Set Move -> [Move] -> Move -> Either PathError [Move]
    walkBoard moves path move@(pos, _)
      | not (board `hasCell` pos) = Right path -- exited, return the path taken
      | not (board `emptyAt` pos) = Left InvalidState -- should never be *in* a blocked cell
      | Set.member move moves = Left CycleDetected -- repeating moves
      | otherwise = walkBoard (Set.insert move moves) (move : path) (update move)
      where
        (nextPos, _) = step move
        update = if board `filledAt` nextPos then turn else step

hasCycle :: Either PathError a -> Bool
hasCycle res = case res of
  Left CycleDetected -> True
  _ -> False

addObstruction :: Pos -> Board -> Board
addObstruction pos (Board nrow ncol blocked) = Board nrow ncol blocked'
  where
    blocked' = Set.insert pos blocked

distinct :: (Ord a) => [a] -> [a]
distinct = Set.toList . Set.fromList

-- Solutions
solve1 :: Solver
solve1 input = show . length . distinct $ [pos | (pos, _) <- path]
  where
    start :: Pos
    start = mustParse startP input
    board :: Board
    board = mustParse boardP input
    path = case exitPath board (start, U) of
      Left pathError -> error $ show pathError
      Right path -> path

solve2 :: Solver
solve2 input = show (length cyclic)
  where
    start :: Pos
    start = mustParse startP input
    board :: Board
    board = mustParse boardP input
    -- Compute path that guard takes to exit the grid
    path = case exitPath board (start, U) of
      Left pathError -> error $ show pathError
      Right path -> path
    -- Collect positions along the path that creates cycles when obstructed
    createsCycle pos = hasCycle $ exitPath (addObstruction pos board) (start, U)
    cyclic = distinct [pos | (pos, _) <- path, pos /= start, createsCycle pos]

main :: IO ()
main = runCLI solve1 solve2
