module Main where

-- TODO: Compare performance with Data.HashSet vs Data.Set
import Data.Map (Map)
import qualified Data.Map as Map
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
    filled :: Set (Int, Int)
  }
  deriving (Show)

hasCell :: Board -> Pos -> Bool
hasCell (Board nrow ncol _) (r, c)
  | (r < 1) || (nrow < r) = False
  | (c < 1) || (ncol < c) = False
  | otherwise = True

filledAt :: Board -> Pos -> Bool
filledAt board pos = Set.member pos (filled board)

fill :: Board -> Pos -> Board
fill board pos = board { filled = Set.insert pos (filled board) }

-- optimized board traversal until next object or out of bounds
jumpWith :: Board -> Move -> Move
jumpWith board = jump
  where
    objs = Set.toList (filled board)
    byRow = Map.fromListWith (++) [(r, [c]) | (r, c) <- objs]
    byCol = Map.fromListWith (++) [(c, [r]) | (r, c) <- objs]
    get = Map.findWithDefault []
    m = nrow board + 1 -- first out of bounds row
    n = ncol board + 1 -- first out of bounds col
    jump (pos, dir) = (jump' dir pos, dir)
    jump' U (r, c) = foldl max (0, c) [(r'+1, c) | r' <- get c byCol, r' < r]
    jump' D (r, c) = foldl min (m, c) [(r'-1, c) | r' <- get c byCol, r < r']
    jump' L (r, c) = foldl max (r, 0) [(r, c'+1) | c' <- get r byRow, c' < c]
    jump' R (r, c) = foldl min (r, n) [(r, c'-1) | c' <- get r byRow, c < c']

-- Parsers
startP :: Parser Pos
startP = coordOf (char '^')

filledP :: Parser Pos
filledP = coordOf (char '#')

boardP :: Parser Board
boardP = do
  grid <- lookAhead block
  let nrow = Grid.nrow grid
  let ncol = Grid.ncol grid
  filled <- Set.fromList <$> many (try filledP)
  return $ Board nrow ncol filled

-- given a board and a starting move, return a sequence of moves to exit the board if it exists
data PathResult = InvalidState | CycleDetected | Path [Move] deriving (Show, Eq)

exitPath :: Board -> Move -> PathResult
exitPath board = walkBoard Set.empty []
  where
    walkBoard :: Set Move -> [Move] -> Move -> PathResult
    walkBoard moves path move@(pos, _)
      | not (board `hasCell` pos) = Path path -- exited, return the path taken
      | board `filledAt` pos = InvalidState -- should never be *in* a blocked cell
      | Set.member move moves = CycleDetected -- repeating moves
      | otherwise = walkBoard (Set.insert move moves) (move : path) (update move)
      where
        (nextPos, _) = step move
        update = if board `filledAt` nextPos then turn else step

exitPath' :: Board -> Move -> PathResult
exitPath' board = walkBoard Set.empty []
  where
    walkBoard :: Set Move -> [Move] -> Move -> PathResult
    walkBoard moves path move@(pos, _)
      | not (board `hasCell` pos) = Path path -- exited, return the path taken
      | board `filledAt` pos = InvalidState -- should never be *in* a blocked cell
      | Set.member move moves = CycleDetected -- repeating moves
      | otherwise = walkBoard (Set.insert move moves) (move : path) (update move)
      where
        jump = jumpWith board
        (nextPos, _) = step move
        update = if board `filledAt` nextPos then turn else jump

unwrapPath :: PathResult -> [Move]
unwrapPath res = case res of
    Path path -> path
    pathError -> error $ show pathError

distinct :: (Ord a) => [a] -> [a]
distinct = Set.toList . Set.fromList

-- Solutions
solve1 :: Solver
solve1 input = show . length . distinct $ [pos | (pos, _) <- path]
  where
    start = mustParse startP input
    board = mustParse boardP input
    path = unwrapPath $ exitPath board (start, U)

solve2 :: Solver
solve2 input = show (length cyclic)
  where
    -- Compute path that guard takes to exit the grid
    start = mustParse startP input
    board = mustParse boardP input
    path = unwrapPath $ exitPath board (start, U)
    -- Collect positions along the path that creates cycles when obstructed
    pathWithout pos = exitPath' (board `fill` pos) (start, U)
    cyclic = distinct [pos | (pos, _) <- path, pathWithout pos == CycleDetected]

main :: IO ()
main = runCLI solve1 solve2
