module Main where

-- TODO: Compare performance with Data.HashSet vs Data.Set
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

-- optimized board traversal until next object or out of bounds
jumpWith :: B -> Move -> Move
jumpWith board = jump
  where
    objs = fst <$> Map.toList (filled board)
    byRow = Map.fromListWith (++) [(r, [c]) | (r, c) <- objs]
    byCol = Map.fromListWith (++) [(c, [r]) | (r, c) <- objs]
    get = Map.findWithDefault []
    m = nrow board -- first out of bounds row
    n = ncol board -- first out of bounds col
    jump (pos, dir) = (jump' dir pos, dir)
    jump' U (r, c) = foldl max (-1, c) [(r' + 1, c) | r' <- get c byCol, r' < r]
    jump' D (r, c) = foldl min (m, c) [(r' - 1, c) | r' <- get c byCol, r < r']
    jump' L (r, c) = foldl max (r, -1) [(r, c' + 1) | c' <- get r byRow, c' < c]
    jump' R (r, c) = foldl min (r, n) [(r, c' - 1) | c' <- get r byRow, c < c']

-- Parsers
startP :: Parser Pos
startP = do
  (r, c) <- coordOf (char '^')
  return (r - 1, c - 1)

filledP :: Parser Pos
filledP = do
  (r, c) <- coordOf (char '#')
  return (r - 1, c - 1)

boardP :: Parser B
boardP = do
  grid <- lookAhead block
  let nrow = Grid.nrow grid
  let ncol = Grid.ncol grid
  filled <- Map.fromList . flip zip (repeat ()) <$> many (try filledP)
  return $ Board nrow ncol filled

-- given a board and a starting move, return a sequence of moves to exit the board if it exists
data PathResult = InvalidState | CycleDetected | Path [Move] deriving (Show, Eq)

exitPath :: B -> Move -> PathResult
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
        update = if board `filledAt` nextPos then turnR else step

exitPath' :: B -> Move -> PathResult
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
        update = if board `filledAt` nextPos then turnR else jump

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
    pathWithout pos = exitPath' (fill board pos ()) (start, U)
    cyclic = distinct [pos | (pos, _) <- path, pathWithout pos == CycleDetected]

main :: IO ()
main = runCLI solve1 solve2
