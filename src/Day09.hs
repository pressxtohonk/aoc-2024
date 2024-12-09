module Main where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Foldable (concatMap)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import PressXToParse hiding (block, blocks)
import PressXToSolve (Solver, runCLI)
import Text.Parsec (anyChar, digit, getPosition, many, sourceColumn)

-- Group of blocks
data Span
  = FileSpan {sidx :: Int, slen :: Int, sid :: Int}
  | SpaceSpan {sidx :: Int, slen :: Int}
  deriving (Show, Eq)

data Block
  = File {idx :: Int, id :: Int}
  | Space {idx :: Int}
  deriving (Show, Eq)

files :: Parser [Int]
files = many ((\d -> read [d]) <$> digit)

uncompress :: [Int] -> [Span]
uncompress = goFile 0 0
  where
    goFile fileId dstIdx [] = []
    goFile fileId dstIdx (x : xs) = FileSpan dstIdx x fileId : goSpace (fileId + 1) (dstIdx + x) xs
    goSpace fileId dstIdx [] = []
    goSpace fileId dstIdx (x : xs) = SpaceSpan dstIdx x : goFile fileId (dstIdx + x) xs

spansToBlocks :: [Span] -> [Block]
spansToBlocks = concatMap spanToBlocks

spanToBlocks :: Span -> [Block]
spanToBlocks span = case span of
  SpaceSpan i n -> [Space i' | i' <- [i .. i + n - 1]]
  FileSpan i n x -> [File i' x | i' <- [i .. i + n - 1]]

compactBlocks :: [Block] -> [Block]
compactBlocks blocks = go blocks [File i n | File i n <- reverse blocks]
  where
    go :: [Block] -> [Block] -> [Block]
    go [] _ = [] -- done compacting
    go xs [] = xs -- no more opportunity to compact
    go (x : xs) (x' : xs')
      | idx x > idx x' = [] -- ascending pointer always has to be behind descending pointer
      | otherwise = case (x, x') of
          (File i _, _) -> x : go xs (x' : xs')
          (Space i, File _ n) -> File i n : go xs xs'
          _ -> error $ "compact received non-file block at reverse pointer: " ++ show (x, x')

-- Starting from the right, push all file spans to the left most slot it can fit into
compactSpans :: [Span] -> [Span]
compactSpans spans = go [] (reverse spans)
  where
    go :: [Span] -> [Span] -> [Span]
    go acc [] = acc
    go acc (last : rest) = case last of
      SpaceSpan {} -> go (last : acc) rest
      FileSpan i n x -> go (last' : acc) rest'
        where
          (last', rest') = maybe (last, rest) (SpaceSpan i n,) (tryInsertR last rest)

-- tries to insert a file span into the right most slot it can fit into
tryInsertR :: Span -> [Span] -> Maybe [Span]
tryInsertR span spans = case spans of
  (x : xs) -> ((x :) <$> tryInsertR span xs) <|> (tryWriteR span x <&> (++ xs))
  _ -> Nothing

-- tries to write a file span into a space span, flushed right
tryWriteR :: Span -> Span -> Maybe [Span]
tryWriteR src dst = case (src, dst) of
  (FileSpan i n x, SpaceSpan i' n')
    | n' > n -> Just [SpaceSpan (i' + n) (n' - n), FileSpan i n x]
    | n' == n -> Just [FileSpan i n x]
  _ -> Nothing

checksum :: [Block] -> Int
checksum blocks = sum $ zipWith (*) [0 ..] (fmap value blocks)

value :: Block -> Int
value (Space _) = 0
value (File _ x) = x

solve1 :: Solver
solve1 = show . checksum . compactBlocks . spansToBlocks . uncompress . mustParse files

solve2 :: Solver
solve2 = show . checksum . spansToBlocks . compactSpans . uncompress . mustParse files

main :: IO ()
main = runCLI solve1 solve2'

-- debugging
block :: Parser Block
block = do
  pos <- getPosition
  let i = sourceColumn pos
  c <- anyChar
  return $
    if c == '.'
      then Space (i - 1)
      else File (i - 1) (read [c])

display :: Block -> String
display (File _ n) = show n
display (Space _) = "."

display' :: Span -> String
display' (FileSpan _ n x) = concatMap (replicate n) (show x)
display' (SpaceSpan _ n) = replicate n '.'

-- Optimized part 2 soln
type Range = (Int, Int) -- index, size

-- builds a list of sets, where i-th set contains space ranges with size i+1
initCache :: [Span] -> [Set Range]
initCache spans =
  [ Set.fromList [(i, n) | SpaceSpan i n <- spans, n == 1],
    Set.fromList [(i, n) | SpaceSpan i n <- spans, n == 2],
    Set.fromList [(i, n) | SpaceSpan i n <- spans, n == 3],
    Set.fromList [(i, n) | SpaceSpan i n <- spans, n == 4],
    Set.fromList [(i, n) | SpaceSpan i n <- spans, n == 5],
    Set.fromList [(i, n) | SpaceSpan i n <- spans, n == 6],
    Set.fromList [(i, n) | SpaceSpan i n <- spans, n == 7],
    Set.fromList [(i, n) | SpaceSpan i n <- spans, n == 8],
    Set.fromList [(i, n) | SpaceSpan i n <- spans, n >= 9]
  ]

-- Returns the lowest range in the set that can contain the given range
query :: Range -> Set Range -> Maybe Range
query (i, n) spans
  | null spans = Nothing
  | otherwise =
      let (i', n') = minimum spans
       in if (i' <= i) && (n <= n')
            then Just (i', n')
            else Nothing

-- Returns the lowest range among all the set that can contain the given range
queryAll :: Range -> [Set Range] -> Maybe Range
queryAll (i, n) gaps = case mapMaybe (query (i, n)) gaps of
  [] -> Nothing
  xs -> Just (minimum xs)

-- Returns a copy of the set list where set `i` has been modified by `f`
updateSet :: Int -> (Set a -> Set a) -> [Set a] -> [Set a]
updateSet i f gaps =
  case splitAt i gaps of
    (head, spans : tail) -> head ++ f spans : tail
    _ -> error $ "could not update gaps at idx " ++ show i

-- For a file range, return a state action that yield its position when flushed left.
-- The state containing sets of space ranges created by `initCache` is updated accordingly.
flushLeft :: Range -> State [Set Range] Range
flushLeft (i, n) = do
  gaps <- get
  case queryAll (i, n) gaps of
    Nothing -> return (i, n)
    Just (i', n')
      | n < n' -> modify (doInsert . doDelete) >> return (i', n)
      | n == n' -> modify doDelete >> return (i', n)
      | otherwise -> error "update failed as queryAll returned gap with insufficient size"
      where
        doDelete = updateSet (n' - 1) (Set.delete (i', n'))
        doInsert = updateSet (n' - n - 1) (Set.insert (i' + n, n' - n))

-- Runs a sequence of `flushLeft` actions for each file range with initial state `spacesCache`
compactSpans' :: [Span] -> [Span]
compactSpans' spans = evalState updateFiles spacesCache
  where
    spacesCache = initCache spans
    updateFiles = sequenceA [spanWithId k <$> flushLeft (i, n) | FileSpan i n k <- reverse spans]
    spanWithId k (i, n) = FileSpan i n k

checksum' :: [Span] -> Int
checksum' spans = sum [k * sum [i .. i + n - 1] | FileSpan i n k <- spans]

solve2' :: Solver
solve2' = show . checksum' . compactSpans' . uncompress . mustParse files
