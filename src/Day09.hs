module Main where

import Control.Applicative ((<|>))
import Data.Foldable (concatMap)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import PressXToParse hiding (block, blocks)
import PressXToSolve (Solver, runCLI)
import Text.Parsec hiding ((<|>))

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
main = runCLI solve1 solve2

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
