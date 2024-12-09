module Main where

import Data.Foldable (concatMap)
import Data.Maybe (fromMaybe)
import PressXToParse hiding (block, blocks)
import PressXToSolve (Solver, runCLI)
import Text.Parsec

data Block
  = File {idx :: Int, id :: Int}
  | Space {idx :: Int}
  deriving (Show, Eq)

files :: Parser [Int]
files = many ((\d -> read [d]) <$> digit)

uncompress :: [Int] -> [Block]
uncompress = goFile 0 0
  where
    goFile srcIdx dstIdx [] = []
    goFile srcIdx dstIdx (x : xs) = uncompressFile srcIdx dstIdx x ++ goSpace (srcIdx + 1) (dstIdx + x) xs
    goSpace srcIdx dstIdx [] = []
    goSpace srcIdx dstIdx (x : xs) = uncompressSpace srcIdx dstIdx x ++ goFile srcIdx (dstIdx + x) xs

uncompressFile :: Int -> Int -> Int -> [Block]
uncompressFile srcIdx dstIdx n = [File i srcIdx | i <- [dstIdx .. dstIdx + n - 1]]

uncompressSpace :: Int -> Int -> Int -> [Block]
uncompressSpace srcIdx dstIdx n = [Space i | i <- [dstIdx .. dstIdx + n - 1]]

compact :: [Block] -> [Block]
compact blocks = go blocks [File i n | File i n <- reverse blocks]
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

data Span
  = FileSpan {sidx :: Int, slen :: Int, sid :: Int}
  | SpaceSpan {sidx :: Int, slen :: Int}
  deriving (Show, Eq)

uncompress' :: [Int] -> [Span]
uncompress' = goFile 0 0
  where
    goFile fileId dstIdx [] = []
    goFile fileId dstIdx (x : xs) = FileSpan dstIdx x fileId : goSpace (fileId + 1) (dstIdx + x) xs
    goSpace fileId dstIdx [] = []
    goSpace fileId dstIdx (x : xs) = SpaceSpan dstIdx x : goFile fileId (dstIdx + x) xs

insert :: Span -> [Span] -> Maybe [Span]
insert fs spans = case (fs, spans) of
  (FileSpan _ n _, SpaceSpan i' n' : rest)
    | n < n' -> Just $ fs : SpaceSpan (i' + n) (n' - n) : rest
    | n == n' -> Just $ fs : rest
  (FileSpan _ n _, span : rest) -> (span :) <$> insert fs rest
  _ -> Nothing

trySwapWithSpace :: (Span, [Span]) -> (Span, [Span])
trySwapWithSpace pair = case pair of
  (SpaceSpan {}, _) -> pair
  (fileSpan, spans) -> fromMaybe pair $ do
    spans' <- insert fileSpan spans
    return (SpaceSpan (sidx fileSpan) (slen fileSpan), spans')

compact' :: [Span] -> [Span]
compact' = reverse . go . reverse
  where
    go :: [Span] -> [Span]
    go reversed = case reversed of
      (last : rest) ->
        let (last', rest') = trySwapWithSpace (last, reverse rest)
         in last' : go (reverse rest')
      _ -> []

spanToBlocks :: Span -> [Block]
spanToBlocks span = case span of
  SpaceSpan i n -> [Space i' | i' <- [i .. i + n - 1]]
  FileSpan i n x -> [File i' x | i' <- [i .. i + n - 1]]

checksum :: [Block] -> Int
checksum blocks = sum $ zipWith (*) [0 ..] (fmap value blocks)

value :: Block -> Int
value (Space _) = 0
value (File _ x) = x

solve1 :: Solver
solve1 = show . checksum . compact . uncompress . mustParse files

solve2 :: Solver
solve2 = show . checksum . concatMap spanToBlocks . compact' . uncompress' . mustParse files

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
