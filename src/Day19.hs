module Main where

import Control.Applicative (asum)
import Control.Monad (when)
import Control.Monad.RWS
import Data.List (stripPrefix, tails)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Tree as Tree
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (alphaNum, eof, many1, newline, sepBy, string)

inputP :: Parser ([String], [String])
inputP = do
  patterns <- many1 alphaNum `sepBy` string ", " <* eol
  newline
  designs <- linesOf (many1 alphaNum) <* eof
  return (patterns, designs)

generate :: [String] -> String -> (String, [String])
generate = flip (,)

search :: String -> Tree.Tree String -> Maybe [String]
search word (Tree.Node part rest)
  | null word = Just []
  | otherwise = do
      word' <- stripPrefix part word
      (part :) <$> asum (search word' <$> rest)

type Cached a = RWS () [String] (Map.Map String a) a

countSolns :: String -> Tree.Tree String -> Cached Int
countSolns word (Tree.Node part forest) = case stripPrefix part word of
  Nothing -> return 0
  Just rest -> do
    cache <- get
    when (Map.notMember rest cache) $ do
      n <- if rest == "" then return 1 else sum <$> mapM (countSolns rest) forest
      tell [unwords [show n, "-> cache", show (word, part, rest)]]
      modify (Map.insert rest n)
    n <- gets (! rest)
    tell [unwords [show n, "<- cache", show (word, part, rest)]]
    return n

-- Tracks the number of ways each word can be composed from fragments
type Cache = Map.Map String Int

cache :: Cache
cache = Map.singleton "" 1

-- DP solution with more compact caching than the trie-based soln above
countArrangements :: [String] -> String -> Int
countArrangements parts word =
  let counts = foldr (countCombiOf parts) cache (tails word)
   in Map.findWithDefault 0 word counts

-- Updates a cache for `word` with the number of ways it composes from:
-- a prefix from `parts` followed by a suffix from `cache` keys
countCombiOf :: [String] -> String -> Cache -> Cache
countCombiOf parts word cache
  | Map.notMember word cache && (count > 0) = Map.insert word count cache
  | otherwise = cache
  where
    suffixes = mapMaybe (`stripPrefix` word) parts
    subCounts = mapMaybe (`Map.lookup` cache) suffixes
    count = sum subCounts

solve1 :: Solver
solve1 input = show $ length (mapMaybe (`search` tree) designs)
  where
    (patterns, designs) = mustParse inputP input
    tree = Tree.unfoldTree (generate patterns) ""

solve2 :: Solver
solve2 input = show $ sum counts
  where
    (patterns, designs) = mustParse inputP input
    tree = Tree.unfoldTree (generate patterns) ""
    actions = mapM (`countSolns` tree) designs
    (counts, cache, log) = runRWS actions () Map.empty

solve2' :: Solver
solve2' input = show $ foldr ((+) . countArrangements patterns) 0 designs
  where
    (patterns, designs) = mustParse inputP input

main :: IO ()
main = runCLI solve1 solve2'
