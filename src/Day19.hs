module Main where

import Control.Applicative (asum)
import Data.List (stripPrefix)
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

solve1 :: Solver
solve1 input = show $ length (mapMaybe (`search` tree) designs)
  where
    (patterns, designs) = mustParse inputP input
    tree = Tree.unfoldTree (generate patterns) ""

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
