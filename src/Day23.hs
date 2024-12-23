module Main where

import Data.Foldable (Foldable (foldl'))
import Data.List (isPrefixOf)
import Data.Map ((!?))
import qualified Data.Map as Map
import qualified Data.Set as Set
import PressXToParse
import PressXToSolve (Solver, runCLI)
import Text.Parsec (alphaNum, char, eof, manyTill)

connP :: Parser (String, String)
connP = (,) <$> alphaNum `manyTill` char '-' <*> alphaNum `manyTill` eol

findTrios :: (Ord a) => [(a, a)] -> [(a, a, a)]
findTrios pairs = trios
  where
    conns = foldl' update Map.empty pairs
    update :: (Ord a) => Map.Map a (Set.Set a) -> (a, a) -> Map.Map a (Set.Set a)
    update acc (u, v) = updateU . updateV $ acc
      where
        updateU = Map.insertWith Set.union u (Set.singleton v)
        updateV = Map.insertWith Set.union v (Set.singleton u)

    trios = foldl' connect [] pairs
    connect acc (u, v) = case Set.intersection <$> conns !? u <*> conns !? v of
      Just xs -> [(u, v, x) | x <- Set.toList xs, x /= u, x /= v] ++ acc
      _ -> acc

hasAny :: (a -> Bool) -> (a, a, a) -> Bool
hasAny p (a, b, c) = any p [a, b, c]

solve1 :: Solver
solve1 = show . (`div` 3) . length . filter (hasAny ("t" `isPrefixOf`)) . findTrios . mustParse (connP `manyTill` eof)

solve2 :: Solver
solve2 = show

main :: IO ()
main = runCLI solve1 solve2
